open Format
open X86_64
open Ast

let debug = ref false

let string_table : (string, string) Hashtbl.t = Hashtbl.create 64
let function_table : (string, fn) Hashtbl.t = Hashtbl.create 16
let function_return_type_table : (string, v_type) Hashtbl.t = Hashtbl.create 16
let is_list_contain_string : (string, bool) Hashtbl.t = Hashtbl.create 16

let label_counter = ref 0
let current_var_name = ref None

let debug_string_table () =
  Format.printf "Current string_table contents:@.";
  Hashtbl.iter (fun key value ->
    Format.printf "  String: \"%s\", Label: %s@." key value
  ) string_table

(* 替代原有的 fresh_label 函数 *)

let fresh_unique_label () =
  let label = Printf.sprintf ".LC%d" !label_counter in
  incr label_counter;
  label
  
let rec string_of_expr expr =
  match expr with
  | TEcst (Cstring s) -> Printf.sprintf "TEcst(Cstring \"%s\")" s
  | TEcst (Cint n) -> Printf.sprintf "TEcst(Cint %Ld)" n
  | TEcst (Cbool b) -> Printf.sprintf "TEcst(Cbool %b)" b
  | TEvar var -> Printf.sprintf "TEvar(name: %s, type: %s)" var.v_name 
      (match var.v_type with
        | Tstring -> "Tstring"
        | Tint -> "Tint"
        | Tbool -> "Tbool"
        | Tnone -> "Tnone")
  | TEcall (fn, args) ->
      let args_str = String.concat ", " (List.map string_of_expr args) in
      Printf.sprintf "TEcall(fn: %s, args: [%s])" fn.fn_name args_str
  | TEbinop (op, left, right) ->
      let op_str = match op with
        | Badd -> "+"
        | Bsub -> "-"
        | Bmul -> "*"
        | Bdiv -> "/"
        | Bmod -> "%"
        | Beq -> "=="
        | Bneq -> "!="
        | Blt -> "<"
        | Ble -> "<="
        | Bgt -> ">"
        | Bge -> ">="
        | Band -> "and"
        | Bor -> "or"
      in
      Printf.sprintf "TEbinop(%s, %s, %s)" op_str (string_of_expr left) (string_of_expr right)
  | TEunop (op, sub_expr) ->
      let op_str = match op with
        | Uneg -> "-"
        | Unot -> "not"
      in
      Printf.sprintf "TEunop(%s, %s)" op_str (string_of_expr sub_expr)
  | TElist elements ->
      let elements_str = String.concat ", " (List.map string_of_expr elements) in
      Printf.sprintf "TElist([%s])" elements_str
  | TEget (lst, idx) ->
      Printf.sprintf "TEget(lst: %s, idx: %s)" (string_of_expr lst) (string_of_expr idx)
  | _ -> "Unknown expr"
  
(* 打印类型化表达式 *)
let rec print_texpr fmt expr =
  match expr with
  | TEcst c -> Format.fprintf fmt "TEcst(%s)" (match c with
      | Cstring s -> Printf.sprintf "\"%s\"" s
      | Cint i -> Int64.to_string i
      | Cbool b -> string_of_bool b
      | Cnone -> "None")
  | TEvar var -> Format.fprintf fmt "TEvar(%s)" var.v_name
  | TEbinop (op, e1, e2) ->
      Format.fprintf fmt "TEbinop(%s, %a, %a)"
        (match op with
        | Badd -> "+"
        | Bsub -> "-"
        | Bmul -> "*"
        | Bdiv -> "//"
        | Bmod -> "%"
        | Beq -> "=="
        | Bneq -> "!="
        | Blt -> "<"
        | Ble -> "<="
        | Bgt -> ">"
        | Bge -> ">="
        | Band -> "and"
        | Bor -> "or")
        print_texpr e1 print_texpr e2
  | _ -> Format.fprintf fmt "Unsupported texpr"

(* 打印类型化语句 *)
let rec print_tstmt fmt stmt =
  match stmt with
  | TSprint expr ->
      Format.fprintf fmt "TSprint(%a)" print_texpr expr
  | TSassign (var, expr) ->
      Format.fprintf fmt "TSassign(%s, %a)" var.v_name print_texpr expr
  | TSblock stmts ->
      Format.fprintf fmt "TSblock([%a])" (Format.pp_print_list print_tstmt) stmts
  | _ -> Format.fprintf fmt "Unsupported tstmt"

let rec evaluate_boolean_expr expr =
  match expr with
  | TEcst (Cbool b) -> Some b
  | TEbinop (op, left, right) -> (
      let eval_int expr =
        match expr with
        | TEcst (Cint n) -> Some (Int64.to_int n)
        | _ -> None
      in
      let left_int = eval_int left in
      let right_int = eval_int right in
      match op, left_int, right_int with
      | Blt, Some l, Some r -> Some (l < r)
      | Ble, Some l, Some r -> Some (l <= r)
      | Bgt, Some l, Some r -> Some (l > r)
      | Bge, Some l, Some r -> Some (l >= r)
      | Beq, Some l, Some r -> Some (l = r)
      | Bneq, Some l, Some r -> Some (l <> r)
      | _ -> None (* 無法靜態求值 *)
    )
  | TEunop (Unot, sub_expr) -> (
      match evaluate_boolean_expr sub_expr with
      | Some b -> Some (not b)
      | None -> None
    )
  | _ -> None (* 其他類型無法靜態求值 *)
  
  
(* 生成表达式的汇编代码 *)
let rec generate_expr ?(is_for=false) expr =
  match expr with
  | TEcst (Cint value) ->
      let code = movq (imm (Int64.to_int value)) (!%rax) in
      (nop, code)
  | TEcst (Cstring s) ->
    let was_already_defined = Hashtbl.mem string_table s in
    let label_name = 
      if was_already_defined then
        Hashtbl.find string_table s
      else
        let new_label = fresh_unique_label () in
        Hashtbl.add string_table s new_label;
        (* debug_string_table (); *)
        new_label 
    in
    let data = 
      if was_already_defined then
        nop
      else
        label label_name ++ string s
    in
    let code = movq (ilab label_name) (!%rdi) ++ movq (!%rdi) (!%rax) in
    (data, code)
  | TEcst (Cbool b) ->
    let code = movq (imm (if b then 1 else 0)) (!%rax) in
    (nop, code)
  | TEvar var ->
    let load_var =
      if var.v_type = Tstring then
        movq (ind ~ofs:var.v_ofs rbp) (!%rdi)
      else if var.v_type = Tint then
        movq (ind ~ofs:var.v_ofs rbp) (!%rax)
      else (* 假设变量是一个列表 *)
        movq (!%r15) (!%r12)
    in
    let add_for = 
      if is_for = true then
        movq (!%r9) (!%rax)
      else
        nop
    in
    (nop, load_var ++ add_for )
  | TEunop (Unot, expr) ->
      let data, code = generate_expr expr in
      let op_code =
        let true_label = fresh_unique_label () in
        let end_label = fresh_unique_label () in
        cmpq (imm 0) (!%rax) ++
        jne true_label ++
        movq (imm 1) (!%rax) ++
        jmp end_label ++
        label true_label ++
        movq (imm 0) (!%rax) ++
        label end_label
      in
      (data, code ++ op_code)
  | TEunop (Uneg, expr) ->
      let data, code = generate_expr expr in
      let op_code = negq (!%rax) in
      (data, code ++ op_code)
  | TEcall (fn, [arg]) when fn.fn_name = "len" ->
    let arg_data, arg_code = generate_expr arg in
    let len_code =
      match arg with
      | TEcst (Cstring _) ->
          arg_code ++
          movq (!%rax) (!%rsi) ++  (* 將字符串地址傳遞給 strlen *)
          call "strlen"        (* 調用 C 的 strlen 函數 *)
      | TEcst (Cint _) ->
          arg_code ++
          movq (imm 1) (!%rax)    (* 整數長度固定為 1 *)
      | _ ->
          failwith "len() only supports string or integer types"
    in
    (arg_data, len_code)
  | TEcall ({ fn_name = "range"; _ }, [start_expr; end_expr]) ->
      let start_data, start_code = generate_expr start_expr in
      let end_data, end_code = generate_expr end_expr in
      let range_code =
        start_code ++
        movq (!%rax) (!%r12) ++ (* 保存 start 到 r12 *)
        end_code ++
        movq (!%rax) (!%r13) ++ (* 保存 end 到 r13 *)
        subq (!%r12) (!%r13) ++ (* 計算範圍長度 *)
        movq (!%r13) (!%rdi) ++
        call "malloc" ++
        movq (!%rax) (!%r14) ++ (* 保存範圍地址到 r14 *)
        xorq (!%r15) (!%r15) ++ (* 初始化索引 r15 *)
        label "range_loop" ++
        cmpq (!%r15) (!%r13) ++
        je "range_end" ++
        movq (!%r15) (!%rdx) ++
        imulq (imm 8) (!%rdx) ++
        addq (imm 8) (!%rdx) ++
        movq (!%r15) (ind ~index:rdx ~scale:1 r14) ++
        addq (imm 1) (!%r15) ++
        jmp "range_loop" ++
        label "range_end"
      in
      (start_data ++ end_data, range_code)  
  | TEcall (fn, args) ->
    (* 收集 args 的 code，依序 push，最後 call fn.fn_name *)
    let (data_args, code_args) =
      List.fold_left (fun (acc_data, acc_code) arg ->
        let d, c = generate_expr arg in
        (* 參數算好 -> 存在 %rax -> pushq %rax *)
        (acc_data ++ d, acc_code ++ c ++ pushq (!%rax))
      ) (nop, nop) (List.rev args)
      (* 因為 x86-64 cdecl 參數是由右至左壓入，所以我們 List.rev 才能從最後一個參數先 push *)
    in
    let call_code = call fn.fn_name in
    (* 呼叫完要把參數彈走 *)
    let cleanup_code =
      if List.length args > 0 then
        addq (imm (8 * List.length args)) (!%rsp)
      else
        nop
    in
    (data_args, code_args ++ call_code ++ cleanup_code)  
  | TEbinop (op, left, right) ->
    let left_data, left_code = generate_expr left in
    let right_data, right_code = generate_expr right in
    let op_code =
      match op with
      | Badd ->
        (match left, right with
          | TEcst (Cstring s1), TEcst (Cstring s2) ->
            let left_data, left_code = generate_expr left in
            let right_data, right_code = generate_expr right in
            let concat_code =
              left_code ++
              call "strlen" ++
              movq (!%rax) (!%r10) ++ (* 保存第一個字符串長度 *)
              right_code ++
              call "strlen" ++
              addq (!%r10) (!%rax) ++ (* 加上第二個字符串長度 *)
              addq (imm 1) (!%rax) ++ (* 加終止符空間 *)
              movq (!%rax) (!%rdi) ++
              call "malloc" ++
              movq (!%rax) (!%r12) ++ (* 保存結果地址 *)
              left_code ++
              movq (!%rax) (!%rsi) ++
              movq (!%r12) (!%rdi) ++
              call "strcpy" ++
              right_code ++
              movq (!%rax) (!%rsi) ++
              movq (!%r12) (!%rdi) ++
              call "strcat"
            in
          (concat_code)
          | TEcst (Cstring _), TEcall (fn, args)
          | TEcall (fn, args), TEcst (Cstring _) ->
              (* 分別生成字符串和函數的代碼 *)
              let (string_code, function_code) =
                match left, right with
                | TEcst (Cstring _), TEcall (fn, args) ->
                    let _, string_code = generate_expr left in
                    let _, function_code = generate_expr (TEcall (fn, args)) in
                    (string_code, function_code)
                | TEcall (fn, args), TEcst (Cstring _) ->
                    let _, function_code = generate_expr (TEcall (fn, args)) in
                    let _, string_code = generate_expr right in
                    (string_code, function_code)
                | _ -> failwith "Unexpected case for TEcst and TEcall"
              in
          
              let concat_code =
                string_code ++
                pushq (!%rbx) ++
                call "strlen" ++
                movq (!%rax) (!%r10) ++ (* 保存第一個字符串長度 *)
                popq (rbx) ++
                movq (!%rbx) (!%rax) ++
                pushq (!%rbx) ++
                (* function_code ++ *)
                call "strlen" ++
                addq (!%r10) (!%rax) ++ (* 加上第二個字符串長度 *)
                addq (imm 1) (!%rax) ++ (* 加終止符空間 *)
                movq (!%rax) (!%rdi) ++
                call "malloc" ++
                movq (!%rax) (!%r12) ++ (* 保存結果地址 *)
                string_code ++
                movq (!%rax) (!%rsi) ++
                movq (!%r12) (!%rdi) ++
                call "strcpy" ++
                (* function_code ++ *)
                popq (rbx) ++
                movq (!%rbx) (!%rax) ++
                movq (!%rax) (!%rsi) ++
                movq (!%r12) (!%rdi) ++
                call "strcat"
              in
              (concat_code)
          | TEcst( Cint _ ), TEcst( Cstring _ ) ->
            let code = 
                call "runtime_error" ++
                movq (imm 0) (!%rax) ++
                leave ++
                ret 
            in
            (code)
            | TEcst( Cstring _ ), TEcst( Cint _ ) ->
              let code = 
                  call "runtime_error" ++
                  movq (imm 0) (!%rax) ++
                  leave ++
                  ret 
              in
              (code)
          | _ ->
            addq (!%rbx) (!%rax) )
      | Bsub -> subq (!%rbx) (!%rax)
      | Bmul -> imulq (!%rbx) (!%rax)
      | Bdiv -> cqto ++ idivq (!%rbx)
      | Bmod -> cqto ++ idivq (!%rbx) ++ movq (!%rdx) (!%rax)
      | Beq | Bneq | Blt | Ble | Bgt | Bge ->
          (match left, right with
            | TElist lefts, TElist rights ->
                let compare_label = fresh_unique_label () in
                let end_label = fresh_unique_label () in
                let code =
                  (* 调用 compare_lists 函数 *)
                  left_code ++ 
                  movq (!%rax) (!%rdi) ++ 
                  right_code ++ 
                  movq (!%rax) (!%rsi) ++
                  call "compare_lists" ++ 

                  (* 根据 compare_lists 的返回值设置 %rax *)
                  cmpq (imm 0) (!%rax) ++
                  (match op with
                  | Beq -> je compare_label
                  | Bneq -> jne compare_label
                  | Blt -> jl compare_label
                  | Ble -> jle compare_label
                  | Bgt -> jg compare_label
                  | Bge -> jge compare_label
                  | _ -> nop) ++
                  movq (imm 0) (!%rax) ++
                  jmp end_label ++
                  label compare_label ++
                  movq (imm 1) (!%rax) ++
                  label end_label
                in
                 code
            | TEcst (Cstring _), TEcst (Cstring _) ->
                let compare_label = fresh_unique_label () in
                let end_label = fresh_unique_label () in
                let strcmp_code =
                  left_code ++
                  movq (!%rdi) (!%rcx) ++  (* 将左字符串的地址存入 %rdi *)
                  right_code ++
                  movq (!%rdi) (!%rsi) ++  (* 将右字符串的地址存入 %rsi *)
                  movq (!%rcx) (!%rdi) ++
                  call "strcmp" ++
                  cmpq (imm 0) (!%rax) ++  (* 根据 strcmp 的返回值判断 *)
                  (match op with
                  | Beq -> je compare_label
                  | Bneq -> jne compare_label
                  | Blt -> jg compare_label
                  | Ble -> jle compare_label
                  | Bgt -> jl compare_label
                  | Bge -> jge compare_label
                  | _ -> nop) ++
                  movq (imm 0) (!%rax) ++
                  jmp end_label ++
                  label compare_label ++
                  movq (imm 1) (!%rax) ++
                  label end_label
                in
                strcmp_code
            | _ ->  (* 其他类型的比较 *)
                let set_rax_to_1_label = fresh_unique_label () in
                let end_label = fresh_unique_label () in
                cmpq (!%rbx) (!%rax) ++
                (match op with
                | Beq -> je set_rax_to_1_label
                | Bneq -> jne set_rax_to_1_label
                | Blt -> jl set_rax_to_1_label
                | Ble -> jle set_rax_to_1_label
                | Bgt -> jg set_rax_to_1_label
                | Bge -> jge set_rax_to_1_label
                | _ -> nop) ++
                movq (imm 0) (!%rax) ++
                jmp end_label ++
                label set_rax_to_1_label ++
                movq (imm 1) (!%rax) ++
                label end_label)
      | Band -> (
        match left, right with
        | _ -> (
          let is_invalid_type expr =
            match expr with
            | TEcall (fn , _) -> false (* 函數調用需額外檢查 *)
            | TEcst (Cbool _) -> true (* 布林值是合法類型 *)
            | TEcst (Cint _) -> true  (* 整數在此處不合法 *)
            | TEbinop ((Blt | Ble | Bgt | Bge | Beq | Bneq), left_sub, right_sub) -> true
            | _ -> false (* 其他類型都不合法 *)
          in
          let check expr =
            match expr with
            | TEcall (fn, args) ->
                (* Format.printf "Detected TEcall: %s\n" fn.fn_name; *)
                List.iter (fun arg -> Format.printf "Argument: %a\n" print_texpr arg) args;
                (* 後續處理邏輯 *)
                true
            | _ ->        
                (* Format.printf "Current expr: %s\n" (string_of_expr expr); *)
                true
            in
          let left_invalid = is_invalid_type left in
          let right_invalid = is_invalid_type right in
          (* let c = check right in *)
          let code = 
            call "runtime_error" ++
            movq (imm 0) (!%rax) ++
            leave ++
            ret 
          in
          match left_invalid, right_invalid with
            | false, false -> code 
            | false, true -> (
              let right_result = evaluate_boolean_expr right in
              match right_result with
              | Some false -> andq (!%rbx) (!%rax)
              | Some true -> code
              | _ -> code
              )
            | true, false -> (
              let left_result = evaluate_boolean_expr left in
              match left_result with
              | Some false -> andq (!%rbx) (!%r12)
              | Some true -> code
              | _ -> code
              )
            | _ -> andq (!%rbx) (!%rax) )
          )
      | Bor -> (
        match left, right with
        | _ -> (
          let is_invalid_type expr =
            match expr with
            | TEcall (fn , _) -> false (* 函數調用需額外檢查 *)
            | TEcst (Cbool _) -> true (* 布林值是合法類型 *)
            | TEcst (Cint _) -> true  (* 整數在此處不合法 *)
            | TEbinop ((Blt | Ble | Bgt | Bge | Beq | Bneq), left_sub, right_sub) -> true
            | _ -> false (* 其他類型都不合法 *)
          in
          let check expr =
            match expr with
            | TEcall (fn, args) ->
                (* Format.printf "Detected TEcall: %s\n" fn.fn_name; *)
                List.iter (fun arg -> Format.printf "Argument: %a\n" print_texpr arg) args;
                (* 後續處理邏輯 *)
                true
            | _ ->        
                (* Format.printf "Current expr: %s\n" (string_of_expr expr); *)
                true
            in
          let left_invalid = is_invalid_type left in
          let right_invalid = is_invalid_type right in
          (* let c = check right in *)
          let code = 
            call "runtime_error" ++
            movq (imm 0) (!%rax) ++
            leave ++
            ret 
          in
          match left_invalid, right_invalid with
            | false, false -> code 
            | false, true -> (
              let right_result = evaluate_boolean_expr right in
              match right_result with
              | Some true -> 
                orq (!%rbx) (!%rax)
              | Some false -> code
              | _ -> code
              )
            | true, false -> (
              let left_result = evaluate_boolean_expr left in
              match left_result with
              | Some true -> 
                orq (!%rbx) (!%rax)
              | Some false -> code
              | _ -> code
              )
            | _ -> 
              orq (!%rbx) (!%rax) )
          )
      | _ -> failwith "Unsupported operator"
    in
    (left_data ++ right_data, right_code ++ pushq (!%rax) ++ left_code ++ popq rbx ++ op_code)  
  | TElist elements ->
    let contains_string = List.exists (function
      | TEcst (Cstring _) -> true
      | _ -> false
    ) elements in

    (* 更新 is_list_contain_string 表格 *)
    (match !current_var_name with
     | Some name -> Hashtbl.add is_list_contain_string name contains_string
     | None -> ());
    let total_size = (List.length elements + 1) * 8 in
    let alloc_code =
      movq (imm total_size) (!%rdi) ++ (* 分配清單大小 *)
      call "malloc@PLT" ++
      movq (!%rax) (!%r12) (* 保存清單基址 *)
    in
    let init_length = movq (imm (List.length elements)) (ind r12) in
    let init_elements =
      List.mapi (fun idx elem ->
        let elem_data, elem_code = generate_expr elem in
        elem_code ++
        movq (!%rax) (ind ~ofs:(8 * (idx + 1)) r12) (* 初始化清單元素 *)
      ) elements
      |> List.fold_left (++) nop
    in
    (nop, alloc_code ++ init_length ++ init_elements ++ movq (!%r12) (!%rax))
  | TEget (lst, idx) ->
    let lst_data, lst_code = generate_expr lst in
    let idx_data, idx_code = generate_expr idx in
    let get_code =
      (* 計算索引的偏移量 *)
      idx_code ++
      movq (!%rax) (!%rdx) ++
      imulq (imm 8) (!%rdx) ++
      addq (imm 8) (!%rdx) ++ (* 加 8 跳過 list 長度 *)
      
      (* 取得列表的起始地址 *)
      lst_code ++
      movq (!%rax) (!%rsi) ++

      (* 加載列表元素的值 *)
      leaq (ind r12 ~index:rdx ~scale:1) (rsi) ++ 
      addq (!%rdx) (!%rax) ++
      movq (ind (rsi)) (!%rax)
    in
    (lst_data ++ idx_data, get_code)
  | _ -> failwith "Unsupported expression"

(* 生成语句的汇编代码 *)
let rec generate_stmt ?(is_for=false) stmt =
  match stmt with
  | TSprint expr ->
    (* Format.printf "Current expr: %s\n" (string_of_expr expr); *)
    let data_expr, code_expr = generate_expr ~is_for:is_for expr in
    let rec print_code expr depth =
      match expr with
      | TElist elements ->
        let start_code = movq (ilab ".LCstart") (!%rdi) ++ call "printf" in
        let finalize_code =
          movq (ilab ".LCend") (!%rdi) ++ call "printf" ++
          (if depth = 0 then movq (imm 10) (!%rdi) ++ call "putchar" else nop) in
        let elements_code =
          List.mapi (fun idx elem ->
            match elem with
            | TElist _ ->
                let elem_data, elem_code = print_code elem (depth + 1) in
                let comma =
                  if idx < List.length elements - 1 then
                    movq (ilab ".LCcomma") (!%rdi) ++ call "printf"
                  else nop
                in
                (elem_data, elem_code ++ comma)
            | _ ->
                (* 普通元素處理 *)
                let elem_data, elem_code = generate_expr elem in
                let separator =
                  if idx < List.length elements - 1 then
                    movq (ilab ".LCcomma") (!%rdi) ++ call "printf"
                  else nop
                in
                (elem_data, elem_code ++
                 movq (!%rax) (!%rsi) ++
                 movq (ilab ".LCd") (!%rdi) ++
                 call "printf" ++
                 separator)
          ) elements
          |> List.split
        in
        let data_code = List.fold_left (++) nop (fst elements_code) in
        let code =
          start_code ++ List.fold_left (++) nop (snd elements_code) ++ finalize_code
        in
        (data_code, code)
      | TEvar { v_type = Tnone; v_ofs } ->  (* 如果是變數，且變數類型為 list *)
          let load_code = movq (!%r15) (!%r12) in
          let start_code = movq (ilab ".LCstart") (!%rdi) ++ call "printf" in
          let print_loop =
            call "print_list" 
          in          
          (nop, load_code ++ start_code ++ print_loop )
      | _ ->  (* 其他情況，當作一般變數處理 *)
          let is_boolean_comparison =
            match expr with
            | TEbinop ((Blt | Ble | Bgt | Bge | Beq | Bneq | Band | Bor) as op, left, right) -> true
            | TEunop (Unot, _) -> true
            | TEcst (Cbool _) -> true
            | _ -> false
          in
          let rec involves_string expr =
            match expr with
            | TEcall (fn, _) -> (
                match Hashtbl.find_opt function_return_type_table fn.fn_name with
                | Some Tstring -> true
                | _ -> false
              )
            | TEcst (Cstring _) -> true
            | TEvar var when var.v_type = Tstring -> true
            | TEbinop (_, left, right) -> involves_string left || involves_string right
            | TEunop (_, sub_expr) -> involves_string sub_expr
            | TElist elements -> List.exists involves_string elements
            | TEget (lst, idx) -> involves_string lst || involves_string idx
            | _ -> false
          in 
          let format_label =
            if is_boolean_comparison then ".LCs"
            else if involves_string expr then ".LCs"
            else ".LCd"
          in
          let bool_conversion_code =
            if is_boolean_comparison then
              let true_label = fresh_unique_label () in
              let end_label = fresh_unique_label () in
              cmpq (imm 1) (!%rax) ++
              jne true_label ++
              movq (ilab ".LCtrue") (!%rdi) ++
              jmp end_label ++
              label true_label ++
              movq (ilab ".LCfalse") (!%rdi) ++
              label end_label
            else nop
          in
          let format_data =
            if Hashtbl.mem string_table format_label then
              nop
            else
              let fmt = match format_label with
                | ".LCs" -> "%s"
                | ".LCd" -> "%d"
                | _ -> "%d\n"
              in
              (* Hashtbl.add string_table format_label fmt; *)
              label format_label ++ string fmt
          in
          let load_format_string = match expr with
            | TEcst (Cstring _) -> movq (ilab format_label) (!%rdx)
            | TEvar var when var.v_type = Tstring -> movq (ilab format_label) (!%rdx)
            | TEbinop ((Blt | Ble | Bgt | Bge | Beq | Bneq | Band | Bor) as op, left, right) -> movq (ilab format_label) (!%rdx)
            | TEcst (Cbool _) -> movq (ilab format_label) (!%rdx)
            | TEunop (Unot, _) -> movq (ilab format_label) (!%rdx)
            | _ -> movq (ilab format_label) (!%rdi)
          in
          let print_code_2 =
            code_expr ++
            bool_conversion_code ++
            movq (!%rax) (!%rsi) ++
            load_format_string ++
            movq (imm 0) (!%rax) ++
            call "printf" ++
            movq (imm 10) (!%rdi) ++  (* 10 是 '\n' 的 ASCII 值 *)
            call "putchar"
          in
          (data_expr ++ format_data, print_code_2)
    in
    (print_code expr 0)
  | TSassign (var, expr) ->
      let data, code = generate_expr expr in
      let assign_code = 
        if var.v_type = Tnone then
          movq (!%rax) (!%r15) 
        else 
          movq (!%rax) (ind ~ofs:var.v_ofs rbp)
        in
      (data, code ++ assign_code)
  | TSblock stmts ->
    let datas, codes =
      stmts
      |> List.map (fun stmt -> generate_stmt ~is_for:is_for stmt)
      |> List.split
    in
      let data = List.fold_left (++) nop datas in
      let code = List.fold_left (++) nop codes in
      (data, code)
  | TSif (cond, then_branch, else_branch) ->
    let cond_data, cond_code = generate_expr cond in
    let then_data, then_code = generate_stmt then_branch in
    let else_data, else_code = generate_stmt else_branch in
    let else_label = fresh_unique_label () in
    let end_label = fresh_unique_label () in
    (cond_data ++ then_data ++ else_data,
      cond_code ++
      cmpq (imm 0) (!%rax) ++
      je else_label ++
      then_code ++
      jmp end_label ++
      label else_label ++
      else_code ++
      label end_label)
  | TSdef (fn, body) ->
    (nop, nop)
  
  | TSreturn expr ->
      let d_expr, c_expr = generate_expr expr in
      (d_expr, c_expr ++ leave ++ ret)
  | TSfor (var, iterable, body) ->
      (* 檢查 iterable 是否為不合法的整數類型 *)
      let is_invalid_iterable =
        match iterable with
        | TEcst (Cint _) -> true  (* 直接使用整數作為迭代對象是不合法的 *)
        | _ -> false
      in
  
      if is_invalid_iterable then
        (* 若迭代器不合法，生成錯誤代碼 *)
        let error_code = 
          call "runtime_error" ++
          movq (imm 0) (!%rax) ++
          leave ++
          ret 
        in
        (nop, error_code)
      else
        let iter_data, iter_code = generate_expr iterable in
        let loop_start = fresh_unique_label () in
        let loop_end = fresh_unique_label () in
        let body_data, body_code = generate_stmt ~is_for:true body in

        let copy_list_code =
          movq (!%rax) (!%r12) ++
          pushq (!%r12) ++
          movq (ind r12) (!%r13) ++
          imulq (imm 8) (!%r13) ++
          addq (imm 8) (!%r13) ++
          movq (!%r13) (!%rdi) ++
          call "malloc@PLT" ++
          movq (!%rax) (!%r14) ++
          movq (ind r12) (!%rax) ++
          movq (!%rax) (ind r14) ++
          xorq (!%r15) (!%r15) ++
          label "copy_list_loop" ++
          cmpq (ind r12) (!%r15) ++
          je "copy_list_end" ++
          movq (!%r15) (!%rdx) ++
          imulq (imm 8) (!%rdx) ++
          addq (imm 8) (!%rdx) ++
          movq (ind ~index:rdx ~scale:1 r12) (!%rax) ++
          movq (!%rax) (ind ~index:rdx ~scale:1 r14) ++
          addq (imm 1) (!%r15) ++
          jmp "copy_list_loop" ++
          label "copy_list_end" ++
          popq (r12)
        in

        let loop_code =
          iter_code ++
          copy_list_code ++
          xorq (!%r13) (!%r13) ++
          label loop_start ++
          cmpq (ind r14) (!%r13) ++
          je loop_end ++
          movq (!%r13) (!%rdx) ++
          imulq (imm 8) (!%rdx) ++
          addq (imm 8) (!%rdx) ++
          movq (ind ~index:rdx ~scale:1 r14) (!%rax) ++
          movq (!%rax) (ind ~ofs:var.v_ofs rbp) ++
          movq (!%rax) (!%r9) ++
          body_code ++
          addq (imm 1) (!%r13) ++
          jmp loop_start ++
          label loop_end
        in
        let save_global_var =
          movq (ind ~ofs:var.v_ofs rbp) (!%r10)  (* 保存全域變量值到 r10 *)
        in
        (* 在循環結束後，將最後一個值更新到全域變量 *)
        let update_global_var =
          movq (!%r13) (!%rdx) ++ (* 使用計數器值 r13 計算偏移量 *)
          subq (imm 1) (!%rdx) ++ (* 計算最後一個元素索引 *)
          imulq (imm 8) (!%rdx) ++
          addq (imm 8) (!%rdx) ++
          movq (ind ~index:rdx ~scale:1 r12) (!%rax) ++ (* 加載清單的最後一個元素到 rax *)
          movq (!%rax) (ind ~ofs:var.v_ofs rbp)  (* 更新全域變量地址 *)
        in
        (iter_data ++ body_data, save_global_var ++ loop_code  ++ update_global_var)    
  | TSeval expr -> 
      (* 新增對 TSeval 的支援 *)
      let data_expr, code_expr = generate_expr expr in
      (data_expr, code_expr)
  | _ ->
    if !debug then Format.printf "Unsupported statement: %a@." print_tstmt stmt;
    failwith "Unsupported statement" 


let rec extract_return_type stmt =
  match stmt with
  | TSreturn expr -> Some (extract_expr_type expr)
  | TSblock stmts -> (
      (* 遍歷所有子語句，提取第一個有效的返回類型 *)
      List.fold_left (fun acc sub_stmt ->
        match acc with
        | Some _ -> acc  (* 已經找到返回類型，直接返回 *)
        | None -> extract_return_type sub_stmt
      ) None stmts
    )
  | TSif (_, then_stmt, else_stmt) -> (
      match extract_return_type then_stmt, extract_return_type else_stmt with
      | Some t1, Some t2 when t1 = t2 -> Some t1  (* then 和 else 返回相同類型 *)
      | Some t, None | None, Some t -> Some t    (* 只有一個分支有返回類型 *)
      | _ -> None                               (* 返回類型不一致或均無返回類型 *)
    )
  | _ -> None  (* 其他情況無返回類型 *)

(* 提取表達式的類型 *)
and extract_expr_type expr =
  match expr with
  | TEcst (Cstring _) -> Tstring
  | TEcst (Cint _) -> Tint
  | TEcst (Cbool _) -> Tbool
  | TEvar var -> var.v_type
  | TEbinop (_, left, right) -> (
      (* 假設二元操作返回與操作數類型一致，這裡取左操作數的類型 *)
      extract_expr_type left
    )
  | TEunop (_, sub_expr) -> extract_expr_type sub_expr
  | TEcall (fn, _) -> (
      (* 查詢函數的返回類型 *)
      match Hashtbl.find_opt function_return_type_table fn.fn_name with
      | Some t -> t
      | None -> Tnone  (* 默認為無類型 *)
    )
  | _ -> Tnone  (* 默認無類型 *)
    
let generate_def (fn, body) =
  (* 收集函數內所有字符串常量 *)
  let rec collect_strings stmt =
    match stmt with
    | TSreturn (TEcst (Cstring s)) -> 
      Hashtbl.replace function_return_type_table fn.fn_name Tstring;
      [s]
    | TSblock stmts -> List.flatten (List.map collect_strings stmts)
    | _ -> []
  in
  let strings = collect_strings body in

  (* 生成字符串的 .data 段 *)
  let string_data =
    List.fold_left (fun acc s ->
      let te_cst = TEcst (Cstring s) in
      let data, _ = generate_expr te_cst in
      acc ++ data
    ) nop strings
  in

  (* 確保字符串已正確加入 string_table *)
  List.iter (fun s ->
    let was_already_defined = Hashtbl.mem string_table s in
    if not was_already_defined then
      let label_name = fresh_unique_label () in
      Hashtbl.add string_table s label_name
      (* debug_string_table () *)
  ) strings;
  
  let return_type = extract_return_type body in
  (match return_type with
  | Some t -> Hashtbl.add function_return_type_table fn.fn_name t
  | None -> ());

  (* 生成函數的代碼 *)
  let data_body, code_body = generate_stmt body in
  let function_code =
    label fn.fn_name ++
    pushq (!%rbp) ++
    movq (!%rsp) (!%rbp) ++
    code_body ++
    movq (imm 0) (!%rax) ++
    leave ++
    ret
  in

  (* 返回整體的 .data 和 .text 段 *)
  (string_data ++ data_body, function_code)

let runtime_error =
  label "runtime_error" ++
  movq (ilab ".LCerror") (!%rdi) ++ (* 錯誤訊息的標籤 *)
  call "puts" ++                   (* 輸出錯誤訊息 *)
  movq (imm 1) (!%rdi) ++          (* 退出代碼 1 *)
  call "exit" ++                   (* 終止程式 *)
  ret
  
    
(* 初始化字符串表 *)
let initialize () =
  Hashtbl.add string_table ".LCtrue" "True";
  Hashtbl.add string_table ".LCfalse" "False";
  Hashtbl.add string_table ".LCs" "%s";
  Hashtbl.add string_table ".LCd" "%d";
  Hashtbl.add string_table ".LCcomma" ", ";
  Hashtbl.add string_table ".LCstart" "[";
  Hashtbl.add string_table ".LCend" "]";
  Hashtbl.add function_table "len" {
    fn_name = "len";
    fn_params = [{ v_name = "arg"; v_ofs = 16; v_type = Tstring }];
  }
  (* debug_string_table () *)

(* 生成主函数代码 *)
let file ?debug:(b=false) (tfile: Ast.tfile) : X86_64.program =
  debug := b;
  
  (* tfile 的第一個元素通常是 (main_fn, TSblock [...]) *)
  let (main_fn, main_stmt) = List.hd tfile in
  let other_defs = List.tl tfile in
  
  (* 先生成 main 區塊 *)
  let (data_functions, functions_code) =
    List.fold_left (fun (acc_data, acc_text) (fn, body) ->
      let data, text = generate_def (fn, body) in
      (acc_data ++ data, acc_text ++ text)
    ) (nop, nop) other_defs
  in
  let data_main, code_main = generate_stmt main_stmt in
  
  (* 生成其他自訂函式 *)
    
    (* 你原本放在 data 的字串，這裡順便示範 *)
  let string_data =
    label ".LCtrue"   ++ string "True"   ++
    label ".LCfalse"  ++ string "False"  ++
    label ".LCcomma"  ++ string ", "     ++
    label ".LCstart"  ++ string "["      ++
    label ".LCend"    ++ string "]"      ++
    label ".LCs"      ++ string "%s"     ++
    label ".LCd"      ++ string "%d" ++
    label ".LCerror" ++ string "Runtime Error"
  in

  
  let runtime_error =
    label "runtime_error" ++
    movq (ilab ".LCerror") (!%rdi) ++ (* 錯誤訊息的標籤 *)
    call "puts" ++                   (* 輸出錯誤訊息 *)
    movq (imm 1) (!%rdi) ++          (* 退出代碼 1 *)
    call "exit" ++                   (* 終止程式 *)
    ret
  in
  let string_data2 =
    Hashtbl.fold (fun value label_name acc ->
      acc ++ (label label_name) ++ string value
      ) string_table nop
    in
  initialize ();
  (* 額外定義的 print_list 函式或其它 C extern, 視需求 *)
  let print_list =
    label "print_list" ++
    pushq (!%r12) ++
    pushq (!%r14) ++
    pushq (!%r15) ++
    movq (ind r12) (!%r15) ++
    xorq (!%r14) (!%r14) ++
    label "print_list_loop" ++
    cmpq (!%r14) (!%r15) ++
    je "print_list_end" ++
    movq (!%r14) (!%rdx) ++
    imulq (imm 8) (!%rdx) ++
    addq (imm 8) (!%rdx) ++
    leaq (ind ~index:rdx ~scale:1 r12) rsi ++
    movq (ind rsi) (!%rax) ++
    movq (!%rax) (!%rsi) ++
    movq (ilab ".LCd") (!%rdi) ++
    call "printf" ++
    addq (imm 1) (!%r14) ++
    cmpq (!%r14) (!%r15) ++
    je "print_list_end" ++
    movq (ilab ".LCcomma") (!%rdi) ++
    call "printf" ++
    jmp "print_list_loop" ++
    label "print_list_end" ++
    movq (ilab ".LCend") (!%rdi) ++
    call "printf" ++
    movq (imm 10) (!%rdi) ++
    call "putchar" ++
    movq (imm 0) (!%rax) ++
    popq (r15) ++
    popq (r14) ++
    popq (r12) ++
    ret
  in

  let text =
    functions_code ++                (* 所有自訂函式 *)
    globl "main" ++
    label "main" ++
    pushq (!%rbp) ++
    movq (!%rsp) (!%rbp) ++
    code_main ++
    movq (imm 0) (!%rax) ++
    leave ++
    ret ++
    print_list ++
    runtime_error
  in

  { text; data = string_data ++ string_data2}