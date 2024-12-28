open Format
open X86_64
open Ast

let debug = ref false

let string_table : (string, string) Hashtbl.t = Hashtbl.create 64
let function_table : (string, fn) Hashtbl.t = Hashtbl.create 16

let label_counter = ref 0

(* 替代原有的 fresh_label 函数 *)
let fresh_unique_label () =
  let label = Printf.sprintf ".LC%d" !label_counter in
  incr label_counter;
  label

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

(* 生成表达式的汇编代码 *)
let rec generate_expr expr =
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
        new_label 
    in
    let data = 
      if was_already_defined then
        nop
      else
        label label_name ++ string s
    in
    let code = movq (ilab label_name) (!%rdi) in
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
    (nop, load_var)
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
  | TEbinop (op, left, right) ->
    let left_data, left_code = generate_expr left in
    let right_data, right_code = generate_expr right in
    let op_code =
      match op with
      | Badd ->
        (match left, right with
          | TEcst (Cstring _), TEcst (Cstring _) ->
              let concat_code =
                (* 调用 strlen 获取两个字符串长度 *)
                left_code ++
                call "strlen" ++
                movq (!%rax) (!%r10) ++ (* 保存左字符串长度到 r10 *)
                right_code ++
                call "strlen" ++
                addq (!%r10) (!%rax) ++ (* 计算总长度 *)
                addq (imm 1) (!%rax) ++ (* 加上 '\0' 终止符 *)
                movq (!%rax) (!%rdx) ++
                call "malloc" ++      (* 分配内存 *)
                movq (!%rax) (!%r12) ++ (* 保存新字符串地址 *)
                left_code ++
                movq (!%rdi) (!%rsi) ++
                movq (!%r12) (!%rdi) ++
                call "strcpy" ++      (* 拷贝左字符串 *)
                right_code ++
                movq (!%rdi) (!%rsi) ++
                movq (!%r12) (!%rdi) ++
                call "strcat"         (* 拼接右字符串 *)
              in
              concat_code
          | _ -> addq (!%rbx) (!%rax) )
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
      | Band -> andq (!%rbx) (!%rax)
      | Bor -> orq (!%rbx) (!%rax)
      | _ -> failwith "Unsupported operator"
    in
    (left_data ++ right_data, right_code ++ pushq (!%rax) ++ left_code ++ popq rbx ++ op_code)  
  | TElist elements ->
      let total_size = (List.length elements + 1) * 8 in
      (* 为列表分配内存 *)
      let alloc_code =
        movq (imm total_size) (!%rdi) ++
        call "malloc@PLT" ++
        movq (!%rax) (!%r12) (* 保存分配的内存地址到 r12 *)
      in
      (* 初始化列表内容 *)
      let init_code, data =
        let rec store_elements idx acc_code acc_data = function
          | [] -> (acc_code, acc_data)
          | hd :: tl ->
              let hd_data, hd_code = generate_expr hd in
              let updated_code =
                acc_code ++ hd_code ++
                movq (!%rax) (ind ~ofs:(idx * 8) r12)
              in
              store_elements (idx + 1) updated_code (acc_data ++ hd_data) tl
        in
        store_elements 1 nop nop elements
      in
      let size_code = movq (imm (List.length elements)) (ind r12) in
      (data, alloc_code ++ size_code ++ init_code ++ movq (!%r12) (!%rax))
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
let rec generate_stmt stmt =
  match stmt with
  | TSprint expr ->
    let data_expr, code_expr = generate_expr expr in
    let print_code =
      match expr with
      | TElist elements ->
          let start_code = movq (ilab ".LCstart") (!%rdi) ++ call "printf" in
          let print_element idx expr =
            let elem_data, elem_code = generate_expr expr in
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
          in
          let elements_data, elements_code =
            List.mapi print_element elements
            |> List.split
          in
          let finalize =
            movq (ilab ".LCend") (!%rdi) ++
            call "printf"  ++
            movq (imm 10) (!%rdi) ++  (* 10 是 '\n' 的 ASCII 值 *)
            call "putchar"
          in
          (List.fold_left (++) nop elements_data,
           start_code ++ List.fold_left (++) nop elements_code ++ finalize)
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
          let involves_string =
            match expr with
            | TEcst (Cstring _) -> true
            | TEbinop (Badd, TEcst (Cstring _), TEcst (Cstring _)) -> true
            | TEbinop (Badd, _, TEcst (Cstring _)) -> true
            | TEbinop (Badd, TEcst (Cstring _), _) -> true
            | _ -> false
          in
          let format_label =
            if is_boolean_comparison then ".LCs"
            else if involves_string then ".LCs"
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
                | ".LCs" -> "%s\n"
                | ".LCd" -> "%d\n"
                | _ -> "%d\n"
              in
              Hashtbl.add string_table format_label fmt;
              label format_label ++ string fmt
          in
          let load_format_string = match expr with
            | TEcst (Cstring _) -> movq (ilab format_label) (!%rdx)
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
    (print_code)
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
      let datas, codes = List.split (List.map generate_stmt stmts) in
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
(* | TSwhile (cond, body) ->
    let start_label = fresh_unique_label () in
    let end_label = fresh_unique_label () in
    let cond_data, cond_code = generate_expr cond in
    let body_data, body_code = generate_stmt body in
    (cond_data ++ body_data,
      label start_label ++
      cond_code ++
      cmpq (imm 0) (!%rax) ++
      je end_label ++
      body_code ++
      jmp start_label ++
      label end_label) *)
  | _ ->
    if !debug then Format.printf "Unsupported statement: %a@." print_tstmt stmt;
    failwith "Unsupported statement" 

(* 初始化字符串表 *)
let initialize () =
  Hashtbl.add string_table ".LCtrue" "True";
  Hashtbl.add string_table ".LCfalse" "False";
  Hashtbl.add string_table ".LCs" "%s";
  Hashtbl.add string_table ".LCd" "%d";
  Hashtbl.add string_table ".LCcomma" ", ";
  Hashtbl.add string_table ".LCstart" "[";
  Hashtbl.add string_table ".LCend" "]\n";
  Hashtbl.add function_table "len" {
    fn_name = "len";
    fn_params = [{ v_name = "arg"; v_ofs = 16; v_type = Tstring }];
  }

(* 生成主函数代码 *)
let file ?debug:(b=false) (tfile: Ast.tfile) : X86_64.program =
  debug := b;

  (* 保證初始化字符串表 *)
  initialize ();  (* 在這裡調用 initialize 確保初始化執行 *)

  (* 從 tfile 中提取主體代碼 *)
  let data, main_code =
    let _, stmt = List.hd tfile in
    generate_stmt stmt
  in

  (* 確保 .LCtrue, .LCfalse, .LCs 等標籤被正確定義在數據段 *)
  let string_data =
    label ".LCtrue" ++ string "True" ++
    label ".LCfalse" ++ string "False" ++
    label ".LCs" ++ string "%s" ++
    label ".LCd" ++ string "%d" ++
    label ".LCcomma" ++ string ", " ++
    label ".LCstart" ++ string "[" ++
    label ".LCend" ++ string "]" 
  in
  let print_list =
    label "print_list" ++
    pushq (!%r12) ++
    pushq (!%r14) ++
    pushq (!%r15) ++
    movq (ind r12) (!%r15) ++  (* 加載清單長度到 rcx *)
    xorq (!%r14) (!%r14) ++    (* 初始化索引 rdx = 0 *)
    label "print_list_loop" ++
    cmpq (!%r14) (!%r15) ++    (* 檢查索引是否到達清單尾端 *)
    je "print_list_end" ++
    movq (!%r14) (!%rdx) ++
    (* 計算清單當前元素的地址 *)
    imulq (imm 8) (!%rdx) ++        (* rdx = rdx * 8 *)
    addq (imm 8) (!%rdx) ++         (* rdx = rdx + 8，跳過長度字段 *)
    leaq (ind ~ofs:0 r12) (rsi) ++
    addq (!%rdx) (!%rsi) ++         (* rsi = rsi + rdx (完整地址) *)
    movq (ind rsi) (!%rax) ++       (* 加載當前元素的值 *)
    movq (!%rax) (!%rsi) ++         (* 輸出值到 rsi *)
    movq (ilab ".LCd") (!%rdi) ++ call "printf" ++
    addq (imm 1) (!%r14) ++         (* rdx = rdx + 1 *)
    cmpq (!%r14) (!%r15) ++    (* 檢查索引是否到達清單尾端 *)
    je "print_list_end" ++
    movq (ilab ".LCcomma") (!%rdi) ++ call "printf" ++
    jmp "print_list_loop" ++
    label "print_list_end" ++
    movq (ilab ".LCend") (!%rdi) ++
    call "printf"  ++
    movq (imm 10) (!%rdi) ++  (* 10 是 '\n' 的 ASCII 值 *)
    call "putchar" ++
    movq (imm 0) (!%rax) ++
    popq (r15) ++
    popq (r14) ++
    popq (r12) ++
    ret
  in
  let compare_lists =
    label "compare_lists" ++
    pushq (!%rbp) ++
    movq (!%rsp) (!%rbp) ++
    movq (ind ~ofs:16 rbp) (!%rdi) ++
    movq (ind ~ofs:24 rbp) (!%rsi) ++
    movq (ind rdi) (!%rax) ++
    movq (ind rsi) (!%rbx) ++
    cmpq (!%rax) (!%rbx) ++
    jl "list1_shorter" ++
    jg "list2_shorter" ++
    movq (imm 8) (!%rcx) ++
    jmp "compare_loop" ++
    label "list1_shorter" ++
    movq (imm (-1)) (!%rax) ++
    jmp "end_compare" ++
    label "list2_shorter" ++
    movq (imm 1) (!%rax) ++
    jmp "end_compare" ++
    label "compare_loop" ++
    cmpq (!%rcx) (!%rax) ++
    jge "end_compare" ++
    movq (ind rdi ~index:rcx ~scale:8) (!%rdx) ++
    movq (ind rsi ~index:rcx ~scale:8) (!%r8) ++
    cmpq (!%rdx) (!%r8) ++
    jl "list1_smaller" ++
    jg "list2_smaller" ++
    addq (imm 8) (!%rcx) ++
    jmp "compare_loop" ++
    label "list1_smaller" ++
    movq (imm (-1)) (!%rax) ++
    jmp "end_compare" ++
    label "list2_smaller" ++
    movq (imm 1) (!%rax) ++
    jmp "end_compare" ++
    label "end_compare" ++
    leave ++
    ret
  in

  (* 生成 main 函數的 text 部分 *)
  let text =
    globl "main" ++
    label "main" ++
    pushq (!%rbp) ++
    movq (!%rsp) (!%rbp) ++
    andq (imm (-16)) (!%rsp) ++
    main_code ++
    movq (imm 0) (!%rax) ++
    leave ++
    ret
  in

  (* 返回包含 data 和 text 的結果，並將 string_data 添加到 data 中 *)
  { text = text ++ print_list ++ compare_lists; data = data ++ string_data }