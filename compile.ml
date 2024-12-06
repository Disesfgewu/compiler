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
      let label_name = ".LC" ^ string_of_int (Hashtbl.hash s) in
      if not (Hashtbl.mem string_table label_name) then
        Hashtbl.add string_table label_name s;
      let data = label label_name ++ string (s) in
      let code = movq (ilab label_name) (!%rdi) in
      (data, code)
      | TEcst (Cbool b) ->
        let code = movq (imm (if b then 1 else 0)) (!%rax) in
        (nop, code)
  | TEvar var ->
    let code =
      if var.v_type = Tstring then
        movq (ind ~ofs:var.v_ofs rbp) (!%rdx)
      else
        movq (ind ~ofs:var.v_ofs rbp) (!%rax)
    in
    (nop, code)
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
          call "strlen" ++        (* 調用 C 的 strlen 函數 *)
          subq (imm 1) (!%rax)    (* 減去尾部的換行符號 *)
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
                movq (!%r12) (!%rdx) ++
                call "strcpy" ++      (* 拷贝左字符串 *)
                right_code ++
                movq (!%r12) (!%rdx) ++
                call "strcat"         (* 拼接右字符串 *)
              in
              concat_code
          | _ -> addq (!%rbx) (!%rax) )
      | Bsub -> subq (!%rbx) (!%rax)
      | Bmul -> imulq (!%rbx) (!%rax)
      | Bdiv -> cqto ++ idivq (!%rbx)
      | Bmod -> cqto ++ idivq (!%rbx) ++ movq (!%rdx) (!%rax)
      | Beq | Bneq | Blt | Ble | Bgt | Bge ->
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
          label end_label
      | Band -> andq (!%rbx) (!%rax)
      | Bor -> orq (!%rbx) (!%rax)
      | _ -> failwith "Unsupported operator"
    in
    (left_data ++ right_data, right_code ++ pushq (!%rax) ++ left_code ++ popq rbx ++ op_code)  
  | _ -> failwith "Unsupported expression"

(* 生成语句的汇编代码 *)
let rec generate_stmt stmt =
  match stmt with
  | TSprint expr ->
    let data_expr, code_expr = generate_expr expr in
    (* 判斷是否為布爾比較運算 *)
    let is_boolean_comparison =
      match expr with
      | TEbinop ((Blt | Ble | Bgt | Bge | Beq | Bneq | Band | Bor) as op, left, right) -> true
      | TEunop (Unot, _) -> true
      | TEcst (Cbool _) -> true
      | _ -> false
    in
    (* 格式化字串選擇 *)
    let format_label =
      if is_boolean_comparison then ".LCs"
      else
        match expr with
        | TEcst (Cstring _) -> ".LCs"
        | _ -> ".LCd"
    in
    (* 添加布爾值轉換邏輯 *)
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
    (* 確保格式化數據段存在 *)
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
    (* 加載格式字串 *)
    let load_format_string =
      if format_label = ".LCs" then
        movq (ilab format_label) (!%rdx)
      else
        movq (ilab format_label) (!%rdi)
    in
    (* 打印代碼 *)
    let print_code =
      code_expr ++
      bool_conversion_code ++
      movq (!%rax) (!%rsi) ++
      load_format_string ++
      movq (imm 0) (!%rax) ++
      call "printf" ++
      movq (imm 10) (!%rdi) ++  (* 10 是 '\n' 的 ASCII 值 *)
      call "putchar"
    in
    (data_expr ++ format_data, print_code)
  | TSassign (var, expr) ->
      let data, code = generate_expr expr in
      let assign_code = movq (!%rax) (ind ~ofs:var.v_ofs rbp) in
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
  Hashtbl.add string_table ".LCtrue" "True\n";
  Hashtbl.add string_table ".LCfalse" "False\n";
  Hashtbl.add string_table ".LCs" "%s\n";
  Hashtbl.add string_table ".LCd" "%d\n";
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
    label ".LCtrue" ++ string "True\n" ++
    label ".LCfalse" ++ string "False\n" ++
    label ".LCs" ++ string "%s\n" ++
    label ".LCd" ++ string "%d\n"
  in
  let len_code =
    globl "len" ++
    label "len" ++
    pushq (!%rbp) ++
    movq (!%rsp) (!%rbp) ++
    movq (ind ~ofs:16 rbp) (!%rsi) ++
    call "strlen" ++
    leave ++
    ret
  in
  (* 生成 main 函數的 text 部分 *)
  let text =
    globl "main" ++
    label "main" ++
    pushq (!%rbp) ++
    movq (!%rsp) (!%rbp) ++
    main_code ++
    movq (imm 0) (!%rax) ++
    leave ++
    ret
  in

  (* 返回包含 data 和 text 的結果，並將 string_data 添加到 data 中 *)
  { text = text ++ len_code; data = data ++ string_data }