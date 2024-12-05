open Format
open X86_64
open Ast

let debug = ref false

let string_table : (string, string) Hashtbl.t = Hashtbl.create 64

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
      let data = label label_name ++ string (s ^ "\n") in
      let code = movq (ilab label_name) (!%rdi) in
      (data, code)
  | TEcst (Cbool b) ->
      let label_name = if b then ".LCtrue" else ".LCfalse" in
      let bool_str = if b then "true" else "false" in
      let data =
        if Hashtbl.mem string_table label_name then
          nop
        else
          let bool_data = label label_name ++ string (bool_str ^ "\n") in
          Hashtbl.add string_table label_name bool_str;
          bool_data
      in
      let code = movq (ilab label_name) (!%rax) in
      (data, code)
  | TEvar var ->
    let code =
      if var.v_type = Tstring then
        movq (ind ~ofs:var.v_ofs rbp) (!%rdx)
      else
        movq (ind ~ofs:var.v_ofs rbp) (!%rax)
    in
    (nop, code)
  | TEbinop (op, left, right) ->
      let left_data, left_code = generate_expr left in
      let right_data, right_code = generate_expr right in
      let op_code =
        match op with
        | Badd -> addq (!%rbx) (!%rax)
        | Bsub -> subq (!%rbx) (!%rax)
        | Bmul -> imulq (!%rbx) (!%rax)
        | Bdiv -> cqto ++ idivq (!%rbx)
        | Bmod -> cqto ++ idivq (!%rbx) ++ movq (!%rdx) (!%rax)
        | Band -> andq (!%rbx) (!%rax)
        | Bor -> orq (!%rbx) (!%rax)
        | Beq ->
            let set_rax_to_1_label = fresh_unique_label () in
            cmpq (!%rbx) (!%rax) ++
            movq (imm 0) (!%rax) ++
            jne set_rax_to_1_label ++
            movq (imm 1) (!%rax) ++
            label set_rax_to_1_label ++ nop
        | Bneq ->
            let set_rax_to_1_label = fresh_unique_label () in
            cmpq (!%rbx) (!%rax) ++
            movq (imm 0) (!%rax) ++
            je set_rax_to_1_label ++
            movq (imm 1) (!%rax) ++
            label set_rax_to_1_label ++ nop
        | Blt ->  (* a < b *)
            let set_rax_to_1_label = fresh_unique_label () in
            cmpq (!%rbx) (!%rax) ++
            movq (imm 1) (!%rax) ++
            jl set_rax_to_1_label ++
            movq (imm 0) (!%rax) ++
            label set_rax_to_1_label ++ nop
        | Ble ->  (* a <= b *)
            let set_rax_to_1_label = fresh_unique_label () in
            cmpq (!%rbx) (!%rax) ++
            movq (imm 1) (!%rax) ++
            jle set_rax_to_1_label ++
            movq (imm 0) (!%rax) ++
            label set_rax_to_1_label ++ nop
        | Bgt ->  (* a > b *)
            let set_rax_to_1_label = fresh_unique_label () in
            cmpq (!%rbx) (!%rax) ++
            movq (imm 1) (!%rax) ++
            jg set_rax_to_1_label ++
            movq (imm 0) (!%rax) ++
            label set_rax_to_1_label ++ nop
        | Bge ->  (* a >= b *)
            let set_rax_to_1_label = fresh_unique_label () in
            cmpq (!%rbx) (!%rax) ++
            movq (imm 1) (!%rax) ++
            jge set_rax_to_1_label ++
            movq (imm 0) (!%rax) ++
            label set_rax_to_1_label ++ nop
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
      | TEbinop ((Blt | Ble | Bgt | Bge | Beq | Bneq) as op, left, right) -> true
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
      call "printf"
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
  Hashtbl.add string_table ".LCd" "%d\n"

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

  (* 生成 main 函數的 text 部分 *)
  let text =
    globl "main" ++
    label "main" ++
    pushq (!%rbp) ++
    movq (!%rsp) (!%rbp) ++
    main_code ++
    leave ++
    ret
  in

  (* 返回包含 data 和 text 的結果，並將 string_data 添加到 data 中 *)
  { text; data = data ++ string_data }
