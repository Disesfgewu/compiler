open Format
open X86_64
open Ast

let debug = ref false

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
  | TSif (cond, then_branch, else_branch) ->
      Format.fprintf fmt "TSif(%a, %a, %a)" print_texpr cond print_tstmt then_branch print_tstmt else_branch
  | _ -> Format.fprintf fmt "Unsupported tstmt"

let file ?debug:(b=false) (tfile: Ast.tfile) : X86_64.program =
  debug := b;

  (* 遍历语法树生成代码 *)
  let rec generate_stmt stmt =
    match stmt with
    | TSprint (TEcst (Cstring s)) ->
        let label_name = ".LC0" in
        (* 数据段，添加换行符 *)
        let data = label label_name ++ string (s ^ "\n") in
        (* 文本段 *)
        let code =
          movq (ilab label_name) (reg rdi) ++
          movq (imm 0) (reg rax) ++
          call "printf"
        in
        (data, code)
    | TSblock stmts ->
        (* 遍历所有语句 *)
        let datas, codes = List.split (List.map generate_stmt stmts) in
        (* 合并所有数据段和代码段 *)
        let data = List.fold_left (++) nop datas in
        let code = List.fold_left (++) nop codes in
        (data, code)
    | _ ->
        if !debug then Format.printf "Unsupported statement: %a@." print_tstmt stmt;
        failwith "Unsupported statement"
  in

  (* 处理主函数 *)
  let data, main_code =
    let _, stmt = List.hd tfile in
    generate_stmt stmt
  in

  (* 包装主函数 *)
  let text =
    globl "main" ++
    label "main" ++
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) ++
    main_code ++
    leave ++
    ret
  in

  { text; data }
