open Ast

(* 异常定义 *)
exception Error of Ast.location * string

(* 报告错误 *)
let error ?(loc=Lexing.dummy_pos, Lexing.dummy_pos) fmt =
  Format.kasprintf (fun msg -> raise (Error (loc, msg))) fmt

(* 类型检查表达式 *)
let rec check_expr (e : expr) : texpr =
  match e with
  | Ecst c -> TEcst c  (* 常量直接类型化 *)
  | Eident id -> TEvar { v_name = id.id; v_ofs = 0 }  (* 变量 *)
  | Ebinop (op, e1, e2) ->  (* 二元运算符 *)
      let te1 = check_expr e1 in
      let te2 = check_expr e2 in
      TEbinop (op, te1, te2)
  | Ecall (id, args) ->  (* 函数调用 *)
      let targs = List.map check_expr args in
      TEcall ({ fn_name = id.id; fn_params = [] }, targs)
  | _ -> error "Unsupported expression"

(* 类型检查语句 *)
let rec check_stmt (s : stmt) : tstmt =
  match s with
  | Sprint e -> TSprint (check_expr e)  (* 支持 print 语句 *)
  | Sassign (id, expr) -> 
      TSassign ({ v_name = id.id; v_ofs = 0 }, check_expr expr)  (* 支持赋值 *)
  | Sif (cond, then_branch, else_branch) ->  (* 支持 if 语句 *)
      let tcond = check_expr cond in
      let tthen = check_stmt then_branch in
      let telse = check_stmt else_branch in
      TSif (tcond, tthen, telse)
  | Sblock stmts -> TSblock (List.map check_stmt stmts)  (* 支持代码块 *)
  | Sreturn expr -> TSreturn (check_expr expr)  (* 支持 return 语句 *)
  | _ -> error "Unsupported statement"

(* 类型检查函数 *)
let check_def (id, params, body) : tdef =
  let fn_params = List.map (fun param -> { v_name = param.id; v_ofs = 0 }) params in
  let tbody = check_stmt body in
  let fn = { fn_name = id.id; fn_params } in
  (fn, tbody)

(* 类型检查文件 *)
let file ~(debug: bool) (p: Ast.file) : Ast.tfile =
  if debug then print_endline "Debugging enabled: Starting type checking...";
  let defs, stmts = p in
  let tdefs = List.map check_def defs in
  let tstmts = check_stmt stmts in
  let main_fn = { fn_name = "main"; fn_params = [] } in
  (main_fn, tstmts) :: tdefs
