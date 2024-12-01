open Ast

(* 異常定義 *)
exception Error of Ast.location * string

(* 符號表，用於存儲變量名稱及其偏移量 *)
let symbol_table : (string, var) Hashtbl.t = Hashtbl.create 16

(* 當前變量偏移量（負數，向下擴展） *)
let current_offset = ref (-16)

(* 報告錯誤 *)
let error ?(loc=Lexing.dummy_pos, Lexing.dummy_pos) fmt =
  Format.kasprintf (fun msg -> raise (Error (loc, msg))) fmt

(* 類型檢查表達式 *)
let rec check_expr (e : expr) : texpr =
  match e with
  | Ecst (Cstring s) -> TEcst (Cstring s)
  | Ecst (Cint i) -> TEcst (Cint i)  (* 常量直接類型化 *)
  | Eident id ->
      if Hashtbl.mem symbol_table id.id then
        TEvar (Hashtbl.find symbol_table id.id)  (* 使用符號表中變量信息 *)
      else
        error ~loc:id.loc "Undefined variable: %s" id.id
  | Ebinop (op, e1, e2) ->  (* 二元運算符 *)
      let te1 = check_expr e1 in
      let te2 = check_expr e2 in
      TEbinop (op, te1, te2)
  | Ecall (id, args) ->  (* 函數調用 *)
      let targs = List.map check_expr args in
      TEcall ({ fn_name = id.id; fn_params = [] }, targs)
  | _ -> error "Unsupported expression"

(* 類型檢查語句 *)
let rec check_stmt (s : stmt) : tstmt =
  match s with
  | Sprint e -> TSprint (check_expr e)  (* 支持 print 語句 *)
  | Sassign (id, expr) ->
      (* 類型檢查右值表達式 *)
      let texpr = check_expr expr in
      (* 檢查變量是否已存在符號表中 *)
      let var =
        if Hashtbl.mem symbol_table id.id then
          (* 如果變量已存在，直接使用 *)
          Hashtbl.find symbol_table id.id
        else
          (* 如果變量不存在，則創建新變量並分配偏移量 *)
          let new_var = { v_name = id.id; v_ofs = !current_offset } in
          Hashtbl.add symbol_table id.id new_var;
          current_offset := !current_offset - 8;  (* 每個變量占用 8 字節 *)
          new_var
      in
      TSassign (var, texpr)
  | Sif (cond, then_branch, else_branch) ->  (* 支持 if 語句 *)
      let tcond = check_expr cond in
      let tthen = check_stmt then_branch in
      let telse = check_stmt else_branch in
      TSif (tcond, tthen, telse)
  | Sblock stmts -> TSblock (List.map check_stmt stmts)  (* 支持代碼塊 *)
  | Sreturn expr -> TSreturn (check_expr expr)  (* 支持 return 語句 *)
  | _ -> error "Unsupported statement"

(* 類型檢查函數 *)
let check_def (id, params, body) : tdef =
  (* 為每個參數分配偏移量 *)
  let fn_params = 
    List.mapi (fun i param ->
      { v_name = param.id; v_ofs = 8 * (i + 2) }  (* 函數參數從 rbp+16 開始存儲 *)
    ) params
  in
  (* 將參數添加到符號表 *)
  List.iter (fun param -> Hashtbl.add symbol_table param.v_name param) fn_params;
  (* 檢查函數體 *)
  let tbody = check_stmt body in
  (* 返回函數定義 *)
  let fn = { fn_name = id.id; fn_params } in
  (fn, tbody)

(* 類型檢查文件 *)
let file ~(debug: bool) (p: Ast.file) : Ast.tfile =
  if debug then print_endline "Debugging enabled: Starting type checking...";
  let defs, stmts = p in
  let tdefs = List.map check_def defs in
  let tstmts = check_stmt stmts in
  let main_fn = { fn_name = "main"; fn_params = [] } in
  (main_fn, tstmts) :: tdefs
