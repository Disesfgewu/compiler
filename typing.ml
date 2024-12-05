open Ast

(* 異常定義 *)
exception Error of Ast.location * string

(* 符號表，用於存儲變量名稱及其偏移量 *)
let symbol_table : (string, var) Hashtbl.t = Hashtbl.create 16

(* 當前變量偏移量（負數，向下擴展） *)
let current_offset = ref (-16)


let function_table : (string, fn) Hashtbl.t = Hashtbl.create 16

(* 報告錯誤 *)
let error ?(loc=Lexing.dummy_pos, Lexing.dummy_pos) fmt =
  Format.kasprintf (fun msg -> raise (Error (loc, msg))) fmt



(* 類型檢查表達式 *)
let rec check_expr (e : expr) : texpr =
  match e with
  | Ecst (Cstring s) -> TEcst (Cstring s)
  | Ecst (Cint i) -> TEcst (Cint i)  (* 常量直接類型化 *)
  | Ecst (Cbool b) -> TEcst (Cbool b)
  | Eident id ->
      if Hashtbl.mem symbol_table id.id then
        TEvar (Hashtbl.find symbol_table id.id)  (* 使用符號表中變量信息 *)
      else
        error ~loc:id.loc "Undefined variable: %s" id.id
  | Ebinop (op, e1, e2) ->  (* 二元運算符 *)
      let te1 = check_expr e1 in
      let te2 = check_expr e2 in
      TEbinop (op, te1, te2)
  | Ecall (id, args) ->
    if not (Hashtbl.mem function_table id.id) then
      error ~loc:id.loc "Undefined function: %s" id.id;
    let fn = Hashtbl.find function_table id.id in
    let targs = List.map check_expr args in
    TEcall (fn, targs)    
  | _ -> error "Unsupported expression"

(* 類型檢查語句 *)
let rec check_stmt (s : stmt) : tstmt =
  match s with
  | Sprint e -> TSprint (check_expr e)  (* 支持 print 語句 *)
  | Sassign (id, expr) ->
      (* 類型檢查右值表達式 *)
      let texpr = check_expr expr in
      (* 獲取右值的類型 *)
      let expr_type =
        match texpr with
        | TEcst (Cint _) -> Tint
        | TEcst (Cbool _) -> Tbool
        | TEcst (Cstring _) -> Tstring
        | _ -> Tnone
      in
      (* 檢查變量是否已存在符號表中 *)
      let var =
        if Hashtbl.mem symbol_table id.id then
          (* 如果變量已存在，檢查類型是否一致 *)
          let existing_var = Hashtbl.find symbol_table id.id in
          if existing_var.v_type <> expr_type then
            error ~loc:id.loc "Type mismatch for variable: %s" id.id;
          existing_var
        else
          (* 如果變量不存在，則創建新變量並分配偏移量 *)
          let new_var = { v_name = id.id; v_ofs = !current_offset; v_type = expr_type } in
          Hashtbl.add symbol_table id.id new_var;
          current_offset := !current_offset - 8;  (* 每個變量占用 8 字節 *)
          new_var
      in
      TSassign (var, texpr)
  | Sif (cond, then_branch, else_branch) -> 
      let tcond = check_expr cond in
      let tthen = check_stmt then_branch in
      let telse = check_stmt else_branch in
      TSif (tcond, tthen, telse)
  | Sblock stmts -> TSblock (List.map check_stmt stmts)  (* 支持代碼塊 *)
  | Sdef (id, params, body) ->
      (* 定義函數參數的符號表 *)
      let fn_params = 
        List.mapi (fun i param ->
          let param_var = { v_name = param.id; v_ofs = 8 * (i + 2); v_type = Tint } in
          Hashtbl.add symbol_table param.id param_var;
          param_var
        ) params
      in
      (* 檢查函數體 *)
      let tbody = check_stmt body in
      (* 返回類型化的函數定義 *)
      TSdef ({ fn_name = id.id; fn_params }, tbody)
  | Sreturn expr ->
      let texpr = check_expr expr in
      TSreturn texpr
  | _ -> error "Unsupported statement"

  let check_def (id, params, body) : tdef =
    (* 檢查函數是否已定義 *)
    if Hashtbl.mem function_table id.id then
      error ~loc:id.loc "Function already defined: %s" id.id;
  
    (* 定義函數參數 *)
    let fn_params =
      List.mapi (fun i param ->
        let param_var = { v_name = param.id; v_ofs = 8 * (i + 2); v_type = Tint } in
        Hashtbl.add symbol_table param.id param_var;
        param_var
      ) params
    in
  
    (* 檢查函數體 *)
    let tbody = check_stmt body in
  
    (* 創建函數記錄並添加到函數符號表 *)
    let fn = { fn_name = id.id; fn_params } in
    Hashtbl.add function_table id.id fn;
  
    (fn, tbody)
let file ~(debug: bool) (p: Ast.file) : Ast.tfile =
  if debug then print_endline "Debugging enabled: Starting type checking...";
  let defs, stmts = p in
  let tdefs = List.map check_def defs in
  let tstmts = check_stmt stmts in
  let main_fn = { fn_name = "main"; fn_params = [] } in
  (main_fn, TSblock [tstmts]) :: tdefs