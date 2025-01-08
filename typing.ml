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

(* 檢查是否為布林類型 *)
let is_bool_type (texpr : texpr) : bool =
  match texpr with
  | TEcst (Cbool _) -> true
  | TEvar var when var.v_type = Tbool -> true
  | _ -> false

(* 檢查是否為整數類型 *)
let rec is_int_type (texpr : texpr) : bool =
  match texpr with
  | TEcst (Cint _) -> true
  | TEvar var when var.v_type = Tint -> true
  | TEbinop (_, left, right) -> is_int_type left && is_int_type right
  | TEunop (Uneg, expr) -> is_int_type expr
  | _ -> false

let is_list_type (texpr : texpr) : bool =
  match texpr with
  | TElist (_)-> true
  (* | TEvar var when var.v_type = Tlist -> true、 *)
  | _ -> false

let rec print_expr fmt expr =
  match expr with
  | Ecst c -> Format.fprintf fmt "Ecst(%s)" (match c with
      | Cnone -> "None"
      | Cbool b -> string_of_bool b
      | Cstring s -> Printf.sprintf "\"%s\"" s
      | Cint i -> Int64.to_string i)
  | Eident id -> Format.fprintf fmt "Eident(%s)" id.id
  | Ebinop (op, e1, e2) -> 
      Format.fprintf fmt "Ebinop(%s, %a, %a)"
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
        print_expr e1 print_expr e2
  | Eunop (op, e) -> 
      Format.fprintf fmt "Eunop(%s, %a)"
        (match op with
        | Uneg -> "-"
        | Unot -> "not")
        print_expr e
  | Ecall (id, args) ->
      Format.fprintf fmt "Ecall(%s, [%a])"
        id.id
        (Format.pp_print_list print_expr) args
  | Elist elements ->
      Format.fprintf fmt "Elist([%a])"
        (Format.pp_print_list print_expr) elements
  | Eget (e1, e2) ->
      Format.fprintf fmt "Eget(%a, %a)" print_expr e1 print_expr e2

(* 類型檢查表達式 *)
let rec check_expr (e : expr) : texpr =
  match e with
  | Ecst (Cstring s) -> TEcst (Cstring s)
  | Ecst (Cint i) -> TEcst (Cint i)  (* 常量直接類型化 *)
  | Ecst (Cbool b) -> TEcst (Cbool b)
  | Ecst Cnone -> TEcst Cnone (* 新增對 None 常量的處理 *)
  | Elist elements ->
      let t_elements = List.map check_expr elements in
      TElist t_elements
  | Eget (e1, e2) -> 
      let t_e1 = check_expr e1 in
      let t_e2 = check_expr e2 in
      TEget (t_e1, t_e2)
  | Eident id ->
      if Hashtbl.mem symbol_table id.id then
        TEvar (Hashtbl.find symbol_table id.id)  (* 使用符號表中變量信息 *)
      else
        error ~loc:id.loc "Undefined variable: %s" id.id
  | Eunop (Unot, e) -> 
      let te = check_expr e in
      if is_bool_type te then TEunop (Unot, te)
      else error "not operation only supports boolean expressions"
  | Eunop (Uneg, e) ->  (* 新增負號運算的處理 *)
      let te = check_expr e in
      if is_int_type te then TEunop (Uneg, te)
      else error "negation operation only supports integer expressions"
  | Ecall (id, args) when id.id = "len" ->
    if List.length args <> 1 then
      error ~loc:id.loc "Function 'len' expects exactly 1 argument.";
    let t_arg = check_expr (List.hd args) in
    (match t_arg with
    | TElist elements ->
      TEcst (Cint (Int64.of_int (List.length elements))) (* 返回清單長度 *)
    | TEcst (Cint _) | TEcst (Cbool _) | TEcst (Cstring _) ->
        TEcall ({ fn_name = "len"; fn_params = [] }, [t_arg])
    | _ -> error ~loc:id.loc "Function 'len' expects a list or a valid argument.")
  | Ebinop (op, e1, e2) ->  (* 二元運算符 *)
      let te1 = check_expr e1 in
      let te2 = check_expr e2 in
      TEbinop (op, te1, te2)
  | Ecall (id, args) when id.id = "range" ->
      if List.length args <> 1 then
        error ~loc:id.loc "Function 'range' expects exactly 1 argument.";
      let t_arg = check_expr (List.hd args) in
      if not (is_int_type t_arg) then
        error ~loc:id.loc "Function 'range' expects an integer argument.";
      (match t_arg with
      | TEcst (Cint n) ->
          let elements = List.init (Int64.to_int n) (fun i -> TEcst (Cint (Int64.of_int i))) in
          TElist elements
      | _ -> error "Dynamic range generation not supported.")
  | Ecall (id, args) when id.id = "list" ->
      if List.length args <> 1 then
        error ~loc:id.loc "Function 'list' expects exactly 1 argument.";
      let arg = List.hd args in
      (match arg with
      | Ecall (range_id, range_args) when range_id.id = "range" ->
          if List.length range_args <> 1 then
            error ~loc:range_id.loc "Function 'range' expects exactly 1 argument.";
          let t_arg = check_expr (List.hd range_args) in
          (* if not (is_int_type t_arg) then
            error ~loc:range_id.loc "Function 'range' expects an integer argument."; *)
          (match t_arg with
          | TEcst (Cint n) ->
              let elements = List.init (Int64.to_int n) (fun i -> TEcst (Cint (Int64.of_int i))) in
              TElist elements
          | TEvar var when var.v_type = Tint ->
                (* 如果是整數變數，保留為動態處理 *)
              TEcall ({ fn_name = "range"; fn_params = [] }, [t_arg])
          | TElist (_) ->
            TEcall ({ fn_name = "range_fail"; fn_params = [] }, [t_arg])
          | _ -> error "Dynamic range generation not supported.")
      | _ -> error ~loc:id.loc "Function 'list' expects a range as its argument.")
  | Ecall (id, args) ->
    if not (Hashtbl.mem function_table id.id) then
      error ~loc:id.loc "Undefined function: %s" id.id;
    let fn = Hashtbl.find function_table id.id in
    let targs = List.map check_expr args in
    TEcall (fn, targs)
  | _ ->
      Format.printf "Expression not supported: %a@." print_expr e;
      error "Unsupported expression"


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
        | TElist _ -> Tnone
        | TEbinop (_, left, right) -> 
          if is_int_type left && is_int_type right then Tint
          else error ~loc:id.loc "Binary operation expects integer operands."
        | TEcall (fn, _) ->
          (* 根據函數返回值的預定義類型來設置變量類型 *)
          if Hashtbl.mem function_table fn.fn_name then
            let fn_info = Hashtbl.find function_table fn.fn_name in
            (* 假設所有函數返回整數類型，根據實際需求修改 *)
            Tint
          else 
            Tnone
        | TEvar var -> var.v_type  (* 使用變量類型 *) 
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
    if Hashtbl.mem function_table id.id then
      error ~loc:id.loc "Function already defined: %s" id.id;
    let fn_params =
      List.mapi (fun i param ->
        let param_var = { v_name = param.id; v_ofs = 8 * (i + 2); v_type = Tint } in
        Hashtbl.add symbol_table param.id param_var;
        param_var
      ) params
    in
    let tbody = check_stmt body in
    let fn = { fn_name = id.id; fn_params } in
    Hashtbl.add function_table id.id fn;
    TSdef (fn, tbody)
  | Sreturn expr ->
      let texpr = check_expr expr in
      TSreturn texpr
  | Sfor (id, iterable, body) ->
    (* 類型化迭代器 *)
    let t_iterable = check_expr iterable in
    let var_type =
      match t_iterable with
      | TElist elements ->
          (match elements with
          | TEcst (Cint _) :: _ -> Tint
          | TEcst (Cbool _) :: _ -> Tbool
          | TEcst (Cstring _) :: _ -> Tstring
          | _ -> Tint)
      | TEcall (fn, _) when fn.fn_name = "range" ->
          Tint
      | TEvar var when var.v_type = Tnone ->
          Tint
      | TEcst n ->
          Tnone
      | _ -> error ~loc:id.loc "Unsupported iterable in for loop"
    in

    (* 暫時允許覆蓋全局變量 *)
    let var =
      if Hashtbl.mem symbol_table id.id then
        Hashtbl.find symbol_table id.id
      else
        let new_var = { v_name = id.id; v_ofs = !current_offset; v_type = var_type } in
        Hashtbl.add symbol_table id.id new_var;
        current_offset := !current_offset - 8;
        new_var
    in

    (* 類型化循環體 *)
    let t_body = check_stmt body in

    (* 返回類型化的 for 循環 *)
    TSfor (var, t_iterable, t_body)   
  | Seval expr -> 
    let texpr = check_expr expr in
    (match texpr with
    | TEcall (fn, _) -> TSeval texpr
    | _ -> error "Only function calls are allowed in evaluation statements")
  | Sset (e1, e2, e3) ->
    let te1 = check_expr e1 in
    let te2 = check_expr e2 in
    let te3 = check_expr e3 in
    TSset (te1, te2, te3)
  | _ -> error "Unsupported statement"

  let check_def (id, params, body) : tdef =
    (* 檢查函數是否已定義 *)
    if Hashtbl.mem function_table id.id then
      error ~loc:id.loc "Function already defined: %s" id.id;
  
    (* 定義函數參數 *)
    let fn_params =
      List.mapi (fun i param ->
        let param_var = { v_name = param.id; v_type = Tint; v_ofs = 8 * (i + 2) } in
        Hashtbl.add symbol_table param.id param_var;
        param_var
      ) params
    in
  
    (* 創建函數記錄並立即添加到符號表，支持遞迴調用 *)
    let fn = { fn_name = id.id; fn_params } in
    Hashtbl.add function_table id.id fn;
  
    (* 檢查函數體 *)
    let tbody = check_stmt body in
  
    (* 返回類型化的函數定義 *)
    (fn, tbody)
  

let initialize () =
  (* 初始化內建函數 len *)
  Hashtbl.add function_table "len" {
    fn_name = "len";
    fn_params = [{ v_name = "arg"; v_ofs = 16; v_type = Tstring }];
  }

let file ~(debug: bool) (p: Ast.file) : Ast.tfile =
  if debug then print_endline "Debugging enabled: Starting type checking...";
  let defs, stmts = p in
  initialize ();
  let tdefs = List.map check_def defs in
  let tstmts = check_stmt stmts in
  let main_fn = { fn_name = "main"; fn_params = [] } in
  (main_fn, TSblock [tstmts]) :: tdefs