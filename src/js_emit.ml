open Messages
open Types
open Printf
open Utils

let rec string_of_addr addr =
  sprintf "oc$%s" (Utils.roman addr)
and string_of_global ?(data = IntMap.empty) n =
  sprintf "og$%s" (match (try snd (IntMap.find n data) with Not_found -> None) with None -> Utils.roman n | Some s -> s)
and string_of_local n =
  if n = 0 then
    "this.accu"
  else
    sprintf "this.v%s" (Utils.roman n)
and string_of_arg n =
  sprintf "this.args[%d]" n
and string_of_value (value : vbc_value) =
  match value with
  | `Int i -> sprintf "%d" i (* FIXME: val_int *)
  | `Float f -> sprintf "val_float (%g)" f
  | `String s -> sprintf "val_string (%S)" s
  | `Int32 i32 -> sprintf "val_int32 (%ld)" i32
  | `Int64 i64 -> sprintf "val_int64 (parseInt64 (\"%Ld\"))" i64
and string_of_slvalue ?(data = IntMap.empty) (slv : vbc_slvalue) =
  match slv with
  | `Env n ->
    sprintf "this.clos.env[%d]" n
  | `Var n ->
    string_of_local n
  | `Arg n ->
    string_of_arg n
  | `Global n ->
    string_of_global ~data n
and string_of_lvalue ?(data = IntMap.empty) (lv : vbc_lvalue) =
  match lv with
  | `Field (rlv, rv) ->
    sprintf "%s.c[%s]" (string_of_rvalue ~data rlv) (string_of_rvalue ~data rv)
    (* sprintf "field (%s, %s)" (string_of_rvalue ~data rlv) (string_of_rvalue ~data rv) *)
  |  #vbc_slvalue as slv ->
    string_of_slvalue ~data slv
and string_of_srvalue ?(data = IntMap.empty) (srv : vbc_srvalue) =
  match srv with
  | `Const v ->
    string_of_value v
  | #vbc_slvalue as lv ->
    string_of_lvalue ~data lv
and string_of_rvalue ?(data = IntMap.empty) (rv : vbc_rvalue) =
  match rv with
  | `Field (rlv, rv) ->
    sprintf "%s.c[%s]" (string_of_rvalue ~data rlv) (string_of_rvalue ~data rv)
    (* sprintf "field (%s, %s)" (string_of_rvalue ~data rlv) (string_of_rvalue ~data rv) *)
  | `Call (`External n, l) ->
    sprintf "%s (%s)" n (sprintf_list "%s" "," (List.map (string_of_rvalue ~data) l))
  | `Call (`Neg, l) ->
    sprintf "-(%s)" (sprintf_list "%s" "," (List.map (string_of_rvalue ~data) l))
  | `Call (`Not, l) ->
    sprintf "!(%s)" (sprintf_list "%s" "," (List.map (string_of_rvalue ~data) l))
  | `Call (`Add|`Sub|`Mul|`Div|`Mod|`And|`Or|`Xor|`Lsl|`Lsr|`Asr|`Eq|`Neq|`Lt|`Le|`Gt|`Ge|`Ult|`Uge as op, [ l; r ]) ->
    string_of_binop ~data op l r
  | `Call _ -> assert false
  | `Size v ->
    sprintf "%s.s" (string_of_rvalue ~data v)
  (*sprintf "o$size (%s)" (string_of_rvalue ~data v)*)
  | `Tag v ->
    sprintf "%s.t" (string_of_rvalue ~data v)
  (*sprintf "o$tag (%s)" (string_of_rvalue ~data v)*)
  | `Is_int v ->
    sprintf "o$is_int (%s)" (string_of_rvalue ~data v)
  | `Alloc (s, t) ->
    sprintf "o$alloc (%s, %s)" (string_of_rvalue ~data s) (string_of_rvalue ~data t)
  | `Tuple (t, v) ->
    (*sprintf "{c:[%s],t:%s,s:%d}" (sprintf_list "%s" "," (List.map (string_of_rvalue ~data) v)) (string_of_rvalue ~data t) (List.length v)*)
    sprintf "o$tuple ([%s], %s)" (sprintf_list "%s" "," (List.map (string_of_rvalue ~data) v)) (string_of_rvalue ~data t)
  | `Closure (addr, arity, args) ->
    sprintf "o$closure (%s, %d, [%s])" (string_of_addr addr) arity (sprintf_list "%s" "," (List.map (string_of_rvalue ~data) args))
  | `Offset_closure (addr, arity) ->
    sprintf "o$offset_closure (%s, %d)" (string_of_addr addr) arity
  | `Lookup (obj, lab) ->
    sprintf "o$lookup (%s, %s)" (string_of_rvalue ~data obj) (string_of_rvalue ~data lab)
  | #vbc_srvalue as srv ->
    string_of_srvalue ~data srv
and string_of_binop ?(data = IntMap.empty) op (l : vbc_rvalue) (r : vbc_rvalue) =
  match op with
  | `Add -> sprintf "$Z(%s + %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Sub -> sprintf "$Z(%s - %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Mul -> sprintf "$Z(%s * %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Div -> sprintf "$Z(%s / %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Mod -> sprintf "$Z(%s %% %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `And -> sprintf "(%s & %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Or -> sprintf "(%s | %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Xor -> sprintf "(%s ^ %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Lsl -> sprintf "$Z(%s << %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Lsr -> sprintf "(%s >> %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Asr -> sprintf "(%s >>> %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Eq -> sprintf "(%s == %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Neq -> sprintf "(%s != %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Lt -> sprintf "(%s < %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Le -> sprintf "(%s <= %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Gt -> sprintf "(%s > %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Ge -> sprintf "(%s >= %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Ult -> sprintf "ult (%s, %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
  | `Uge -> sprintf "uge (%s, %s)" (string_of_rvalue ~data l) (string_of_rvalue ~data r)
and string_of_instr ?(data = IntMap.empty) (instr : vbc_instr) =
  match instr with
  | `Store (`Field (rlv, rv), drv) ->
    sprintf "store_field (%s, %s, %s)" (string_of_rvalue ~data rlv) (string_of_rvalue ~data rv) (string_of_rvalue ~data drv)
  | `Store (lv, rv) ->
    sprintf "%s = %s" (string_of_lvalue ~data lv) (string_of_rvalue ~data rv)
  | `Apply (f, a, r) ->
    sprintf "o$apply (%s, [%s], %s)" (string_of_rvalue ~data f) (sprintf_list "%s" "," (List.map (string_of_rvalue ~data) a)) (string_of_addr r)
  | `App_term (f, a) ->
    sprintf "o$appterm (%s, [%s])" (string_of_rvalue ~data f) (sprintf_list "%s" "," (List.map (string_of_rvalue ~data) a))
  | `Return v ->
    sprintf "o$return (%s)" (string_of_rvalue ~data v)
  | `Check_signals -> "o$checkpoint ()"
  | `Stop -> "o$stop ()"
  | `Event -> "o$event ()"
  | `Break -> "o$break ()"
  | `Switch (v, li, lp) ->
    sprintf "o$switch (%s, [%s], [%s])" (string_of_rvalue ~data v) (sprintf_list "%s" "," (List.map string_of_addr li)) (sprintf_list "%s" "," (List.map string_of_addr lp))
  | `Push_trap a ->
    sprintf "o$push_trap (%s)" (string_of_addr a)
  | `Pop_trap -> "o$pop_trap ()"
  | `Raise v -> sprintf "o$raise (%s)" (string_of_rvalue ~data v)
  (* unused when called from print_program *)
  | `Ext_call (d, n, l, r) ->
    sprintf "  %s = %s (%s, %s)" (string_of_lvalue ~data d) n (sprintf_list "%s" "," (List.map (string_of_rvalue ~data) l)) (string_of_addr r)
  | `Branch (`Const (`Int n), addr) when n <> 0 -> sprintf "this.pc = %s ; return" (string_of_addr addr)
  | `Branch (v, addr) -> sprintf "if (o$branch (%s, %s)) return" (string_of_rvalue ~data v) (string_of_addr addr)

let print_program
    (fp : out_channel)
    (data : (vbc_rvalue * string option) IntMap.t)
    (segments : vbc_segment IntMap.t)
    (closures : int IntMap.t)
    (name : string)
    : unit =
  debug "js-emit" "printing global data" ;
  fprintf fp "/* %s - global data */\n" name ;
  IntMap.iter
    (* don't print zeroes, either they are placeholders or they have been inlined *)
    (fun i (v, n) -> if v <> `Const (`Int 0) then fprintf fp "%s\n%!" (string_of_instr ~data (`Store (`Global i, v))))
    data ;
  debug "js-emit" "printing program" ;
  fprintf fp "/* %s - program */\n" name ;
  (* Compute for each segment, true if shared, false if only used onces, in order to inline branching utins JS's if. *)
  let rcfg, _ = Analysis.build_backward_cfg vbc_branches segments (closure_entries closures) in
  let used_once = IntMap.map (fun x -> AddrSet.cardinal x <= 1) rcfg in
  IntMap.iter
    (fun head arity ->
      let _, segs = Analysis.build_forward_cfg vbc_branches segments [head] in
      let printed = ref IntSet.empty (* so we don't print already inlined segments *) in
      fprintf fp "/* closure %s start - arity %d */\n" (string_of_addr head) arity ;
      let rec print_seg a =
	let rec print_seg_contents indent a =
	  let seg = IntMap.find a segments in
	  let rec loop indent i =
	    if i < Array.length seg then begin
	      match seg.(i) with
	      | `Ext_call (d, n, l, r) ->
		fprintf fp "%*sthis.pc = %s;\n" indent "" (string_of_addr r) ;
		fprintf fp "%*s%s = %s (%s)\n" indent "" (string_of_lvalue ~data d) n (sprintf_list "%s" "," (List.map (string_of_rvalue ~data) l))
	      | `Branch (cond, addr) when IntMap.find addr used_once ->
		fprintf fp "%*sif (%s) {\n" indent "" (string_of_rvalue ~data cond) ;
		printed := IntSet.add addr !printed ;
		print_seg_contents (indent + 2) addr ;
		if i <> Array.length seg - 1 then begin
		  fprintf fp "%*s} else {\n" indent "" ;
		  loop (indent + 2) (i + 1)
		end ;
		fprintf fp "%*s}\n" indent ""
	      | `Branch (`Const (`Int n), addr) when n <> 0 ->
		fprintf fp "%*sthis.pc = %s\n" indent "" (string_of_addr addr) ;
	      | `Branch (cond, addr) ->
		fprintf fp "%*sif (%s) {\n" indent "" (string_of_rvalue ~data cond) ;
		fprintf fp "%*sthis.pc = %s\n" (indent + 2) "" (string_of_addr addr) ;
		if i <> Array.length seg - 1 then begin
		  fprintf fp "%*s} else {\n" indent "" ;
		  loop (indent + 2) (i + 1)
		end ;
		fprintf fp "%*s}\n" indent ""
	      | instr ->
		fprintf fp "%*s%s\n" indent "" (string_of_instr ~data instr) ;
		loop indent (i + 1)
	    end
	  in loop indent 0
	in
	if not (IntSet.mem a !printed) then begin
	  printed := IntSet.add a !printed ;
	  fprintf fp "function %s () {\n" (string_of_addr a) ;
	  print_seg_contents 2 a ;
	  fprintf fp "}\n"
	end
      in
      IntSet.iter print_seg segs)
    closures ;
  fprintf fp "oc$entry = oc$a\n" ;
  fprintf fp "og$exe_name = val_string (%S)\n" name

let print_annotated_program
    (type domain)
    (fp : out_channel)
    (data : (vbc_rvalue * string option) IntMap.t)
    (segments : vbc_segment IntMap.t)
    (original : bc_segment IntMap.t)
    (closures : int IntMap.t)
    (name : string)
    (string_of_annot : domain -> string)
    (annots : domain AddrMap.t)
    : unit =
  debug "js-emit" "printing global data" ;
  IntMap.iter
    (* don't print zeroes, either they are placeholders or they have been inlined *)
    (fun i (v, n) -> if v <> `Const (`Int 0) then fprintf fp "%s\n" (string_of_instr ~data (`Store (`Global i, v))))
    data ;
  debug "js-emit" "printing program" ;
  IntMap.iter
    (fun head arity ->
      let _, segs = Analysis.build_forward_cfg vbc_branches segments [head] in
      fprintf fp "/* closure %s start - arity %d */\n" (string_of_addr head) arity ;
      IntSet.iter
	(fun i ->
	  let seg = IntMap.find i segments in
	  fprintf fp "function %s () {\n" (string_of_addr i) ;
	  for j = 0 to Array.length seg - 1 do
	    fprintf fp "  %- 50s // %s\n"
	      (string_of_instr ~data seg.(j))
	      (try string_of_annot (AddrMap.find (i, j) annots) with Not_found -> "--")
	  done ;
	  fprintf fp "}\n")
	segs)
    closures ;
  fprintf fp "oc$entry = oc$a\n" ;
  fprintf fp "og$exe_name = val_string (%S)\n%!" name
