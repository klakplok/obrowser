open Printf
open Types
open Messages
open Utils

(* common utilities *)
let max_var = ref 0
let update_max_var (segments : vbc_segments) : unit =
  let updater = object
    inherit vbc_iterator as mom
    method! iter_slvalue = function `Var i -> if i > !max_var then max_var := i | _ -> ()
  end in
  IntMap.iter (fun _ s -> Array.iter updater # iter_instr s) segments
let gen_var () : [> vbc_lvalue ] = incr max_var ; (`Var !max_var)

(* information on externals *)
let pure_externals : StringSet.t =
  List.fold_right StringSet.add
    [ 
      "caml_power_float" ; "caml_sin_float" ; "caml_cos_float" ; "caml_tan_float" ; "caml_atan_float" ;
      "caml_asin_float" ; "caml_acos_float" ; "caml_atan2_float" ; "caml_sinh_float" ; "caml_cosh_float" ; "caml_tanh_float" ;
      "caml_add_float" ; "caml_sub_float" ; "caml_div_float" ; "caml_mul_float" ; "caml_float_of_int" ; "caml_format_int" ; "caml_int_of_float" ;
      "caml_js_params" ; "caml_ml_string_length" ; "caml_nativeint_shift_left" ; "caml_nativeint_shift_right" ;
      "caml_nativeint_neg" ; "caml_nativeint_add" ; "caml_nativeint_sub" ; "caml_nativeint_mul" ; "caml_nativeint_div" ; "caml_nativeint_mod" ;
      "caml_nativeint_and" ; "caml_nativeint_or" ; "caml_nativeint_xor" ; "caml_nativeint_of_int" ; "caml_nativeint_to_int" ;
      "caml_int32_neg" ; "caml_int32_add" ; "caml_int32_sub" ; "caml_int32_mul" ; "caml_int32_div" ; "caml_int32_mod" ;
      "caml_int32_and" ; "caml_int32_or" ; "caml_int32_xor" ; "caml_int32_lsl" ; "caml_int32_asr" ; "caml_int32_lsr" ;
      "caml_int32_of_int" ; "caml_int32_to_int" ; "caml_int64_float_of_bits" ;
      "caml_int64_neg" ; "caml_int64_add" ; "caml_int64_sub" ; "caml_int64_mul" ; "caml_int64_div" ; "caml_int64_mod" ; "caml_int64_and" ;
      "caml_int64_or" ; "caml_int64_xor" ; "caml_int64_lsl" ; "caml_int64_asr" ; "caml_int64_shift_left" ; "caml_int64_shift_right" ;
      "caml_int64_lsr" ; "caml_int64_of_int" ; "caml_int64_to_int" ;
    ]
    StringSet.empty
let inlinable_externals : StringSet.t =
  List.fold_right StringSet.add
    [
      "caml_array_blit" ; "caml_array_get" ; "caml_array_set_addr" ; "caml_array_get_addr" ; "caml_array_unsafe_get" ; "caml_array_unsafe_set" ;
      "caml_blit_string" ; "caml_channel_descriptor" ; "caml_create_string" ; "caml_equal" ;
      "caml_greaterequal" ; "caml_greaterthan" ; "caml_hash" ; "caml_hash_univ_param" ;
      "caml_int_of_string" ; "caml_js_node_child" ; "caml_js_node_children" ; "caml_js_node_n_children" ; "caml_lessequal" ;
      "caml_make_array" ; "caml_make_vect" ; "caml_ml_out_channels_list" ; "caml_sys_time" ;
      "caml_obj_dup" ; "caml_obj_set_tag" ; "caml_obj_tag" ; "caml_register_named_value" ; "caml_string_get" ;
      "caml_string_notequal" ; "caml_sys_get_argv" ; "caml_sys_get_config" ; "caml_sys_getenv" ; "caml_sys_random_seed" ;
      "jsoo_extract" ; "jsoo_get" ; "jsoo_get_event_args" ; "jsoo_inject" ; "jsoo_set" ; "jsoo_wrap_event" ; "ml_get_char" ;
      "caml_obj_is_block" ; "caml_obj_is_int" ; "caml_obj_tag" ; "caml_obj_set_tag" ; "caml_obj_size" ;
      "caml_obj_field" ; "caml_obj_set_field" ; "caml_obj_block" ; "caml_obj_dup" ; "caml_obj_truncate" ;
    ]
    pure_externals
let eval_external_rvalue (prim : string) (args : vbc_rvalue list) (dest : vbc_lvalue) : vbc_instr list =
  match prim, args with
  | "caml_array_unsafe_get", [ arr ; idx ] -> [ `Store (dest, `Field (arr, idx)) ]
  | "caml_array_unsafe_set", [ arr ; idx ; v ] -> [ `Store (`Field (arr, idx), v) ]
  | "caml_obj_block", [ t ; s ] -> [ `Store (dest, `Alloc (s, t)) ]
  | "caml_obj_is_block", [ x ] -> [ `Store (dest, `Call (`Not, [`Is_int x])) ]
  | "caml_obj_is_int", [ x ] -> [ `Store (dest, `Is_int x) ]
  | "caml_obj_tag", [ x ] -> [ `Store (dest, `Tag x) ]
  | "caml_obj_size", [ x ] -> [ `Store (dest, `Size x) ]
  | "caml_obj_field", [ x ; f ] -> [ `Store (dest, `Field (x, f)) ]
  | "caml_obj_set_field", [ x ; f ; v ] -> [ `Store (`Field (x, f), v) ]
  | "caml_add_float", [ `Const (`Float x) ; `Const (`Float y) ] -> [ `Store (dest, `Const (`Float (x +. y))) ]
  | "caml_sub_float", [ `Const (`Float x) ; `Const (`Float y) ] -> [ `Store (dest, `Const (`Float (x -. y))) ]
  | "caml_div_float", [ `Const (`Float x) ; `Const (`Float y) ] -> [ `Store (dest, `Const (`Float (x /. y))) ]
  | "caml_mul_float", [ `Const (`Float x) ; `Const (`Float y) ] -> [ `Store (dest, `Const (`Float (x *. y))) ]
  | "caml_sys_get_argv", _ -> [ `Store (dest, `Alloc (`Const (`Int 0), `Const (`Int 0))) ]
  | "caml_sys_get_config", _ -> [ `Store (dest, `Tuple (`Const (`Int 0), [ `Const (`String "Unix") ; `Const (`Int 32) ; `Const (`Int 0) ])) ]
  | _ -> raise Not_found

(* Collects the use  of global variables.
   Counters store how many times each global has been (read, written, field read, field written).
   Field read and field written are set to > 0 when a global is aliased or passed to an external primitive. *)
let collect_globals_usage (segments : vbc_segments) (closures : vbc_closures) : (int * int * int * int) IntMap.t =
  debug ~level:1 "collect-globals-usage" "collecting information on globals" ;
  let merge old glob ((r2,w2,fr2,fw2) as use) =
    try
      let (r1,w1,fr1,fw1) = IntMap.find glob old in
      IntMap.add glob (r1 + r2, w1 + w2, fr1 + fr2, fw1 + fw2) old
    with Not_found ->
      IntMap.add glob use old
  in
  let eval instr old =
    let res = ref old in
    let iterator = object (self)
      inherit vbc_iterator as mom
      method! iter_instr instr =
	mom # iter_instr instr ;
	match instr with
	| `Store (`Global i, _)
	| `Ext_call (`Global i, _ ,_ ,_) -> res := merge !res i (0,1,0,0)
	| `Store (`Field (`Global i, _), _)
	| `Ext_call (`Field (`Global i, _), _ ,_ ,_) -> res := merge !res i (-1,0,0,1)
	| _ -> ()	
      method! iter_rvalue rv =
	match rv with
	| `Global i -> res := merge !res i (1,0,0,0)
	| `Field (`Global i, rv) -> res := merge !res i (0,0,1,0) ; self # iter_rvalue rv
	(* | `Call (`External _, rvs) -> *)
	| _ -> mom  # iter_rvalue rv
    end in 
    iterator # iter_instr instr ;
    !res
  in
  let entries = closure_entries closures in
  Analysis.single_analysis vbc_branches segments entries eval IntMap.empty

(* collects the set of alive variables and their occurrences from this point *)
let collect_alive_variables (segments : vbc_segments) (closures : vbc_closures) : IntSet.t AddrMap.t =
  debug ~level:1 "collect-occurrences" "collecting liveness information" ;
  let eval (a,ai) (b,bi) (instr_a : vbc_instr) la =
    (* collect in res the set of used variables (at each instruction, res := res - lvalue occs + rvalue occs) *)
    let res = ref la in
    let iterator = object
      inherit vbc_iterator as mom
      method! iter_instr instr =
	(* remove potential assigned lvalue then do regular rvalue collection *)
	match instr with
        | `Store (`Var i, rv) ->
	  res := IntSet.remove i !res ;
	  mom # iter_rvalue rv
	| `Ext_call (`Var i, _, rvs, _) ->
	  res := IntSet.remove i !res ;
	  List.iter mom # iter_rvalue rvs
	| `Push_trap _ when a = b && ai = bi + 1 ->
	  res := IntSet.remove 0 !res
	| `Apply (rv, rvs, _) ->
	  res := IntSet.remove 0 !res ;
	  mom # iter_rvalue rv ;
	  List.iter mom # iter_rvalue rvs
	| _ -> mom # iter_instr instr
      method! iter_slvalue = function
        | `Var i -> res := IntSet.add i !res
	| _ -> ()
    end in
    iterator # iter_instr instr_a ;
    !res
  in
  let init = closure_entries closures in
  Analysis.backward_analysis vbc_branches segments init eval IntSet.union IntSet.empty IntSet.equal

(* collects the set of alive variables and their occurrences from this point *)
let collect_occurences (segments : vbc_segments) (closures : vbc_closures) : AddrSet.t IntMap.t AddrMap.t =
  debug ~level:1 "collect-occurrences" "collecting live variable occurences" ;
  let unify v1 v2 =
    IntMap.merge
      (fun v ocs1 ocs2 ->
	let ocs1 = match ocs1 with None -> AddrSet.empty | Some s -> s in
	let ocs2 = match ocs2 with None -> AddrSet.empty | Some s -> s in
	Some (AddrSet.union ocs1 ocs2))
      v1 v2
  in
  let eval (a,ai) (b,bi) (instr_a : vbc_instr) la =
    (* collect in res the set of used variables (at each instruction, res := res + rvalue occs - lvalue occs) *)
    let res = ref la in
    let iterator = object
      inherit vbc_iterator as mom
      method! iter_instr instr =
	(* remove potential assigned lvalue then do regular rvalue collection *)
	match instr with
        | `Store (`Var i, rv) ->
	  res := IntMap.remove i !res ;
	  mom # iter_rvalue rv
	| `Ext_call (`Var i, _, rvs, _) ->
	  res := IntMap.remove i !res ;
	  List.iter mom # iter_rvalue rvs
	| `Push_trap _ when a = b && ai = bi + 1 ->
	  res := IntMap.remove 0 !res
	| `Apply (rv, rvs, _) ->
	  res := IntMap.remove 0 !res ;
	  mom # iter_rvalue rv ;
	  List.iter mom # iter_rvalue rvs
	| _ -> mom # iter_instr instr
      method! iter_slvalue = function
        | `Var i ->
	  let old = try IntMap.find i !res with Not_found -> AddrSet.empty in
	  res := IntMap.add i (AddrSet.add (a, ai) old) !res
	| _ -> ()
    end in
    iterator # iter_instr instr_a ;
    !res
  in
  let init = closure_entries closures in
  Analysis.backward_analysis vbc_branches segments init eval unify IntMap.empty (IntMap.equal AddrSet.equal)

(* collects simple aliases (variable = constant) *)
let collect_simple_aliases (segments : vbc_segments) (closures : vbc_closures) : vbc_rvalue LvalueMap.t AddrMap.t =
  let occurences = collect_occurences segments closures in
  let globals = collect_globals_usage segments closures in
  let unify v1 v2 =
    LvalueMap.merge
      (fun v o1 o2 ->
	match o1, o2 with
	| None, _ | _, None -> None
	| Some o1, Some o2 -> if o1 = o2 then Some o1 else None)
      v1 v2
  in
  let droppable (a : addr * int) (b : addr * int) (rv : vbc_rvalue) ((`Var v) as var) =
    let rec droppable rv =
      let occurs_only_once () =
	try
	  let occs = IntMap.find v (AddrMap.find a occurences) in
	  (AddrSet.cardinal occs = 1)
	  && (let (sa, ia) = AddrSet.choose occs in nb_appearances (IntMap.find sa segments).(ia) ((=) (`Var v)) = 1)
	with Not_found -> true
      in
      match rv with
      | `Call (`External prim, args) ->
	occurs_only_once () && StringSet.mem prim pure_externals && List.for_all (fun rv -> droppable rv) args
      | `Global _ -> false
      | `Var _ -> (rv <> (var :> vbc_rvalue)) (* TODO: peephole : inline when the only occurence is just next *)
      | `Arg _ -> true
      | `Env _ -> true (* FIXME: not so sure *)
      | `Alloc (rvt, rv) | `Lookup (rvt, rv) ->
	occurs_only_once () && droppable rvt && droppable rv
      | `Tuple (rvt, rvs) ->
	occurs_only_once () && droppable rvt && List.for_all (fun rv -> droppable rv) rvs
      | `Closure (_, _, []) -> true
      | `Closure (_, _, rvs) ->
	occurs_only_once () &&  List.for_all (fun rv -> droppable rv) rvs
      | `Field (rvo, rvf) -> false (* FIXME: do better droppable rvo && droppable rvf *)
      | `Offset_closure _ -> true
      | `Call (_, rvs) ->
	(* don't compute arithmetic twice *)
	occurs_only_once () && List.for_all (fun rv -> droppable rv) rvs
      | `Is_int rv | `Size rv | `Tag rv -> droppable rv
      | `Const (`Int _ | `Int32 _ | `Int64 _ | `Float _) -> true
      | `Const _ (* allocated value *) ->
	occurs_only_once ()
    in
    match rv with
    | `Global g -> true (* let (r,w,fr,fw) = IntMap.find g globals in w = 0 *) (* FIXME: this is WRONG *)
    | `Field (`Global g, rv) -> let (r,w,fr,fw) = IntMap.find g globals in w = 0 && fw = 0 && r = 0 && droppable rv
    | _ -> droppable rv
  in
  let eval (a,ai) (b,bi) (instr_a : vbc_instr) la =
    let result =
      match instr_a with
      | `Store (`Var _ as var, rv) when droppable (a, ai)  (b, bi) rv var ->
	let la = LvalueMap.filter (fun _ srv -> not (appears_in_rvalue srv ((=) var))) la in
	LvalueMap.add var rv la
      | `Store (`Var _ as var, _)
      | `Ext_call (`Var _ as var, _, _, _) ->
	LvalueMap.remove var (LvalueMap.filter (fun _ srv -> not (appears_in_rvalue srv ((=) var))) la)
      | `Apply _ ->
	LvalueMap.remove (`Var 0) (LvalueMap.filter (fun _ srv -> not (appears_in_rvalue srv ((=) (`Var 0)))) la)
      | `Push_trap _ when (* trap edge *) (a <> b || bi = 0) ->
	LvalueMap.remove (`Var 0) (LvalueMap.filter (fun _ srv -> not (appears_in_rvalue srv ((=) (`Var 0)))) la)
      | _ -> la
    in
    (* (* FIXME: complexity *)
    let fold result =
      LvalueMap.fold
	(fun lv rv r ->
	  let existing = LvalueMap.fold (fun lv' rv' r -> if rv = rv' then Some lv' else r) r None in
	  match existing with
	  | None -> LvalueMap.add lv rv r
	  | Some lv' -> LvalueMap.add lv (lv' :> vbc_rvalue) r)
	result
	LvalueMap.empty
    in fold *) result
  in
  let init = List.map (fun (k,_) -> (k, LvalueMap.empty)) (IntMap.bindings closures) in
  let result = Analysis.forward_analysis vbc_branches segments init eval unify LvalueMap.empty (LvalueMap.equal (=)) in
  result

(* inline assignments of the form (variable = constant) *)
let drop_simple_aliases (segments : vbc_segments) closures : vbc_segments =
  debug ~level:1 "drop-simple-aliases" "removing simple aliases" ;
  let aliases = collect_simple_aliases segments closures in
  let total_dropped = ref 0 in
  let segments =
    IntMap.mapi
      (fun ns -> map_segment (fun ni instr ->
	let aliases = try AddrMap.find (ns, ni) aliases with Not_found -> LvalueMap.empty in
	let replacer = object
	  inherit vbc_mapper as mom
	  method! map_rvalue = function
          | `Var i as var ->
	    (try
	       let res = LvalueMap.find var aliases in
	       incr total_dropped ; res
	     with Not_found -> var)
	  | any -> mom # map_rvalue any
	end in
	replacer # map_instr instr
       ))
      segments
  in
  if !total_dropped > 0 then debug "drop-simple-aliases" "dropped %d aliases" !total_dropped ;
  segments

(* inline assignments of the form (read only global = constant) *)
let drop_global_consts (segments : vbc_segments) (closures : vbc_closures) (globals : vbc_globals) : vbc_segments * vbc_globals =
  debug ~level:1 "drop-global-consts" "dropping global constants" ;
  let usage = collect_globals_usage segments closures in
  let total_dropped = ref 0 in
  let dropped = ref IntSet.empty in
  let segments =
    IntMap.mapi
      (fun ns -> map_segment (fun ni instr ->
	let replacer = object
	  inherit vbc_mapper as mom
	  method! map_rvalue = function
          | `Global i as var ->
	    (try
	       let (r,w,fr,fw) = IntMap.find i usage in
	       let (v, _) = IntMap.find i globals in
	       match v with
	       | `Const (`Int _ | `Int32 _ | `Int64 _ | `Float _) as res when w = 0 ->
		 incr total_dropped ;
		 dropped := IntSet.add i !dropped ;
		 res
	       | _ -> var
	     with Not_found -> var)
	  | any -> mom # map_rvalue any
	end in
	replacer # map_instr instr
       ))
      segments
  in
  let globals = IntSet.fold IntMap.remove !dropped globals in
  if !total_dropped > 0 then debug "drop-globals-consts" "dropped %d constants from globals" !total_dropped ;
  segments, globals

(* unbox globals (the ones either pre-allocated or written only once) *)
let unbox_globals (segments : vbc_segments) (closures : vbc_closures) (globals : vbc_globals) : vbc_segments * vbc_globals =
  debug ~level:1 "unbox-globals" "unboxing globals" ;
  let usage = collect_globals_usage segments closures in
  let unboxed = ref IntSet.empty in
  let bindings = ref GlobalFieldMap.empty in
  let uniq = ref (fst (IntMap.max_binding globals) + 1) in
  let nglobals = ref globals in
  let binding v =
    try
      GlobalFieldMap.find v !bindings
    with Not_found ->
      bindings := GlobalFieldMap.add v !uniq !bindings ;
      unboxed := IntSet.add (fst v) !unboxed ;
      nglobals := IntMap.add !uniq (`Const (`Int 0), None) !nglobals ;
      incr uniq ;
      !uniq - 1
  in
  let candidates =
    (* first case: stored once in the initial segment *)
    let eval instr old =
      match instr with
      | `Store (`Global i, `Tuple (_, _)) ->
	let (r,w,fr,fw) = IntMap.find i usage in
	if (r,w) = (0,1) then
	  IntSet.add i old
	else old
      | _ -> old
    in
    Analysis.single_analysis vbc_branches segments [0] eval IntSet.empty
    (* TODO: second case: static tuple *)
  in
  let segments =
    IntMap.mapi
      (fun ns -> rewrite_segment (fun ni instr ->
	let replacer = object (self)
	  inherit vbc_mapper as mom
	  method rewrite_instr = function
	  | `Store (`Global i, `Tuple (_, rvs)) when IntSet.mem i candidates ->
	    let n = ref (-1) in
	    List.map (fun rv -> incr n ; (`Store (`Global (binding (i, !n)), self # map_rvalue rv) :> vbc_instr)) rvs
	  | instr -> [ mom # map_instr instr ]
	  method! map_rvalue = function
          | `Field (`Global i, `Const (`Int n)) when IntSet.mem i candidates ->
	    `Global (binding (i, n))
	  | any -> mom # map_rvalue any
	end in
	replacer # rewrite_instr instr
       ))
      segments
  in
  if IntSet.cardinal !unboxed > 0 then debug "unbox-globals" "unboxed %d global(s)" (IntSet.cardinal !unboxed) ;
  segments, !nglobals

(* make static unique constant writes of globals *)
let lift_global_const_inits (segments : vbc_segments) (closures : vbc_closures) (globals : vbc_globals) : vbc_segments * vbc_globals =
  debug ~level:1 "lift-global-const-inits" "lifting global constant assignments" ;
  let rec const = function
    | `Const (`String _) -> false
    | `Const _ -> true
    | `Tuple (t, rvs) -> const t && List.for_all const rvs
    | `Closure (_, _, rvs) -> List.for_all const rvs
    | _ -> false
  in
  let candidates =
    let eval instr (collected, dissmissed) =
      match instr with
      | `Store (`Global i, v) when const v ->
	(try
	   let old_val = IntMap.find i collected in
	   if old_val <> v then
	     (IntMap.remove i collected, IntSet.add i dissmissed)
	   else
	     raise Exit
	 with Not_found | Exit ->
	   if not (IntSet.mem i dissmissed) then
	     (IntMap.add i v collected, dissmissed)
	   else
	     (collected, dissmissed))
      | _ -> (collected, dissmissed)
    in
    fst (Analysis.single_analysis vbc_branches segments (closure_entries closures) eval (IntMap.empty, IntSet.empty))
  in
  let globals = IntMap.fold (fun i v r -> IntMap.add i (v, snd (IntMap.find i globals)) r) candidates globals in
  let segments =
    IntMap.mapi
      (fun ns -> rewrite_segment (fun ni -> function `Store (`Global i, _) when IntMap.mem i candidates -> [] | instr -> [ instr ]))
      segments
  in
  if IntMap.cardinal candidates > 0 then debug "lift-global-const-inits" "lifted %d global assignments" (IntMap.cardinal candidates);
  segments, globals

(* remove globals that are never read (except from predefined exceptions)  *)
let remove_useless_globals (segments : vbc_segments) (closures : vbc_closures) (globals : vbc_globals) : vbc_globals =
  debug ~level:1 "remove-useless-globals" "removing useless globals" ;
  let usage = collect_globals_usage segments closures in
  let nglobals = IntMap.fold (fun i f r -> if f <= (0,0,0,0) && i > 11 then r else IntMap.add i (IntMap.find i globals) r) usage IntMap.empty in
  let total_removed = IntMap.cardinal nglobals - IntMap.cardinal globals in
  if total_removed > 0 then debug "remove-useless-globals" "removed %d useless globals" total_removed ;
  nglobals

(* remove assignments of the form (variable with zero occ = pure expression) *)
let remove_useless_assignments (segments : vbc_segments) (closures : vbc_closures) (globals : vbc_globals) : vbc_segments =
  debug ~level:1 "remove-useless-assignments" "removing useless assignments" ;
  let usage = collect_globals_usage segments closures in
  let liveness = collect_alive_variables segments closures in
  let total_globals_removed = ref 0 in
  let total_locals_removed = ref 0 in
  let rec removable (rv : vbc_rvalue) =
    (* keep in sync with collect_simple_aliases in order not to make coputations twice ! *)
    match rv with
    | `Call (`External prim, rvs) -> StringSet.mem prim pure_externals && List.for_all removable rvs
    | `Offset_closure _ | `Var _ | `Arg _ | `Env _ | `Const _ | `Global _ -> true
    | `Alloc (rv1, rv2) | `Lookup (rv1, rv2) | `Field (rv1, rv2) ->
      removable rv1 && removable rv2
    | `Tuple (rv, rvs) ->
      removable rv && List.for_all removable rvs
    | `Call (_, rvs) | `Closure (_, _, rvs) ->
      List.for_all removable rvs
    | `Is_int rv | `Size rv | `Tag rv ->
      removable rv
  in
  let segments =
    IntMap.mapi
      (fun ns -> rewrite_segment (fun ni instr ->
	match instr with
	| `Store (`Global i, rv) when removable rv ->
	  (try
	     let (r,w,fr,fw) = IntMap.find i usage in
	     if r = 0 && fr = 0 then
	       (incr total_globals_removed ; [])
	     else
	       [ instr ]
	   with Not_found -> incr total_globals_removed ; [])
	| `Store (`Var i, rv) when removable rv -> 
	  (try
	     if IntSet.mem i (AddrMap.find (ns, ni) liveness) then
	       [ instr ]
	     else
	       (incr total_locals_removed ; [])
	   with Not_found -> incr total_locals_removed ; [])
	| `Apply (`Closure (_, arity, env), args, ret) when List.for_all removable env && List.for_all removable args && List.length args < arity ->
	  (* useless partial application *)
	  (try
	     if IntSet.mem 0 (AddrMap.find (ns, ni) liveness) then
	       [ instr ]
	     else
	       (incr total_locals_removed ; [ `Branch (`Const (`Int 1), ret) ])
	   with Not_found -> incr total_locals_removed ; [])
	| _ -> [ instr ]))
      segments
  in
  if !total_globals_removed > 0 then debug "remove-useless-assignments" "removed %d global assignments" !total_globals_removed ;
  if !total_locals_removed > 0 then debug "remove-useless-assignments" "removed %d local assignments" !total_locals_removed ;
  segments

(* simple dead code elimination : every function entry which is not put in a closure or reachable from such a one is removed *)
let simple_dead_code_removal (segments : vbc_segments) (closures : vbc_closures) (globals : vbc_globals) : vbc_segments * vbc_closures =
  debug ~level:1 "simple-dead-code-removal" "removing dead code" ;
  (* TODO: implement this correctly... *)
  let alive = ref (IntSet.singleton 0) in
  let collector = object (self)
    inherit vbc_iterator as mom
    (*method! iter_instr instr =
      List.iter (fun addr -> alive := IntSet.add addr !alive) (vbc_branches instr)*)
    method! iter_rvalue rv =
      match rv with
      | `Closure (addr, _, rvs) ->
	alive := IntSet.add addr !alive ;
	List.iter self # iter_rvalue rvs
      | `Offset_closure (addr, _) ->
	alive := IntSet.add addr !alive
      | _ -> mom # iter_rvalue rv
  end in
  IntMap.iter (fun _ (v, _) -> collector # iter_rvalue v) globals ;
  let rec iter () =
    let old = !alive in
    let eval instr () = collector # iter_instr instr in
    Analysis.single_analysis vbc_branches segments (IntSet.elements old) eval () ;
    if !alive <> old then iter ()
  in
  iter () ;
  let alive_segments =
    let addrs = snd (Analysis.build_forward_cfg vbc_branches segments (IntSet.elements !alive)) in
    IntSet.fold (fun s r -> IntMap.add s (IntMap.find s segments) r) addrs IntMap.empty
  in
  let alive_closures = IntSet.fold (fun s r -> IntMap.add s (IntMap.find s closures) r) !alive IntMap.empty in
  let total_closures_removed = IntMap.cardinal closures - IntMap.cardinal alive_closures in
  if total_closures_removed > 0 then debug "simple-dead-code-removal" "removed %d unused functions" total_closures_removed ;
  let total_segments_removed = IntMap.cardinal segments - IntMap.cardinal alive_segments in
  if total_segments_removed > 0 then debug "simple-dead-code-removal" "removed %d unused segments" total_segments_removed ;
  alive_segments, alive_closures

(* bypass and remove segments that only contain an unconditional jump *)
let bypass_silly_jumps (segments : vbc_segments) (closures : vbc_closures) (globals : vbc_globals) : vbc_segments * vbc_closures * vbc_globals =
  debug ~level:1 "bypass-silly-jumps" "bypassing silly jumps" ;
  let to_bypass =
    IntMap.fold
      (fun src s r ->
	match s with
	| [| `Branch (`Const (` Int n), dst) |] when n <> 0 && src <> 0 ->
	    (* TODO: enable bypass zero *)
	    IntMap.add src (try IntMap.find dst r with Not_found -> dst) r
	| _ -> r)
      segments IntMap.empty
  in
  let relocator = object
    inherit vbc_mapper
    method! map_addr addr = try IntMap.find addr to_bypass with Not_found -> addr
  end in
  let closures = IntMap.fold (fun s i r -> IntMap.add (try IntMap.find s to_bypass with Not_found -> s) i r) closures IntMap.empty in
  let segments = IntMap.fold (fun s body r -> IntMap.add s (Array.map relocator # map_instr body) r) segments IntMap.empty in
  let globals = IntMap.fold (fun i (v, n) r -> IntMap.add i (relocator # map_rvalue v, n) r) globals IntMap.empty in
  segments, closures, globals

(* replace an unconditional jump to a segment used only once by its contents *)
let join_segments (segments : vbc_segments) (closures : vbc_closures) : vbc_segments =
  debug ~level:1 "join-segments" "removing unique, unconditional jumps" ;
  let rcfg, _ = Analysis.build_backward_cfg vbc_branches segments (closure_entries closures) in
  let total_joined = ref 0 in
  let trivial =
    IntMap.map
      (fun s ->
	match s with
	| [| `Return (`Const _ | `Var _ | `Env _ | `Arg _) |]
	| [| `Raise (`Const _ | `Var _ | `Env _ | `Arg _) |] -> true
	| _ -> false)
      segments
  in
  let segments =
    IntMap.fold
      (fun s body r ->
	match body.(Array.length body - 1) with
	| `Branch (`Const (`Int n), addr) when n <> 0 && (try IntMap.find addr trivial || AddrSet.cardinal (IntMap.find addr rcfg) = 1 with Not_found ->  false) ->
	  incr total_joined ;
	  IntMap.add s (Array.concat [ Array.sub body 0 ((Array.length body - 1)) ; IntMap.find addr segments ]) r
	| _ -> IntMap.add s body r)
      segments IntMap.empty
  in
  if !total_joined > 0 then debug "join-segments" "joined %d segments" !total_joined ;
  segments

(* simple partial evaluation (arithmetics, constant jumps, etc.) *)
let partial_evaluation (segments : vbc_segments) : vbc_segments =
  let arith31 x = (x land 0x7FFFFFFF) lor (if x < 0 then lnot 0x7FFFFFFF else 0) in
  let ult a b = if (a >= 0) then ((b < 0) || (a < b)) else ((b < 0) && (a < b)) in
  let int_of_bool = function true -> 1 | false -> 0 in
  let evaluator = object (self)
    inherit vbc_mapper as mom
    val mutable generated = []
    method! map_rvalue rv =
      let rv = mom # map_rvalue rv in
      match rv with
      (* precompute *)
      | `Call (`Neg, [`Const (`Int n)]) -> `Const (`Int (arith31 (-n)))
      | `Call (`Not, [`Const (`Int 0)]) -> `Const (`Int 1)
      | `Call (`Not, [`Const (`Int n)]) -> `Const (`Int 0)
      | `Call (`Add, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (arith31 (n1 + n2)))
      | `Call (`Sub, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (arith31 (n1 - n2)))
      | `Call (`Mul, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (arith31 (n1 * n2)))
      | `Call (`Div, [`Const (`Int n1) ; `Const (`Int n2)]) when n2 <> 0 -> `Const (`Int (arith31 (n1 / n2)))
      | `Call (`Mod, [`Const (`Int n1) ; `Const (`Int n2)]) when n2 > 0 -> `Const (`Int (arith31 (n1 mod n2)))
      | `Call (`And, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (arith31 (n1 land n2)))
      | `Call (`Or, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (arith31 (n1 lor n2)))
      | `Call (`Xor, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (arith31 (n1 lxor n2)))
      | `Call (`Lsl, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (arith31 (n1 lsl n2)))
      | `Call (`Lsr, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (arith31 (n1 lsr n2)))
      | `Call (`Asr, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (arith31 (n1 asr n2)))
      | `Call (`Eq, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (int_of_bool (n1 = n2)))
      | `Call (`Neq, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (int_of_bool (n1 <> n2)))
      | `Call (`Lt, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (int_of_bool (n1 < n2)))
      | `Call (`Le, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (int_of_bool (n1 <= n2)))
      | `Call (`Gt, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (int_of_bool (n1 > n2)))
      | `Call (`Ge, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (int_of_bool (n1 >= n2)))
      | `Call (`Ult, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (int_of_bool (ult n1 n2)))
      | `Call (`Uge, [`Const (`Int n1) ; `Const (`Int n2)]) -> `Const (`Int (int_of_bool (not (ult n1 n2))))
      | `Is_int (`Const (`Int _)) -> `Const (`Int 1)
      | `Is_int (`Const (`Float _ | `String _ ) | `Alloc _ | `Tuple _ ) -> `Const (`Int 0)
      (* simplify *)
      | `Call (`Add, [ v ; `Const (`Int n2)]) when n2 < 0 -> `Call (`Sub, [ v ; `Const (`Int (-n2))])
      | `Call (`Sub, [ v ; `Const (`Int n2)]) when n2 < 0 -> `Call (`Add, [ v ; `Const (`Int (-n2))])
      | `Call (`External prim, args) ->
	let lv = gen_var () in
	(try generated <- generated @ eval_external_rvalue prim args lv ; lv with Not_found -> rv)
      | rv -> rv
    method rewrite_instr _ instr =
      generated <- [] ;
      let res =
	match self # map_instr instr with
	| `Ext_call (lv, prim, args, addr) ->
	  (try eval_external_rvalue prim args lv @ [ `Branch (`Const (`Int 1), addr) ] with Not_found -> [ instr ])
	| `Branch (`Const (`Int 0), _) -> []
	| instr -> [ instr ]
      in
      let res = generated @ res in
      generated <- [] ; res
	  
  end in
  let segments = IntMap.fold (fun s body r -> IntMap.add s (rewrite_segment evaluator # rewrite_instr body) r) segments IntMap.empty in
  segments

(* inline external calls whenever possible *)
let inline_externals (segments : vbc_segments) : vbc_segments =
  debug ~level:1 "inline-externals" "inlining non asynchronous external calls" ;
  let total_inlined = ref 0 in
  let segments =
    IntMap.fold
      (fun s body r ->
	match body.(Array.length body - 1) with
	| `Ext_call (lv, name, args, addr) when StringSet.mem name inlinable_externals ->
	  incr total_inlined ;
	  IntMap.add s (Array.concat [ Array.sub body 0 ((Array.length body - 1)) ; [| `Store (lv, `Call (`External name, args)) ; `Branch (`Const (`Int 1), addr) |] ]) r
	| _ -> IntMap.add s body r)
      segments IntMap.empty
  in
  if !total_inlined > 0 then debug "inline-externals" "inlined %d external calls" !total_inlined ;
  segments

(* remove porky uses of accu *)
let peephole_accu_removal (segments : vbc_segments) : vbc_segments =
  debug ~level:1 "peephole-accu-removal" "removing useless temporary uses of accu" ;
  let segments =
    IntMap.fold
      (fun s body r ->
	let body = Array.to_list body in
	let inliner = object (self)
	  inherit vbc_mapper as mom
	  val mutable dest : vbc_rvalue = `Var 0
	  val mutable nb = 0
	  method! map_rvalue (rv : vbc_rvalue) =
	    match rv with
	    | `Var 0 -> if nb = 0 then raise Exit else (nb <- nb - 1 ; dest)
	    | rv -> mom # map_rvalue rv
	  method process ?(max = 1 (* FIXME: ugly *)) rv d =
	    dest <- d ;
	    nb <- max ;
	    self # map_rvalue rv
	end in 
	let rec treat (seg : vbc_instr list) : vbc_instr list =
	  match seg with
	  | `Store (`Var 0, term) :: `Store (`Var v, `Var 0) :: `Store (`Var 0, term2) :: tl when v <> 0 ->
	    `Store (`Var v, term) :: treat (`Store (`Var 0, inliner # process ~max:10000 term2 (`Var v)) :: tl)
	  | `Store (`Var 0, term) as e1 :: (`Store (`Var 0, term') as e2) :: tl when appears_in_rvalue term' ((=) (`Var 0)) ->
	    (try treat (`Store (`Var 0, inliner # process term' term) :: tl) with Exit -> e1 :: treat (e2 :: tl)) 
	  | i :: tl -> i :: treat tl
	  | [] -> []
	in
	IntMap.add s (Array.of_list (treat body)) r)
      segments IntMap.empty
  in
  segments


(* narow all ids of the program *)
let narrow (segments : vbc_segments) (closures : vbc_closures) (globals : vbc_globals) : vbc_segments * vbc_closures * vbc_globals =
  let uniq s = let cpt = ref (s-1) in fun () -> incr cpt ; !cpt in
  (* compute relocation information *)
  let alloc_global = uniq 0 in
  let globals_reloc = IntMap.fold (fun i _ r -> IntMap.add i (alloc_global ()) r) globals IntMap.empty in
  let alloc_segment = uniq 0 in
  let segments_reloc = IntMap.fold (fun i _ r -> IntMap.add i (alloc_segment ()) r) segments IntMap.empty in
  let nsegments = ref IntMap.empty in
  let reloc_closure head =
    let segs = snd (Analysis.build_forward_cfg vbc_branches segments [head]) in
    let alloc_local = uniq 1 in
    let locals_reloc = ref (IntMap.singleton 0 0) in
    let reloc_local n = try IntMap.find n !locals_reloc with Not_found -> let n' = alloc_local () in locals_reloc := IntMap.add n n' !locals_reloc ; n' in
    let relocator = object
      inherit vbc_mapper
      method! map_addr a = IntMap.find a segments_reloc
      method! map_slvalue = function
      | `Var n -> `Var (reloc_local n)
      | `Arg _ | `Env _ as v -> v
      | `Global n -> `Global (IntMap.find n globals_reloc)
    end in
    IntSet.iter
      (fun i -> nsegments := IntMap.add (IntMap.find i segments_reloc) (Array.map relocator # map_instr (IntMap.find i segments)) !nsegments) 
      segs
  in
  let nclosures = IntMap.fold (fun head ar r -> reloc_closure head ; IntMap.add (IntMap.find head segments_reloc) ar r) closures IntMap.empty in
  let nglobals =
    let relocator = object
      inherit vbc_mapper
      method! map_addr a = IntMap.find a segments_reloc
      method! map_slvalue = function
      | `Global n -> `Global (IntMap.find n globals_reloc)
      | rv -> rv
    end in
    IntMap.fold (fun i (v, n) r -> IntMap.add (IntMap.find i globals_reloc) (relocator # map_rvalue v, n) r) globals IntMap.empty
  in
  !nsegments, nclosures, nglobals

(* prototype value analysis *)
module Value_analysis = struct
  type abstract_value =
  [ `Top
  | `Block of abstract_int * [ `Size of abstract_int | `Values of abstract_value array ]
  | `Closure of [ `Top | `Addr of int ] * [ `Top | `Env of abstract_value list ]
  | `Const of abstract_int
  | `Bot ]
  and abstract_int =
  [ `Top
  | `Int of int * int
  | `Low of int
  | `High of int
  | `Bot ]

  let string_of_abstract_int = function
    | `Top -> "⊤"
    | `Bot -> "⊥"
    | `Int (l, r) when l = r -> string_of_int l
    | `Int (l, r) -> "[" ^ string_of_int l ^ "," ^ string_of_int r ^ "]"
    | `Low r -> "]-oo," ^ string_of_int r ^ "]"
    | `High l -> "[" ^ string_of_int l ^ ",+oo["

  let rec string_of_abstract_value = function
    | `Top -> "⊤"
    | `Bot -> "⊥"
    | `Const i -> string_of_abstract_int i
    | `Block (t, `Size i) -> "alloc (" ^ string_of_abstract_int t ^ "," ^ string_of_abstract_int i ^ ")"
    | `Block (t, `Values ar) -> "tuple (" ^ string_of_abstract_int t ^ ",[" ^ sprintf_array "%s" "," (Array.map string_of_abstract_value ar) ^ "]"
    | `Closure _ -> "closure"

  let meet_int i1 i2 =
    match i1, i2 with
    | `Top, x | x, `Top -> x
    | `High l, `Low r | `Low r, `High l -> if r <= l then `Int (r, l) else `Bot
    | `Int (l1, r1), `High l | `High l, `Int (l1, r1) -> if l > r1 then `Bot else `Int (max l l1, r1)
    | `Int (l1, r1), `Low r | `Low r, `Int (l1, r1) -> if r < l1 then `Bot else `Int (l1, min r r1)
    | `Int (l1, r1), `Int (l2, r2) -> let nr = min r1 r2 and nl = max l1 l2 in if nr < nl then `Bot else `Int (nl, nr)
    | _, _ -> `Bot

  let join_int i1 i2 =
    match i1, i2 with
    | `Bot, x | x, `Bot -> x
    | `High l, `Low r | `Low r, `High l -> `Top
    | `Int (l1, r1), `High l | `High l, `Int (l1, r1) -> `High (min l l1)
    | `Int (l1, r1), `Low r | `Low r, `Int (l1, r1) -> `Low (max r r1)
    | `Int (l1, r1), `Int (l2, r2) -> `Int (min l1 l2, max r1 r2)
    | _, _ -> `Top

  let add_int i1 i2 =
    match i1, i2 with
    | `Bot, x | x, `Bot -> `Bot
    | `Int (l1, r1), `Int (l2, r2) -> `Int (l1 + l2, r1 + r2)
    | _, _ -> `Top

  let neg_int i =
    match i with
    | `Bot -> `Bot
    | `Int (l, r) -> `Int (-r, -l)
    | `Top -> `Top

  let sub_int i1 i2 =
    add_int i1 (neg_int i2)

  let rec meet_abstract_value ?(max_depth = 2) v1 v2 =
    let rec meet_rec depth v1 v2 =
      if depth = max_depth then
	`Bot
      else
	match v1, v2 with
	| `Top, x | x, `Top -> x
	| `Block (tag1, ctns1), `Block (tag2, ctns2) ->
	  `Block (meet_int tag1 tag2,
		  (match ctns1, ctns2 with
		   | `Size (`Int (l, r)), `Values ar |  `Values ar, `Size (`Int (l, r)) ->
		     if let len = Array.length ar in l <= len && len <= r then `Values ar else `Size `Bot
		   | `Values ar1, `Values ar2 ->
		     if Array.length ar1 = Array.length ar2 then
		       `Values (Array.mapi (fun i l -> meet_rec (depth + 1) l ar2.(i)) ar1)
		     else `Size `Bot
		   | `Size s1, `Size s2 -> `Size (meet_int s1 s2)
		   | _, _ ->`Size `Bot))
	| `Closure (`Top, `Top), `Closure (x, y) -> `Closure (x, y)
	| `Const (x), `Const (y) -> `Const (meet_int x y)
	| _, _ -> `Bot
    in meet_rec 0 v1 v2

  let rec join_abstract_value ?(max_depth = 2) v1 v2 =
    let rec join_rec depth v1 v2 =
      if depth = max_depth then
	`Top
      else
	match v1, v2 with
	| `Bot, x | x, `Bot -> x
	| `Block (tag1, ctns1), `Block (tag2, ctns2) ->
	  `Block (join_int tag1 tag2,
		  (match ctns1, ctns2 with
		   | `Size (`Int (l, r)), `Values ar |  `Values ar, `Size (`Int (l, r)) ->
		     let len = Array.length ar in
		     `Size (`Int (min l len, max r len))
		   | `Values ar1, `Values ar2 ->
		     let len1 = Array.length ar1 and len2 = Array.length ar2 in
		     if len1 = len2 then
		       `Values (Array.mapi (fun i l -> join_rec (depth + 1) l ar2.(i)) ar1)
		     else `Size (`Int (min len1 len2, max len1 len2))
		   | `Size s1, `Size s2 -> `Size (join_int s1 s2)
		   | _, _ ->`Size `Top))
	| `Closure (`Top, `Top), `Closure (x, y) -> `Closure (x, y)
	| `Const (x), `Const (y) -> `Const (join_int x y)
	| _, _ -> `Top
    in join_rec 0 v1 v2

  let unify v1 v2 =
    IntMap.merge
      (fun v o1 o2 ->
	match o1, o2 with
	| None, x | x, None -> x
	| Some v1, Some v2 -> Some (join_abstract_value v1 v2))
      v1 v2

  let eval (a,ai) (b,bi) (instr_a : vbc_instr) la =
    let merge v n la =
      try
	let o = IntMap.find v la in
	let nv = meet_abstract_value n o in
	if nv = `Top then
	  IntMap.remove v la
	else
	  IntMap.add v nv la
      with
	Not_found -> IntMap.add v n la
    in
    let branching = a <> b || bi = 0 in
    match instr_a with
    | `Store (`Var v, `Const (`Int i)) ->
      merge v (`Const (`Int (i, i))) la
    (* narrow a variable value after a test *)
    | `Branch (`Call (`Eq, [`Var v ; `Const (`Int i)]), addr) when branching ->
      merge v (`Const (`Int (i, i))) la
    | `Branch (`Call (`Eq, [`Var v ; `Const (`Int i)]), addr) ->
      la
    | `Branch (`Call (`Neq, [`Var v ; `Const (`Int i)]), addr) when branching ->
      la
    | `Branch (`Call (`Neq, [`Var v ; `Const (`Int i)]), addr) ->
      merge v (`Const (`Int (i, i))) la
    | `Branch ((`Call (`Lt, [`Var v ; `Const (`Int i)]) | `Call (`Ge, [`Const (`Int i) ; `Var v])), addr) when branching ->
      merge v (`Const (`Low (i - 1))) la
    | `Branch ((`Call (`Lt, [`Var v ; `Const (`Int i)]) | `Call (`Ge, [`Const (`Int i) ; `Var v])), addr) ->
      merge v (`Const (`High i)) la
    | `Branch ((`Call (`Gt, [`Var v ; `Const (`Int i)]) | `Call (`Le, [`Const (`Int i) ; `Var v])), addr) when branching ->
      merge v (`Const (`High (i + 1))) la
    | `Branch ((`Call (`Gt, [`Var v ; `Const (`Int i)]) | `Call (`Le, [`Const (`Int i) ; `Var v])), addr) ->
      merge v (`Const (`Low i)) la
    | `Branch ((`Call (`Le, [`Var v ; `Const (`Int i)]) | `Call (`Gt, [`Const (`Int i) ; `Var v])), addr) when branching ->
      merge v (`Const (`Low i)) la
    | `Branch ((`Call (`Le, [`Var v ; `Const (`Int i)]) | `Call (`Gt, [`Const (`Int i) ; `Var v])), addr) ->
      merge v (`Const (`High (i + 1))) la
    | `Branch ((`Call (`Ge, [`Var v ; `Const (`Int i)]) | `Call (`Lt, [`Const (`Int i) ; `Var v])), addr) when branching ->
      merge v (`Const (`High i)) la
    | `Branch ((`Call (`Ge, [`Var v ; `Const (`Int i)]) | `Call (`Lt, [`Const (`Int i) ; `Var v])), addr) ->
      merge v (`Const (`Low (i - 1))) la

    (* narrow thanks to a switch *)
    | `Switch (`Var v, li, lb) ->
      let approx_i = fst (List.fold_left (fun (r, i) addr -> (if addr = b then join_int (`Int (i, i)) r else r), i + 1) (`Bot, 0) li) in
      let approx_b = fst (List.fold_left (fun (r, i) addr -> (if addr = b then join_int (`Int (i, i)) r else r), i + 1) (`Bot, 0) lb) in
      let approx =
	match approx_i, approx_b with
	| `Bot, `Bot ->  error "value-analysis" "bad switch"
	| `Int r, `Bot -> `Const (`Int r)
	| `Bot, `Int r -> `Block (`Int r, `Size `Top)
	| _, _ -> `Top
      in 
      merge v approx la

    | `Store (`Var v, `Var v2) ->
      (try IntMap.add v (IntMap.find v2 la) la with Not_found -> la)
    | `Store (`Var v, _) ->
      IntMap.remove v la
    | `Ext_call (`Var v, _, _, _) ->
      IntMap.remove v la
    | `Apply _ ->
      IntMap.remove 0 la
    | `Push_trap _ when branching ->
      IntMap.add 0 (`Block (`Top, `Size `Top)) la
    | _ -> la
      
  let collect (segments : vbc_segments) (closures : vbc_closures) : abstract_value IntMap.t AddrMap.t =
    let init = List.map (fun (k,_) -> (k, IntMap.empty)) (IntMap.bindings closures) in
    let result = Analysis.forward_analysis vbc_branches segments init eval unify IntMap.empty (IntMap.equal (=)) in
    result

end
