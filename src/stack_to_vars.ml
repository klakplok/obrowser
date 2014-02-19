open Printf
open Types
open Messages
open Utils

let decompose_into_basic_blocks code =
  debug ~level:1 "basic-blocks" "computing breaking spots" ;
  let targets = ref (IntSet.singleton 0) in
  for i = 0 to Array.length code - 1 do
    match code.(i) with
      | `Closure_rec (_, al) -> 
	targets := List.fold_right IntSet.add al !targets
      | `Switch (ai, at) ->
	targets := List.fold_right IntSet.add (ai @ at) !targets
      | `Apply _ | `App_term _ | `Return _ | `Raise | `Ext_call _ ->
	targets := IntSet.add (i + 1) !targets
      | `Branch a -> 
	targets := IntSet.add a !targets
      | `Grab (_, a) ->
	targets := IntSet.add a !targets
      | `Push_ret_addr a | `Closure (_, a)
      | `Branch_if a | `Branch_if_not a | `Push_trap a
      | `Branch_cmpop (_, _, a) ->
	targets := IntSet.add a !targets
      | e -> ()
  done ;
  for i = 0 to Array.length code - 2 do
    match code.(i) with
      | `Branch _ | `Raise | `Return _ | `Stop | `App_term _ | `Switch _ -> 
	targets := IntSet.add (i + 1) !targets
      | _ -> ()
  done ;
  let rec build_reloc_info l n =
    match l with
    | [] -> IntMap.empty
    | [a] -> IntMap.singleton a (n, Array.length code - 1)
    | a :: (b :: _ as l) -> IntMap.add a (n, b - 1) (build_reloc_info l (n + 1))
  in
  let reloc_info = build_reloc_info (IntSet.elements !targets) 0 in
  let reloc a =
    try fst (IntMap.find a reloc_info)
    with Not_found -> error "basic-blocks" "target not at beginning of a basic block"
  in
  debug ~level:1 "basic-blocks" "building basic segments" ;
  IntMap.fold
    (fun s (i,e) res ->
      let seg = Array.make (e - s + 1) `Stop in
      for i = s to e do
	seg.(i - s) <- match code.(i) with
	  | `Grab (i, a) -> `Grab (i, reloc a)
	  | `Push_ret_addr a -> `Push_ret_addr (reloc a)
	  | `Closure (n, a) -> `Closure (n, reloc a)
	  | `Closure_rec (n, al) -> `Closure_rec (n, List.map reloc al)
	  | `Switch (ai, at) -> `Switch (List.map reloc ai, List.map reloc at)
	  | `Branch a -> `Branch (reloc a)
	  | `Branch_if a -> `Branch_if (reloc a)
	  | `Branch_if_not a -> `Branch_if_not (reloc a)
	  | `Push_trap a -> `Push_trap (reloc a)
	  | `Branch_cmpop (op, i, a) -> `Branch_cmpop (op, i, reloc a)
	  | i -> i
      done ;
      (* insert fake unconditional jumps to branch to next segment  *)
      let seg' =
	match seg.(Array.length seg - 1) with
	| `Branch _ | `Raise | `Return _
	| `Stop | `App_term _
	| `Switch _ -> seg
	| _ -> Array.concat [ seg ; [| `Branch (i + 1) |] ]
      in
      IntMap.add i seg' res)
    reloc_info
    IntMap.empty

let retrieve_closures segments =
  debug ~level:1 "retrieve-structure" "computing function entry points" ;
  let groups = ref [] in
  let rec collect_in_seg filter seg =
    Array.iter (filter (succ seg)) (IntMap.find seg segments)
  in
  let collect_from filter n =
    let collected = ref (IntSet.singleton n) in
    let newly = ref (IntSet.singleton n) in
    let add_new n =
      if not (IntSet.mem n !collected) then (
	collected := IntSet.add n !collected ;
	newly := IntSet.add n !newly
      )
    in
    while not (IntSet.is_empty !newly) do
      let pred = !newly in
      newly := IntSet.empty ;
      IntSet.iter (fun l -> collect_in_seg (filter add_new) l) pred
    done ;
    !collected
  in
  let filter_jumps callback def = function
    | `Branch a
    | `Branch_if a
    | `Branch_if_not a
    | `Push_trap a
    | `Branch_cmpop (_, _, a)
    | `Push_ret_addr a -> callback a
    | `Switch (ai, at) -> List.iter callback ai ; List.iter callback at
    | `Ext_call _
    | `Apply _ -> callback def
    | _ -> ()
  in
  let filter_closures callback def = function
    | `Closure_rec (_, al) -> groups := al :: !groups ; List.iter callback al
    | `Closure (_, a) -> callback a
    | `Grab (_, a) -> ( (* don't capture restarters *) )
    | _ -> ()
  in
  let filter_both callback def instr =
    filter_closures callback def instr ;
    filter_jumps callback def instr
  in
  let arity seg =
    match (IntMap.find seg segments).(0) with
    | `Grab (n, _) -> n + 1
    | i -> 1
  in
  let active_segments = collect_from filter_both 0 in
  debug ~level:1 "recon-struct" "found %d active sergments" (IntSet.cardinal active_segments) ;
  let closure_addresses = let res = ref IntSet.empty in IntSet.iter (collect_in_seg (filter_closures (fun i -> res := IntSet.add i !res))) active_segments ; !res in
  let closures = IntSet.fold (fun addr res -> IntMap.add addr (collect_from filter_jumps addr, arity addr) res) closure_addresses IntMap.empty in
  let closures = IntMap.add 0 (collect_from filter_jumps 0, 0) closures in
  debug "retrieve-structure" "found %d function(s)" (IntMap.cardinal closures) ;
  let offsets =
    List.fold_left
      (fun r arr ->
	let res = ref r in
	let len = List.length arr in
	iteri
	  (fun i addr ->
	    let map = ref IntMap.empty in
	    iteri (fun j v -> map := IntMap.add (j - i) v !map) arr ;
	    res := IntSet.fold (fun s r -> IntMap.add s (!map, 1 + 2 * (len - i - 1)) r) (fst (IntMap.find addr closures)) !res)
	  arr ;
	!res)
      IntMap.empty
      !groups
  in
  closures, offsets

let remove_dead_segments segments closures =
  debug ~level:1 "remove-dead-segments" "initial dead segments removal" ;
  let segments' =
    IntMap.fold
      (fun seg (segs, _) res ->	IntSet.fold (fun seg res -> IntMap.add seg (IntMap.find seg segments) res) segs res)
      closures
      IntMap.empty
  in
  let osz = IntMap.cardinal segments in
  let nsz = IntMap.cardinal segments' in
  if nsz <> osz then
    debug ~level:1 "remove-dead-segments" "removed %d segments that were out of functions" (osz - nsz) ;
  segments'

let infer_variables (segments : bc_segment IntMap.t) closures =
  debug ~level:1 "infer-vars" "infering variables from stack levels" ;
  let branches (instr : bc_instr) =
    match instr with
    | `Branch a | `Branch_if a | `Branch_if_not a | `Push_trap a
    | `Branch_cmpop (_, _, a) | `Push_ret_addr a -> [a]
    | `Switch (ai, at) -> ai @ at
    | _ -> []
  in
  let unify v1 v2 =
    match v1, v2 with
    | None, None -> None
    | Some v, None | None, Some v -> Some v
    | Some (l1, s1), Some (l2, s2) ->
      if s1 != s2 then error "stack-to-vars" "sharing of basic blocks between closures is unsupported" ;
      let rec unify_lists l1 l2 =
	match l1, l2 with
	| [], [] -> []
	| r1 :: tl1, r2 :: tl2 ->
	  if !r1 <> !r2 then
	    let min = min !r1 !r2 and max = max !r1 !r2 in
	    r1 := min ;
	    r2 := min ;
	    s1 := List.sort compare (max :: !s1) ;
	    r1 :: unify_lists tl1 tl2
	  else
	    r1 :: unify_lists tl1 tl2
	| _, _ ->  error "stack-to-vars" "bad stack levels"
      in Some (unify_lists l1 l2, s1)
  in
  let eq  v1 v2 =
    match v1, v2 with
    | None, None -> false
    | Some v, None | None, Some v -> false
    | Some (l1, s1), Some (l2, s2) -> l1 = l2
  in
  let reservations = ref AddrMap.empty in
  let reserve a st =
    match st with
    | None -> None (* assert false *)
    | Some (l, st) ->
      let v, nst = 
	match !st with
	| [] -> assert false
	| [v] -> v, [v + 1]
	| v :: tl -> v, tl
      in
      let rr = ref v in
      st := nst ;
      (try
	 reservations := AddrMap.add a (rr :: AddrMap.find a !reservations) !reservations
       with Not_found ->
	 reservations := AddrMap.add a [rr] !reservations) ;
      Some (rr :: l, st)
  in
  let drop st =
    match st with
    | None -> assert false
    | Some (l, st) ->
      match l with
      | [] -> assert false
      | _ :: tl -> Some (tl, st)
  in
  let rec x n f v = if n = 0 then v  else x (n - 1) f (f v) in
  let eval (a,ai) (b,bi) (instr_a : bc_instr) la =
    match instr_a with
    | `App_term _ | `Acc _ | `Assign _ | `Grab (_, _)
    | `Closure (0, _) | `Return _ | `Get_global _
    | `Set_global _  | `Get_field _ | `Get_float_field _
    | `Offset_closure _ | `Atom _ | `Vect_length | `Branch _
    | `Branch_if _ | `Branch_if_not _ | `Branch_cmpop _
    | `Offset_int _ | `Offset_ref _ | `Is_int | `Const _ | `Neg
    | `Switch _ | `Bool_not | `Raise | `Check_signals
    | `Stop | `Event | `Break | `Env_acc _ -> la
    | `Push -> (reserve (a, ai)) la
    | `Pop n -> x n drop la
    | `Push_ret_addr addr ->  if a = b && bi = ai + 1 then x 3 (reserve (a, ai)) la else la
    | `Apply (1 | 2 | 3 as n) -> x n drop la
    | `Apply n -> x (n + 3) drop la
    | `Restart -> error "stack-to-vars" "RESTART in the middle of closure"
    | `Closure (n, _) -> x (n - 1) drop la
    | `Closure_rec (0, l) -> x (List.length l) (reserve (a, ai)) la
    | `Closure_rec (n, l) -> x (List.length l) (reserve (a, ai)) (x (n - 1) drop la)
    | `Make_block (s, _)
    | `Make_float_block s -> if s >= 1 then x (s - 1) drop la else la
    | `Set_field _
    | `Set_float_field _ -> drop la
    | `Get_vect_item -> drop la
    | `Set_vect_item -> x 2 drop la
    | `Get_char -> drop la
    | `Set_char -> x 2 drop la
    | `Push_trap addr -> if a = b && bi = ai + 1 then x 4 (reserve (a, ai)) la else la
    | `Pop_trap -> x 4 drop la
    | `Ext_call (n, _) -> x (n - 1) drop la
    | `Binop op -> drop la
    | `Cmpop op -> drop la
    | `Get_method -> la
    | `Get_pub_met _ -> reserve (a, ai) la
    | `Get_dyn_met -> la
  in
  let init = List.map (fun (k,_) -> (k, Some ([], ref [1]))) (IntMap.bindings closures) in
  let result = Analysis.forward_analysis branches segments init eval unify None eq in
  AddrMap.merge
    (fun _ rl res ->
      Some
	((match rl with
  	| None -> []
  	| Some (rrl) -> List.map (fun r -> !r) rrl),
	 (match res with
	 | None | Some None -> []
  	 | Some (Some (rrl, _)) -> List.map (fun r -> !r) rrl)))
    !reservations
    result

let stack_to_vars_and_args (segments : bc_segment IntMap.t) closures offsets : vbc_segment IntMap.t =
  debug ~level:1 "stack-to-vars" "converting stack based bytecord to variable based representation" ;
  let variables = infer_variables segments closures in
  let arity addr = snd (IntMap.find addr closures) in
  IntMap.mapi
    (fun k arr ->
      let instrs = ref [] in
      let emit i = instrs := i :: !instrs in
      let coroutines, offset = try IntMap.find k offsets with Not_found -> IntMap.empty, 1 in
      for i = 0 to Array.length arr - 1 do
	try 
	  let new_vars, vars = AddrMap.find (k, i) variables in
	  let nvars = List.length vars in
	  let rec interval n m = if n > m then [] else if n = m then [m] else n :: interval (n + 1) m in
	  let intervars n m = List.map (fun i -> `Var (List.nth vars i)) (interval n m) in
	  match arr.(i) with
	  | `Acc n when n >= nvars -> emit (`Store (`Var 0, `Arg (n - nvars)))
	  | `Acc n -> emit (`Store (`Var 0, `Var (List.nth vars n)))
	  | `Push -> emit (`Store (`Var (List.hd new_vars), `Var 0))
	  | `Assign n -> emit (`Store (`Var (List.nth vars n), `Var 0))
	  | `Env_acc n -> emit (`Store (`Var 0, `Env (n - offset)))
	  | `Pop _ -> ()
	  | `Apply n -> emit (`Apply (`Var 0, intervars 0 (n - 1), k + 1))
 	  | `App_term (n, _) -> emit (`App_term (`Var 0, intervars 0 (n - 1)))
	  | `Return _ -> emit (`Return (`Var 0))
	  | `Restart -> assert false
	  | `Grab _ -> ()
	  | `Closure (n, addr) ->
	    let env = if n = 0 then [] else `Var 0 :: intervars 0 (n - 2) in
	    emit (`Store (`Var 0, `Closure (addr, arity addr, env)))
	  | `Closure_rec (n, addrs) ->
	    let addrs = Array.of_list addrs in
	    let l = Array.length addrs - 1 in
	    let addrs = Array.map (fun a -> a, arity a) addrs in
	    let env = if n = 0 then [] else `Var 0 :: intervars 0 (n - 2) in
	    for i = 0 to l do
	      emit (`Store (`Var (List.nth new_vars (l - i)), `Closure (fst addrs.(i), snd addrs.(i), env)))
	    done
	  | `Offset_closure n ->
	    let addr = IntMap.find (n / 2) coroutines in
	    let arity = snd (IntMap.find addr closures) in
	    emit (`Store (`Var 0, `Offset_closure (addr, arity)))
	  | `Get_global n -> emit (`Store (`Var 0, `Global n))
	  | `Set_global n -> emit (`Store (`Global n, `Var 0))
	  | `Atom t -> emit (`Store (`Var 0, `Alloc (`Const (`Int 0), `Const (`Int t))))
	  | `Make_block (s, t) -> emit (`Store (`Var 0, `Tuple (`Const (`Int t), `Var 0 :: intervars 0 (s - 2))))
	  | `Make_float_block s -> emit (`Store (`Var 0, `Tuple (`Const (`Int (Obj.double_array_tag)), `Var 0 :: intervars 0 (s - 2))))
	  | `Get_field n -> emit (`Store (`Var 0, `Field (`Var 0, `Const (`Int n))))
	  | `Set_field n -> emit (`Store (`Field (`Var 0, `Const (`Int n)), `Var (List.nth vars 0))) ; emit (`Store (`Var 0, `Const (`Int 0)))
	    (* FIXME: differentiate float / int ops ? *)
	  | `Get_float_field n -> emit (`Store (`Var 0, `Field (`Var 0, `Const (`Int n))))
	  | `Set_float_field n -> emit (`Store (`Field (`Var 0, `Const (`Int n)), `Var (List.nth vars 0))) ; emit (`Store (`Var 0, `Const (`Int 0)))
	  | `Vect_length -> emit (`Store (`Var 0, `Size (`Var 0)))
	  | `Get_vect_item -> emit (`Store (`Var 0, `Field (`Var 0, `Var (List.nth vars 0))))
	  | `Set_vect_item -> emit (`Store (`Field (`Var 0, `Var (List.nth vars 0)), `Var (List.nth vars 1))) ; emit (`Store (`Var 0, `Const (`Int 0)))
	  | `Get_char -> emit (`Store (`Var 0, `Call (`External "ml_get_char", [ `Var 0; `Var (List.nth vars 0) ])))
	  | `Set_char -> emit (`Store (`Var 0, `Call (`External "ml_set_char", [ `Var 0; `Var (List.nth vars 0); `Var (List.nth vars 1) ])))
	  | `Branch a ->
	    (* remove fake, useless unconditional jumps *)
	    if i = 0 || match arr.(i - 1) with `Ext_call _ | `Apply _ -> false | _ -> true then emit (`Branch (`Const (`Int 1), a))
	  | `Branch_if a -> emit (`Branch (`Var 0, a))
	  | `Branch_if_not a -> emit (`Branch (`Call (`Not, [`Var 0]), a))
	  | `Branch_cmpop (op, c, addr) -> emit (`Branch (`Call ((op :> vbc_primitive), [`Const (`Int c) ; `Var 0]), addr))
	  | `Bool_not -> emit (`Store (`Var 0, `Call (`Not, [`Var 0])))
	  | `Const c -> emit (`Store (`Var 0, `Const (`Int c)))
	  | `Neg -> emit (`Store (`Var 0, `Call (`Neg, [`Var 0])))
	  | `Cmpop op -> emit (`Store (`Var 0, `Call ((op :> vbc_primitive), [`Var 0 ; `Var (List.nth vars 0)])))
	  | `Binop op -> emit (`Store (`Var 0, `Call ((op :> vbc_primitive), [`Var 0 ; `Var (List.nth vars 0)])))
	  | `Offset_int n -> emit (`Store (`Var 0, `Call (`Add, [`Var 0 ; `Const (`Int n)])))
	  | `Offset_ref n -> emit (`Store (`Field (`Var 0, `Const (`Int 0)), `Call (`Add, [`Field (`Var 0, `Const (`Int 0)) ; `Const (`Int n)])))
	  | `Ext_call (n, prim) -> emit (`Ext_call (`Var 0, prim, `Var 0 :: intervars 0 (n - 2), k + 1))
	  | `Raise -> emit (`Raise (`Var 0))
	  | `Push_trap addr -> emit (`Push_trap addr)
	  | `Pop_trap -> emit `Pop_trap
	  | `Is_int -> emit (`Store (`Var 0, `Is_int (`Var 0)))
	  | `Stop | `Event | `Break | `Check_signals as e -> emit e
	  | `Get_method -> emit (`Store (`Var 0, `Field (`Field (`Var (List.nth vars 0), `Const (`Int 0)), `Var 0)))
	  | `Get_pub_met n ->
	    emit (`Store (`Var (List.nth new_vars 0), `Var 0)) ;
	    emit (`Store (`Var 0, `Const (`Int n))) ;
	    emit (`Store (`Var 0, `Lookup (`Var (List.nth new_vars 0), `Var 0)))
	  | `Get_dyn_met ->
	    emit (`Store (`Var 0, `Lookup (`Var (List.nth vars 0), `Var 0)))
	  | `Switch (li, lp) -> emit (`Switch (`Var 0, li, lp))
	  | `Push_ret_addr _ -> ()
	with
	| Not_found ->
	  Printexc.print_backtrace stdout ;
	  warning "stack-to-vars" "lol %d %d" k i
	| exn ->
	  Printexc.print_backtrace stdout ;
	  error "stack-to-vars" "lol %d %d %s" k i (Printexc.to_string exn)
      done ;
      Array.of_list (List.rev !instrs))
    segments

let rec bc_to_vbc_value (bcv : bc_value) : vbc_rvalue =
  match bcv with
  | `Int _ | `String _ | `Int64 _ | `Int32 _ | `Float _ as v -> `Const v
  | `Block (t, v) -> `Tuple (`Const (`Int t), List.map bc_to_vbc_value (Array.to_list v))
  | `Float_block fs -> `Tuple (`Const (`Int Obj.double_array_tag), List.map (fun f -> `Const (`Float f)) (Array.to_list fs))
 
(* Switch from bytecode to variable based IR.
   Correct ENV_ACCs, retrieve functions and convert globals *)
let bc_to_vbc data code =
  let closures, offsets = retrieve_closures code in
  let code = remove_dead_segments code closures in
  let globals = IntMap.map (fun (v, n) -> (bc_to_vbc_value v, n)) data in
  let functions = IntMap.map (fun (_, arity) -> arity) closures in
  let segments = stack_to_vars_and_args code closures offsets in
  globals, segments, functions
