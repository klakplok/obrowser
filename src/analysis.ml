(** generic abstract interpreter *)

open Types
open Messages


(* Build a simple (only branching, no calling) CFG, given specific entry points.
   Also return the domain (all activated segments). *)
let build_forward_cfg
    (type instr)
    (branches : instr -> int list)
    (program : instr array IntMap.t)
    (entries : int list)
    : IntSet.t AddrMap.t * IntSet.t =
  let res = ref AddrMap.empty in
  let processed = ref IntSet.empty in
  let to_process = ref (List.fold_right IntSet.add entries IntSet.empty) in
  let add src dst =
    res := AddrMap.add src (IntSet.add dst (try AddrMap.find src !res with Not_found -> IntSet.empty)) !res ;
    if not (IntSet.mem dst !processed) then to_process := IntSet.add dst !to_process
  in
  while not (IntSet.is_empty !to_process) do
    let pass = !to_process in
    processed := IntSet.union !processed !to_process ;
    to_process := IntSet.empty ;
    IntSet.iter
      (fun s ->
	Array.iteri
	  (fun i instr -> List.iter (add (s, i)) (branches instr))
	  (IntMap.find s program))
      pass
  done ;
  !res, !processed

(* Build a simple (only branching, no calling) reverse CFG, given specific entry points.
   Also return the domain (all activated segments). *)
let build_backward_cfg
    (type instr)
    (branches : instr -> int list)
    (program : instr array IntMap.t)
    (entries : int list)
    : AddrSet.t IntMap.t * IntSet.t =
  let cfg, dom = build_forward_cfg branches program entries in
  AddrMap.fold
    (fun src dsts r ->
      IntSet.fold
	(fun dst r -> IntMap.add dst (AddrSet.add src (try IntMap.find dst r with Not_found -> AddrSet.empty)) r)
	dsts r)
    cfg IntMap.empty,
  dom

(* Make a single traversal of the CFG. *)
let single_analysis
    (type instr) (type domain)
    (branches : instr -> int list)
    (program : instr array IntMap.t)
    (entries : int list)
    (eval : instr -> domain -> domain)
    (init : domain)
    : domain =
  let cfg, dom = build_forward_cfg branches program entries in
  IntSet.fold (fun seg -> Array.fold_right eval (IntMap.find seg program)) dom init

(* Generic fix point iterator for backward DFA abstract interpretation.
   Transfer and join are performed by eval.
   Meet and narrow are done by combine. *)
let backward_analysis
    (type instr) (type domain)
    (branches : instr -> int list)
    (program : instr array IntMap.t)
    (entries : int list)
    (eval : (int * int) -> (int * int) -> instr -> domain -> domain)
    (combine : domain -> domain -> domain)
    (bot : domain)
    (fix : domain -> domain -> bool)
    : domain AddrMap.t =
  (* First build the CFG and pre-allocate buckets *)
  let rcfg, dom = build_backward_cfg branches program entries in
  (* init *)
  let dirty = ref dom in
  let results =
    IntSet.fold
      (fun s r ->
	let len = Array.length (IntMap.find s program) in
	let rec add_all r = function -1 -> r | n -> add_all (AddrMap.add (s, n) (ref bot) r) (n - 1) in
	add_all r len)
      dom AddrMap.empty
  in
  let outcomes =
    IntMap.fold
      (fun src dsts r ->
	AddrSet.fold
	  (fun dst r ->
	    AddrMap.add dst (AddrMap.add (src, 0) (ref bot) (try AddrMap.find dst r with Not_found -> AddrMap.empty)) r)
	  dsts r
      ) rcfg AddrMap.empty
  in
  let outcomes = (* patch to add direct arc from following instruction *)
    AddrMap.mapi
      (fun (seg, i) s ->
	if i < Array.length (IntMap.find seg program) - 1 then
	  AddrMap.add (seg, i+1) (ref bot) s
	else s)
      outcomes
  in
  (* functions to manage the working set *)
  let make_dirty seg = dirty := IntSet.add seg !dirty in
  let get addr = AddrMap.find addr results in
  let store dst src res =
    let dst_bucket = try AddrMap.find src (AddrMap.find dst outcomes) with Not_found -> get dst in
    if not (fix !dst_bucket res) then
      make_dirty (fst dst) ;
    dst_bucket := res
  in
  (* abstract evaluation loop *)
  while not (IntSet.is_empty !dirty) do    
    let todo = !dirty in
    dirty := IntSet.empty ;
    IntSet.iter
      (fun seg ->
	let body = IntMap.find seg program in
	for i = Array.length body - 1 downto 0 do
	  let here = (seg, i) in
	  let next_addrs = if i = 0 then try AddrSet.elements (IntMap.find seg rcfg) with Not_found -> [] else [(seg, i - 1)] in
	  (try
	     let inputs = AddrMap.find here outcomes in
	     let inputs = List.map (!) (snd (List.split (AddrMap.bindings inputs))) in
	     get here := Utils.pairwise_fold combine inputs ;
	   with Not_found -> ()) ;
	  List.iter (fun addr -> store addr here (eval here addr body.(i) !(get here))) next_addrs
	done)
      todo
  done ;
  AddrMap.map (!) results

(* Generic fix point iterator for forward DFA abstract interpretation.
   Uses on the fly traversal (no preliminary CFG construction). *)
let forward_analysis
    (type instr) (type domain)
    (branches : instr -> int list)
    (program : instr array IntMap.t)
    (entries : (int * domain) list)
    (eval : (int * int) -> (int * int) -> instr -> domain -> domain)
    (combine : domain -> domain -> domain)
    (bot : domain)
    (fix : domain -> domain -> bool)
    : domain AddrMap.t =
  (* For each arc a = (s_a,i_a) -> b = (s_b, 0) in the CFG, 'outcomes'
     is indexed by s_b then by a.  It stores the results of transfer
     functions. Their combination using 'combine' is not computed at
     every of these arcs, only when evaluating b. *)
  let outcomes : domain AddrMap.t ref IntMap.t ref = ref IntMap.empty in
  (* 'results' maps each program point to its last evaluation. *)
  let results : domain ref AddrMap.t ref = ref AddrMap.empty in
  (* 'dirty' is the working set of segments whose inputs have changed
     since their last evaluation. *)
  let dirty = ref IntSet.empty in
  (* functions to manage the working set *)
  let make_dirty seg = dirty := IntSet.add seg !dirty in
  let get addr =
    try
      AddrMap.find addr !results
    with Not_found ->
      let result = ref bot in
      results := AddrMap.add addr result !results ;
      result
  in
  let store (seg, idx) from_addr input =
    if idx = 0 then (
      let last_outcomes =
	try IntMap.find seg !outcomes
	with Not_found ->
	  let new_outcomes = ref AddrMap.empty in
	  outcomes := IntMap.add seg new_outcomes !outcomes ;
	  new_outcomes
      in
      try
	let last_input = AddrMap.find from_addr !last_outcomes in
	if not (fix input last_input) then raise Exit
      with Not_found | Exit ->
	last_outcomes := AddrMap.add from_addr input !last_outcomes ;
	make_dirty seg
    ) else (
      get (seg, idx) := input
    )
  in
  (* init *)
  let uniq = ref 0  (* used to build fake input addresses so that multiple values for one entry are ok *) in
  List.iter (fun (seg, init) -> incr uniq ; store (seg, 0) (-1,!uniq) init) entries ;
  (* abstract evaluation loop *)
  while not (IntSet.is_empty !dirty) do    
    let todo = !dirty in
    dirty := IntSet.empty ;
    IntSet.iter
      (fun seg ->
	let body = IntMap.find seg program in
	(* update init result by combining all outcomes *)
	get (seg, 0) := Utils.pairwise_fold combine (snd (List.split (AddrMap.bindings !(IntMap.find seg !outcomes)))) ;
	(* then processs the segment  *)
	for i = 0 to Array.length body - 1 do
	  let here = (seg, i) in
	  let next_addrs = List.map (fun seg -> (seg, 0)) (branches body.(i)) in
	  let next_addrs = if i < Array.length body - 1 then (seg, i + 1) ::  next_addrs else next_addrs in
	  List.iter (fun addr -> store addr here (eval here addr body.(i) !(get here))) next_addrs
	done)
      todo
  done ;
  (* When the loop has ended, 'result' is up to date w.r.t. outcomes,
     so it is indeed the end result. *)
  AddrMap.map (!) !results
