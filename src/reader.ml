open Messages
open Utils
open Types
open Printf

(* FIXME: copy and paste from ocaml source code... *)
type ('a, 'b) tbl = Empty | Node of ('a, 'b) tbl * 'a * 'b * ('a, 'b) tbl * int
type ident = { stamp: int; name: string; mutable flags: int }
type numtable = { num_cnt: int; num_tbl: (ident, int) tbl }

let decode_symbols s =
  let res = ref [] in
  let rec revassoc t =
    match t with
    | Empty -> ()
    | Node(l, v, d, r, _) ->
      (*Messages.debug "symb" "%d -> %s" d v.name ;*)
      res := (d, v.name) :: !res ;
      revassoc l ;
      revassoc r
  in
  revassoc (Marshal.from_string s 0 : numtable).num_tbl ;
  (fun i -> List.assoc i !res)

let load path =
  let magic = "Caml1999X008" in
  let dat = read_whole_file path in
  let len = String.length dat in
  let r32le s n =
    let v =
      (Char.code s.[n + 0] lsl 24)
      lor (Char.code s.[n + 1] lsl 16)
      lor(Char.code s.[n + 2] lsl 8)
      lor (Char.code s.[n + 3])
    in
    if (Char.code s.[n] land 0x80) = 0x80 then
      (* sign extension *)
      ((-1) lxor 0x7F_FF_FF_FF) lor v
    else
      v
  in
  let r32be s n =
    let v =
      (Char.code s.[n + 3] lsl 24)
      lor (Char.code s.[n + 2] lsl 16)
      lor (Char.code s.[n + 1] lsl 8)
      lor (Char.code s.[n + 0])
    in
    if (Char.code s.[n + 3] land 0x80) = 0x80 then
      (* sign extension *)
      ((-1) lxor 0x7F_FF_FF_FF) lor v
    else
      v
  in
  (* check magic *)
  ensure (String.sub dat (len - 12) 12 = magic) "reader" "bad magic %S (expected %S)" (String.sub dat (len - 12) 12) magic ;
  (* split into raw sections *)
  let nsecs = r32le dat (len - 16) in
  let rec read_rsec n lb ib =
    if n >= nsecs then [] else
      let name = String.sub dat ib 4 in
      let slen = r32le dat (ib + 4) in
      debug "reader" "reading section [%s] of length %d" name slen ;
      let eb = lb - slen in
      (name, String.sub dat eb slen) :: read_rsec (n + 1) eb (ib - 8)
  in 
  let rsections = read_rsec 0 (len - 16 - nsecs * 8) (len - 16 - 8) in
  (* decode sections *)
  let decode_data seg (lookup : int -> string) =
    let rec unobj obj =
      if Obj.is_int obj then
	`Int (Obj.obj obj)
      else
	match Obj.tag obj with
	  | 253 (* DOUBLE_TAG *) ->
	    `Float (Obj.obj obj)
	  | 254 (* DOUBLE_ARRAY *) ->
	    let arr = Array.make (Obj.size obj) 0. in
	    for i = 0 to Obj.size obj - 1 do
	      arr.(i) <- Obj.double_field obj i
	    done ;
	    `Float_block arr
	  | 252 (* STRING_TAG *) ->
	    `String (Obj.obj obj)
	  | 255 (* CUSTOM *) ->
	    if Obj.field obj 0 == Obj.field (Obj.repr 1L) 0 then
	      `Int64 (Obj.obj obj)
	    else
	      if Obj.field obj 0 == Obj.field (Obj.repr 1l) 0 then
		`Int32 (Obj.obj obj)
	      else
		if Obj.field obj 0 == Obj.field (Obj.repr 1n) 0 then
		  `Int32 (Obj.obj obj) (* FIXME: nativeint *)
		else
		  error "reader" "unsupported custom block global"
	  | 250 (* FORWARD_TAG *)
	  | 249 (* INFIX_TAG *)
	  | 248 (* OBJECT_TAG *)
	  | 247 (* CLOSURE_TAG *)
	  | 246 (* LAZY_TAG *)
	  | 251 (* ABSTRACT *) ->
	    error "reader" "unsupported tag %d" (Obj.tag obj)
	  | t ->
	    let arr = Array.make (Obj.size obj) (`Int 0) in
	    for i = 0 to Obj.size obj - 1 do
	      arr.(i) <- unobj (Obj.field obj i)
	    done ;
	    `Block (t, arr)
    in
    debug "reader" "decoding data section" ;
    let raw = Obj.repr (Marshal.from_string seg 0) in
    let res = ref IntMap.empty in
    for i = 0 to Obj.size raw - 1 do
      res := IntMap.add i (unobj (Obj.field raw i), try Some (lookup i) with Not_found -> None) !res
    done ;
    !res
  in
  let decode_stringlist seg =
    debug "reader" "decoding strings section" ;
    let rec zero acc s l =
      if s + l >= String.length seg then
	acc
      else
	if seg.[s + l] = '\000' then
	  zero (String.sub seg s l :: acc) (s + l + 1) 0
	else
	  zero acc s (l + 1)
    in
    List.rev (zero [] 0 0)
  in
  let decode_code seg prims =
    debug "reader" "decoding code section" ;
    let nwc = String.length seg / 4 in
    let cl = ref [] and cc = ref 0 in
    let rmap = Array.make nwc None in
    let reloc a =
      if a < 0 || a >= nwc then error "reader" "impossible relocation @%d" a ;
      match  rmap.(a) with
	| None -> error "reader" "impossible relocation @%d" a
	| Some a -> a
    in
    let i = ref 0 in
    let arg () = incr i ; r32be seg (!i * 4) in
    let emit instr = cl := instr :: !cl ; incr cc in
    while !i < nwc do
      rmap.(!i) <- Some !cc ;
      let ii = !i + 1 in
      (* decode instructions *)
      begin match r32be seg (!i * 4) with
	| 0 -> emit (`Acc 0)
	| 1 -> emit (`Acc 1)
	| 2 -> emit (`Acc 2)
	| 3 -> emit (`Acc 3)
	| 4 -> emit (`Acc 4)
	| 5 -> emit (`Acc 5)
	| 6 -> emit (`Acc 6)
	| 7 -> emit (`Acc 7)
	| 8 -> emit (`Acc (arg ()))
	| 9 -> emit `Push
	| 10 -> emit `Push (* ; emit (`Acc 0) *)
	| 11 -> emit `Push ; emit (`Acc 1)
	| 12 -> emit `Push ; emit (`Acc 2)
	| 13 -> emit `Push ; emit (`Acc 3)
	| 14 -> emit `Push ; emit (`Acc 4)
	| 15 -> emit `Push ; emit (`Acc 5)
	| 16 -> emit `Push ; emit (`Acc 6)
	| 17 -> emit `Push ; emit (`Acc 7)
	| 18 -> emit `Push ; emit (`Acc (arg ()))
	| 19 -> emit (`Pop (arg ()))
	| 20 -> emit (`Assign (arg ()))
	| 21 -> emit (`Env_acc 1)
	| 22 -> emit (`Env_acc 2)
	| 23 -> emit (`Env_acc 3)
	| 24 -> emit (`Env_acc 4)
	| 25 -> emit (`Env_acc (arg ()))
	| 26 -> emit `Push ; emit (`Env_acc 1)
	| 27 -> emit `Push ; emit (`Env_acc 2)
	| 28 -> emit `Push ; emit (`Env_acc 3)
	| 29 -> emit `Push ; emit (`Env_acc 4)
	| 30 -> emit `Push ; emit (`Env_acc (arg ()))
	| 31 -> emit (`Push_ret_addr (ii + arg ()))
	| 32 -> let n = arg () in
		if n >=1 && n <= 3 then error "reader" "apply123" ;
		emit (`Apply n)
	| 33 -> emit (`Apply 1)
	| 34 -> emit (`Apply 2)
	| 35 -> emit (`Apply 3)
	| 36 -> let n = arg () in
		emit (`App_term (n, arg ()))
	| 37 -> emit (`App_term (1, arg ()))
	| 38 -> emit (`App_term (2, arg ()))
	| 39 -> emit (`App_term (3, arg ()))
	| 40 -> emit (`Return (arg ()))
	| 41 -> emit (`Restart)
	| 42 -> emit (`Grab (arg (), ii - 2))
	| 43 -> let nv = arg () in
		let a = arg () in
		emit (`Closure (nv, ii + 1 + a))
	| 44 -> let nf = arg () in
		let nv = arg () in
		let ptrs = ref [] in
		for f = 0 to nf - 1 do ptrs := (ii + 2 + arg ()) :: !ptrs done ;
		emit (`Closure_rec (nv, List.rev !ptrs))
	| 45 -> emit (`Offset_closure (-2))
	| 46 -> emit (`Offset_closure (0))
	| 47 -> emit (`Offset_closure (2))
	| 48 -> emit (`Offset_closure (arg ()))
	| 49 -> emit `Push ; emit (`Offset_closure (-2))
	| 50 -> emit `Push ; emit (`Offset_closure (0))
	| 51 -> emit `Push ; emit (`Offset_closure (2))
	| 52 -> emit `Push ; emit (`Offset_closure (arg ()))
	| 53 -> emit (`Get_global (arg ()))
	| 54 -> emit `Push ; emit (`Get_global(arg ()))
	| 55 -> emit (`Get_global (arg ())) ; emit (`Get_field (arg ()))
	| 56 -> emit `Push ; emit (`Get_global (arg ())) ; emit (`Get_field (arg ()))
	| 57 -> emit (`Set_global (arg ()))
	| 58 -> emit (`Atom 0)
	| 59 -> emit (`Atom (arg ()))
	| 60 -> emit `Push ; emit (`Atom 0)
	| 61 -> emit `Push ; emit (`Atom (arg ()))
	| 62 -> let s = arg () in
		let t = arg () in
		emit (`Make_block (s, t))
	| 63 -> let t = arg () in
		emit (`Make_block (1, t))
	| 64 -> let t = arg () in
		emit (`Make_block (2, t))
	| 65 -> let t = arg () in
		emit (`Make_block (3, t))
	| 66 -> let s = arg () in
		emit (`Make_float_block s)
	| 67 -> emit (`Get_field 0)
	| 68 -> emit (`Get_field 1)
	| 69 -> emit (`Get_field 2)
	| 70 -> emit (`Get_field 3)
	| 71 -> emit (`Get_field (arg ()))
	| 72 -> emit (`Get_float_field (arg ()))
	| 73 -> emit (`Set_field 0)
	| 74 -> emit (`Set_field 1)
	| 75 -> emit (`Set_field 2)
	| 76 -> emit (`Set_field 3)
	| 77 -> emit (`Set_field (arg ()))
	| 78 -> emit (`Set_float_field (arg ()))
	| 79 -> emit `Vect_length
	| 80 -> emit `Get_vect_item
	| 81 -> emit `Set_vect_item
	| 82 -> emit `Get_char
	| 83 -> emit `Set_char
	| 84 -> emit (`Branch (ii + arg ()))
	| 85 -> emit (`Branch_if (ii + arg ()))
	| 86 -> emit (`Branch_if_not (ii + arg ()))
	| 87 -> let n = arg () in 
		let nt = n lsr 16 in
		let ni = n land 0xFF_FF in
		let ptrs = ref [] in
		for f = 0 to ni - 1 do ptrs := (ii + 1 + arg ()) :: !ptrs done ;
		let li = List.rev !ptrs in
		ptrs := [] ;
		for f = 0 to nt - 1 do ptrs := (ii + 1 + arg ()) :: !ptrs done ;
		let lt = List.rev !ptrs in		
		emit (`Switch (li, lt))
	| 88 -> emit (`Bool_not)
	| 89 -> emit (`Push_trap (ii + arg ()))
	| 90 -> emit `Pop_trap
	| 91 -> emit `Raise
	| 92 -> emit `Check_signals
	| 93 -> emit (`Ext_call (1, prims.(arg ())))
	| 94 -> emit (`Ext_call (2, prims.(arg ())))
	| 95 -> emit (`Ext_call (3, prims.(arg ())))
	| 96 -> emit (`Ext_call (4, prims.(arg ())))
	| 97 -> emit (`Ext_call (5, prims.(arg ())))
	| 98 -> let n = arg () in
		emit (`Ext_call (n, prims.(arg ())))
	| 99 -> emit (`Const 0)
	| 100 -> emit (`Const 1)
	| 101 -> emit (`Const 2)
	| 102 -> emit (`Const 3)
	| 103 -> emit (`Const (arg ()))
	| 104 -> emit `Push ; emit (`Const 0)
	| 105 -> emit `Push ; emit (`Const 1)
	| 106 -> emit `Push ; emit (`Const 2)
	| 107 -> emit `Push ; emit (`Const 3)
	| 108 -> emit `Push ; emit (`Const (arg ()))
	| 109 -> emit `Neg
	| 110 -> emit (`Binop `Add)
	| 111 -> emit (`Binop `Sub)
	| 112 -> emit (`Binop `Mul)
	| 113 -> emit (`Binop `Div)
	| 114 -> emit (`Binop `Mod)
	| 115 -> emit (`Binop `And)
	| 116 -> emit (`Binop `Or)
	| 117 -> emit (`Binop `Xor)
	| 118 -> emit (`Binop `Lsl)
	| 119 -> emit (`Binop `Lsr)
	| 120 -> emit (`Binop `Asr)
	| 121 -> emit (`Cmpop `Eq)
	| 122 -> emit (`Cmpop `Neq)
	| 123 -> emit (`Cmpop `Lt)
	| 124 -> emit (`Cmpop `Le)
	| 125 -> emit (`Cmpop `Gt)
	| 126 -> emit (`Cmpop `Ge)
	| 127 -> emit (`Offset_int (arg ()))
	| 128 -> emit (`Offset_ref (arg ()))
	| 129 -> emit `Is_int
	| 130 -> emit `Get_method
	| 131 -> let v = arg () in
		 let a = ii + 1 + arg () in
		 emit (`Branch_cmpop (`Eq, v, a))
	| 132 -> let v = arg () in
		 let a = ii + 1 + arg () in
		 emit (`Branch_cmpop (`Neq, v, a))
	| 133 -> let v = arg () in
		 let a = ii + 1 + arg () in
		 emit (`Branch_cmpop (`Lt, v, a))
	| 134 -> let v = arg () in
		 let a = ii + 1 + arg () in
		 emit (`Branch_cmpop (`Le, v, a))
	| 135 -> let v = arg () in
		 let a = ii + 1 + arg () in
		 emit (`Branch_cmpop (`Gt, v, a))
	| 136 -> let v = arg () in
		 let a = ii + 1 + arg () in
		 emit (`Branch_cmpop (`Ge, v, a))
	| 137 -> emit (`Cmpop `Ult)
	| 138 -> emit (`Cmpop `Uge)
	| 139 -> let v = arg () in
		 let a = ii + 1 + arg () in
		 emit (`Branch_cmpop (`Ult, v, a))
	| 140 -> let v = arg () in
		 let a = ii + 1 + arg () in
		 emit (`Branch_cmpop (`Uge, v, a))
	| 141 -> emit (`Get_pub_met (arg ())) ; ignore (arg () (* skip method cache *))
	| 142 -> emit `Get_dyn_met
	| 143 -> emit `Stop
	| 144 -> emit `Event
	| 145 -> emit `Break
	| c -> error "reader" "unknown opcode %d at index %d of %d" c ii nwc
      end ; incr i
    done ;
    let oc = Array.of_list (List.rev !cl ;) in
    (* fix addresses *)
    for i = 0 to Array.length oc - 1 do
      oc.(i) <- match oc.(i) with
	| `Push_ret_addr a -> `Push_ret_addr (reloc a)
	| `Grab (n, a) -> `Grab (n, reloc a)
	| `Closure (n, a) -> `Closure (n, reloc a)
	| `Closure_rec (n, al) -> `Closure_rec (n, List.map reloc al)
	| `Switch (ai, at) -> `Switch (List.map reloc ai, List.map reloc at)
	| `Branch a -> `Branch (reloc a)
	| `Branch_if a -> `Branch_if (reloc a)
	| `Branch_if_not a -> `Branch_if_not (reloc a)
	| `Push_trap a -> `Push_trap (reloc a)
	| `Branch_cmpop (op, i, a) -> `Branch_cmpop (op, i, reloc a)
	| e -> e
    done ;
    oc
  in
  let dlls = "dllstdlib" :: decode_stringlist (List.assoc "DLLS" rsections) in
  let known_dlls = "dllvmthreads" :: "dllstdlib" :: "dllobrowser" :: [] in
  List.iter (fun dll -> if not (List.mem dll known_dlls) then warning "reader" "using unknown dll %s" dll) dlls ;
  let lookup_symb = decode_symbols (List.assoc "SYMB" rsections) in
  let data = decode_data (List.assoc "DATA" rsections) lookup_symb in
  let prims = decode_stringlist (List.assoc "PRIM" rsections) in
  let code = decode_code (List.assoc "CODE" rsections) (Array.of_list prims) in
  debug "reader" "bytecode reading finished" ;
  data, code, dlls
