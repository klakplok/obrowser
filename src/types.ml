type addr = int

module IntSet = Set.Make (struct type t = int let compare = compare end)
module IntMap = Map.Make (struct type t = int let compare = compare end)
module StringSet = Set.Make (struct type t = string let compare = compare end)
module StringMap = Map.Make (struct type t = string let compare = compare end)
module AddrSet = Set.Make (struct type t = int * int let compare = compare end)
module AddrMap = Map.Make (struct type t = int * int let compare = compare end)

(* original bytecode instructions *)

type bc_binop =
  [ `Add | `Sub | `Mul | `Div | `Mod | `And | `Or | `Xor | `Lsl | `Lsr | `Asr ]

type bc_cmpop =
  [ `Eq | `Neq | `Lt | `Le | `Gt | `Ge | `Ult | `Uge ]

type bc_value =
  [ `Int of int | `Int64 of Int64.t | `Int32 of Int32.t
  | `Block of int * bc_value array | `String of string
  | `Float of float | `Float_block of float array ]

type bc_instr =
  [ `Acc of int | `Push | `Pop of int | `Assign of int
  | `Env_acc of int
  | `Push_ret_addr of addr
  | `Apply of int
  | `App_term of int * int
  | `Return of int
  | `Restart
  | `Grab of (int * addr)
  | `Closure of int * addr
  | `Closure_rec of int * addr list
  | `Offset_closure of int
  | `Get_global of int
  | `Set_global of int
  | `Atom of int
  | `Make_block of int * int
  | `Make_float_block of int
  | `Get_field of int | `Set_field of int
  | `Get_float_field of int | `Set_float_field of int
  | `Vect_length | `Get_vect_item | `Set_vect_item
  | `Get_char | `Set_char
  | `Branch of addr | `Branch_if of addr | `Branch_if_not of addr
  | `Branch_cmpop of bc_cmpop * int * addr
  | `Switch of addr list * addr list
  | `Bool_not
  | `Push_trap of addr | `Pop_trap | `Raise
  | `Check_signals
  | `Ext_call of int * string
  | `Const of int
  | `Neg
  | `Binop of bc_binop | `Cmpop of bc_cmpop
  | `Offset_int of int | `Offset_ref of int
  | `Is_int
  | `Get_method | `Get_pub_met of int | `Get_dyn_met
  | `Stop | `Event | `Break ]

type bc_segment = bc_instr array
type bc_program = bc_segment IntMap.t * (bc_value * string option) IntMap.t

(* variables based bytecode instructions *)

type vbc_value =
  [ `Int of int
  | `String of string
  | `Int64 of Int64.t
  | `Int32 of Int32.t
  | `Float of float ]

type vbc_primitive =
  [ bc_binop
  | bc_cmpop
  | `Neg | `Not
  | `External of string ]

type vbc_slvalue =
  [ `Var of int
  | `Arg of int
  | `Global of int
  | `Env of int ]

type vbc_srvalue =
  [ vbc_slvalue
  | `Const of vbc_value ]

type vbc_rvalue =
  [ vbc_srvalue
  | `Field of vbc_rvalue * vbc_rvalue
  | `Call of vbc_primitive * vbc_rvalue list
  | `Size of vbc_rvalue
  | `Tag of vbc_rvalue
  | `Is_int of vbc_rvalue
  | `Alloc of vbc_rvalue * vbc_rvalue
  | `Tuple of vbc_rvalue * vbc_rvalue list
  | `Closure of addr * int * vbc_rvalue list
  | `Offset_closure of addr * int
  | `Lookup of vbc_rvalue * vbc_rvalue ]

type vbc_lvalue =
  [ vbc_slvalue
  | `Field of vbc_rvalue * vbc_rvalue ]

type vbc_instr =
  [ `Store of vbc_lvalue * vbc_rvalue
  | `Apply of vbc_rvalue * vbc_rvalue list * addr
  | `App_term of vbc_rvalue * vbc_rvalue list
  | `Ext_call of vbc_lvalue * string * vbc_rvalue list * addr
  | `Return of vbc_rvalue
  | `Branch of vbc_rvalue * addr
  | `Switch of vbc_rvalue * addr list * addr list
  | `Push_trap of addr | `Pop_trap | `Raise of vbc_rvalue
  | `Check_signals | `Stop | `Event | `Break ]

type vbc_segment = vbc_instr array
type vbc_globals = (vbc_rvalue * string option) IntMap.t
type vbc_segments = vbc_segment IntMap.t
type vbc_program = vbc_segments * vbc_globals
type vbc_closures = int IntMap.t

module RvalueSet = Set.Make (struct type t = vbc_rvalue let compare = compare end)
module RvalueMap = Map.Make (struct type t = vbc_rvalue let compare = compare end)
module LvalueSet = Set.Make (struct type t = vbc_lvalue let compare = compare end)
module LvalueMap = Map.Make (struct type t = vbc_lvalue let compare = compare end)
module GlobalFieldMap = Map.Make (struct type t = int * int let compare = compare end)


(* utilities *)

let closure_entries closures = List.map (fun (k,_) -> k) (IntMap.bindings closures)

class vbc_iterator = object (self)
  method iter_instr (instr : vbc_instr) : unit =
    match instr with
    | `Store (lv, rv) -> self # iter_lvalue lv ; self # iter_rvalue rv
    | `Apply (rv, rvs, a) -> self # iter_rvalue rv ; List.iter (self # iter_rvalue) rvs ; self # iter_addr a
    | `App_term (rv, rvs) -> self # iter_rvalue rv ; List.iter (self # iter_rvalue) rvs
    | `Ext_call (lv, _, rvs, a) -> self # iter_lvalue lv ; List.iter (self # iter_rvalue) rvs ; self # iter_addr a
    | `Return rv | `Raise rv -> self # iter_rvalue rv
    | `Switch (rv, ai, ab) -> self # iter_rvalue rv ; List.iter self # iter_addr ai ; List.iter self # iter_addr ab
    | `Branch (rv, a) -> self # iter_rvalue rv ; self # iter_addr a
    | `Push_trap a -> self # iter_addr a
    | `Pop_trap | `Check_signals | `Stop | `Event | `Break -> ()
  method iter_rvalue (rv : vbc_rvalue) : unit =
    match rv with
    | #vbc_srvalue as srv -> self # iter_srvalue srv
    | `Field (lrv, rv) -> self # iter_rvalue lrv ; self # iter_rvalue rv
    | `Call (prim, rvs) -> List.iter (self # iter_rvalue) rvs
    | `Size rv | `Tag rv | `Is_int rv -> self # iter_rvalue rv
    | `Lookup (rv1, rv2) | `Alloc (rv1, rv2) -> self # iter_rvalue rv1 ; self # iter_rvalue rv2
    | `Tuple (rv, rvs) -> self # iter_rvalue rv ; List.iter (self # iter_rvalue) rvs
    | `Closure (a, _, rvs) -> self # iter_addr a ; List.iter (self # iter_rvalue) rvs
    | `Offset_closure (a, _) -> self # iter_addr a
  method iter_lvalue (lv : vbc_lvalue) : unit =
    match lv with
    | `Field (lrv, rv) -> self # iter_rvalue lrv ; self # iter_rvalue rv
    | #vbc_slvalue as slv -> self # iter_slvalue slv
  method iter_srvalue (srv : vbc_srvalue) : unit =
    match srv with
    | `Const v -> self # iter_value v
    | #vbc_slvalue as slv -> self # iter_slvalue slv
  method iter_slvalue (slv : vbc_slvalue) : unit = ()
  method iter_value (v : vbc_value) : unit = ()
  method iter_addr (v : addr) : unit = ()
end  

class vbc_mapper = object (self)
  method map_instr (instr : vbc_instr) : vbc_instr =
    match instr with
    | `Store (lv, rv) -> `Store (self # map_lvalue lv, self # map_rvalue rv)
    | `Apply (rv, rvs, ret) -> `Apply (self # map_rvalue rv, List.map (self # map_rvalue) rvs, self # map_addr ret)
    | `App_term (rv, rvs) -> `App_term (self # map_rvalue rv, List.map (self # map_rvalue) rvs)
    | `Ext_call (lv, a, rvs, b) -> `Ext_call (self # map_lvalue lv, a, List.map (self # map_rvalue) rvs, self # map_addr b)
    | `Return rv -> `Return (self # map_rvalue rv)
    | `Branch (rv, a) -> `Branch (self # map_rvalue rv, self # map_addr a)
    | `Switch (rv, a1, a2) -> `Switch (self # map_rvalue rv, List.map self # map_addr a1, List.map self # map_addr a2)
    | `Raise rv -> `Raise (self # map_rvalue rv)
    | `Push_trap a -> `Push_trap (self # map_addr a)
    | `Pop_trap | `Check_signals | `Stop | `Event | `Break -> instr
  method map_rvalue (rv : vbc_rvalue) : vbc_rvalue =
    match rv with
    | #vbc_srvalue as srv -> (self # map_srvalue srv :> vbc_rvalue)
    | `Field (lrv, rv) -> `Field (self # map_rvalue lrv, self # map_rvalue rv)
    | `Call (prim, rvs) -> `Call (prim, List.map (self # map_rvalue) rvs)
    | `Size rv -> `Size (self # map_rvalue rv)
    | `Tag rv -> `Tag (self # map_rvalue rv)
    | `Is_int rv -> `Is_int (self # map_rvalue rv)
    | `Lookup (rv1, rv2) -> `Lookup (self # map_rvalue rv1, self # map_rvalue rv2)
    | `Alloc (rv1, rv2) -> `Alloc (self # map_rvalue rv1, self # map_rvalue rv2)
    | `Tuple (rv, rvs) -> `Tuple (self # map_rvalue rv, List.map (self # map_rvalue) rvs)
    | `Closure (a, r, rvs) -> `Closure (self # map_addr a, r, List.map (self # map_rvalue) rvs)
    | `Offset_closure (a, r) -> `Offset_closure (self # map_addr a, r)
  method map_lvalue (lv : vbc_lvalue) : vbc_lvalue =
    match lv with
    | `Field (lrv, rv) -> `Field (self # map_rvalue lrv, self # map_rvalue rv)
    | #vbc_slvalue as slv -> (self # map_slvalue slv :> vbc_lvalue)
  method map_srvalue (srv : vbc_srvalue) : vbc_srvalue =
    match srv with
    | `Const v -> `Const (self # map_value v)
    | #vbc_slvalue as slv -> (self # map_slvalue slv :> vbc_srvalue)
  method map_slvalue (slv : vbc_slvalue) : vbc_slvalue = slv
  method map_value (v : vbc_value) : vbc_value = v
  method map_addr (v : addr) : addr = v
end  

let vbc_branches (instr : vbc_instr) : addr list =
  match instr with
  | `Branch (_, a) | `Apply (_, _, a) | `Push_trap a | `Ext_call (_,_,_,a) -> [a]
  | `Switch (_, ai, at) -> ai @ at
  | _ -> []

let map_segment (f : int -> vbc_instr -> vbc_instr) (seg : vbc_segment) : vbc_segment =
  Array.mapi f seg

let rewrite_segment (f : int -> vbc_instr -> vbc_instr list)  (seg : vbc_segment) : vbc_segment =
  let res = ref [] in
  Array.iteri (fun i instr -> res := f i instr :: !res) seg ;
  Array.of_list (List.flatten (List.rev !res))

let nb_appearances glob pred =
  let res = ref 0 in
  let iterator = object
    inherit vbc_iterator as mom
    method! iter_rvalue v = if pred v then incr res ; mom # iter_rvalue v
  end in
  iterator # iter_instr glob ;
  !res

let appears_in_instr glob pred =
  nb_appearances glob pred > 0

let appears_in_rvalue glob pred =
  nb_appearances (`Store (`Var (-1000), glob)) pred > 0

