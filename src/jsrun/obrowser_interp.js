acc(%d)
push()
pop(%d)
assign(%d)
env_acc(%d)
push_ret_addr(oc$%s)
  | `Apply i -> sprintf "apply(%d);" i
  | `App_term (i, s) -> sprintf "app_term(%d,%d);" i s
  | `Return i -> sprintf "return(%d);" i
  | `Restart -> sprintf "restart();"
  | `Grab (i, a) -> sprintf "grab(%d,oc$%s);" i (Utils.roman a)
  | `Closure (i, a) -> sprintf "closure(%d,oc$%s);" i (Utils.roman a)
  | `Closure_rec (i, al) ->sprintf "closure_rec(%d,[%s]);" i (sprintf_list "%s" "," (List.map Utils.roman al))
  | `Offset_closure i -> sprintf "offset_closure  %d" i
  | `Get_global i -> sprintf "og$%s;" 
  | `Set_global i -> sprintf "set_global(%s);" i
  | `Atom i -> sprintf "atom(%d);" i
  | `Make_block (s, t) -> sprintf "make_block(%d,%d);" s t
  | `Make_float_block s -> sprintf "make_float_block(%d);" s
  | `Get_field i -> sprintf "get_field(%d);" i
  | `Set_field i -> sprintf "set_field(%d);" i
  | `Get_float_field i -> sprintf "get_float_field(%d);" i
  | `Set_float_field i -> sprintf "set_float_field(%d);" i
  | `Vect_length -> sprintf "vect_length();"
  | `Get_vect_item -> sprintf "get_vect_item();"
  | `Set_vect_item -> sprintf "set_vect_item();"
  | `Get_char -> sprintf "get_char();"
  | `Set_char -> sprintf "set_char();"
  | `Branch a -> sprintf "branch(%s);" (Utils.roman a)
  | `Branch_if a -> sprintf "branch_if(%s);" (Utils.roman a)
  | `Branch_if_not a -> sprintf "branch_if_not(%s);" (Utils.roman a)
  | `Switch (ai, at) -> sprintf "switch([%s],[%s]);" (sprintf_list "%s" "," (List.map Utils.roman ai)) (sprintf_list "%s" "," (List.map Utils.roman at))
  | `Bool_not -> sprintf "bool_not();"
  | `Push_trap a -> sprintf "push_trap(%s);" (Utils.roman a)
  | `Pop_trap -> sprintf "pop_trap();"
  | `Raise -> sprintf "raise();"
  | `Check_signals -> sprintf "check_signals();"
  | `Ext_call (i, n) -> sprintf "ext_call(%d, \"%s\");" i n
  | `Const i -> sprintf "const(%d);" i
  | `Neg -> sprintf "neg();"
  | `Binop `Add -> sprintf "add();"
  | `Binop `Sub -> sprintf "sub();"
  | `Binop `Mul -> sprintf "mul();"
  | `Binop `Div -> sprintf "div();"
  | `Binop `Mod -> sprintf "mod();"
  | `Binop `And -> sprintf "and();"
  | `Binop `Or -> sprintf "or();"
  | `Binop `Xor -> sprintf "xor();"
  | `Binop `Lsl -> sprintf "lsl();"
  | `Binop `Lsr -> sprintf "lsr();"
  | `Binop `Asr -> sprintf "asr();"
  | `Cmpop `Eq -> sprintf "eq();"
  | `Cmpop `Neq -> sprintf "neq();"
  | `Cmpop `Lt -> sprintf "lt_int();"
  | `Cmpop `Le -> sprintf "le_int();"
  | `Cmpop `Gt -> sprintf "gt_int();"
  | `Cmpop `Ge -> sprintf "ge_int();"
  | `Cmpop `Ult -> sprintf "ult_int();"
  | `Cmpop `Uge -> sprintf "uge_int();"
  | `Offset_int i -> sprintf "offset_int(%d);" i
  | `Offset_ref i -> sprintf "offset_ref(%d);" i
  | `Is_int -> sprintf "is_int();"
  | `Branch_cmpop (`Eq, i, a) -> sprintf "beq(%d,oc$%s);" i (Utils.roman a)
  | `Branch_cmpop (`Neq, i,a) -> sprintf "bneq(%d,oc$%s);" i (Utils.roman a)
  | `Branch_cmpop (`Lt, i, a) -> sprintf "blt_int(%d,oc$%s);" i (Utils.roman a)
  | `Branch_cmpop (`Le, i, a) -> sprintf "ble_int(%d,oc$%s);" i (Utils.roman a)
  | `Branch_cmpop (`Gt, i, a) -> sprintf "bgt_int(%d,oc$%s);" i (Utils.roman a)
  | `Branch_cmpop (`Ge, i, a) -> sprintf "bge_int(%d,oc$%s);" i (Utils.roman a)
  | `Branch_cmpop (`Ult, i,a) -> sprintf "bult_int(%d,oc$%s);" i (Utils.roman a)
  | `Branch_cmpop (`Uge, i,a) -> sprintf "buge_int(%d,oc$%s);" i (Utils.roman a)
  | `Get_method -> sprintf "get_method();"
  | `Get_pub_met i -> sprintf "get_pub_met(%d);" i
  | `Get_dyn_met -> sprintf "get_dyn_met();"
  | `Stop -> sprintf "stop();"
  | `Event -> sprintf "event();"
  | `Break -> sprintf "break();"
