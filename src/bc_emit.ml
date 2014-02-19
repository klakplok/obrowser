open Messages
open Types
open Printf
open Utils

let rec print_value fp indent value =
  match value with
  | `Int i ->
    fprintf fp "  %a.int    %d\n%!" prindent indent i
  | `Float f ->
    fprintf fp "  %a.float  %g\n%!" prindent indent f
  | `Float_block fs ->
    fprintf fp "  %a.block 254 [\n%!%a  %a]\n%!"
      prindent indent
      (fun fp -> Array.iter (fun v -> print_value fp (indent + 1) (`Float v))) fs
      prindent indent
  | `String s ->
    fprintf fp "  %a.string \"%s\"\n%!" prindent indent s
  | `Block (tag,vs) ->
    fprintf fp "  %a.block %d [\n%!%a  %a]\n%!"
      prindent indent
      tag
      (fun fp -> Array.iter (fun v -> print_value fp (indent + 1) v)) vs
	  prindent indent
  | `Int32 i32 ->
    fprintf fp "  %a.int32  %ld\n%!" prindent indent i32
  | `Int64 i64 ->
    fprintf fp "  %a.int64  %Ld\n%!" prindent indent i64

let string_of_instr instr globals =
  match instr with
  | `Acc i -> sprintf "acc %d" i
  | `Push -> sprintf "push"
  | `Pop i -> sprintf "pop %d" i
  | `Assign i  -> sprintf "assign %d" i
  | `Env_acc i  -> sprintf "env_acc %d" i
  | `Push_ret_addr a -> sprintf "push_ret_addr @%d" a
  | `Apply i -> sprintf "apply %d" i
  | `App_term (i, s) -> sprintf "app_term %d %d" i s
  | `Return i -> sprintf "return %d" i
  | `Restart -> sprintf "restart"
  | `Grab (i, a) -> sprintf "grab %d @%d" i a
  | `Closure (i, a) -> sprintf "closure  %d @%d" i a
  | `Closure_rec (i, al) ->sprintf "closure_rec %d [%s]" i (sprintf_list "@%d" "," al)
  | `Offset_closure i -> sprintf "offset_closure  %d" i
  | `Get_global i -> sprintf "get_global @G%d (%s)" i (match snd (IntMap.find i globals) with None -> "unknwon" | Some n -> n)
  | `Set_global i -> sprintf "set_global @G%d (%s)" i (match snd (IntMap.find i globals) with None -> "unknwon" | Some n -> n)
  | `Atom i -> sprintf "atom %d" i
  | `Make_block (s, t) -> sprintf "make_block %d %d" s t
  | `Make_float_block s -> sprintf "make_float_block %d" s
  | `Get_field i -> sprintf "get_field %d" i
  | `Set_field i -> sprintf "set_field %d" i
  | `Get_float_field i -> sprintf "get_float_field %d" i
  | `Set_float_field i -> sprintf "set_float_field %d" i
  | `Vect_length -> sprintf "vect_length"
  | `Get_vect_item -> sprintf "get_vect_item"
  | `Set_vect_item -> sprintf "set_vect_item"
  | `Get_char -> sprintf "get_char"
  | `Set_char -> sprintf "set_char"
  | `Branch a -> sprintf "branch @%d" a
  | `Branch_if a -> sprintf "branch_if @%d" a
  | `Branch_if_not a -> sprintf "branch_if_not @%d" a
  | `Switch (ai, at) -> sprintf "switch [%s] [%s]" (sprintf_list "@%d" "," ai) (sprintf_list "@%d" "," at)
  | `Bool_not -> sprintf "bool_not"
  | `Push_trap a -> sprintf "push_trap @%d" a
  | `Pop_trap -> sprintf "pop_trap"
  | `Raise -> sprintf "raise"
  | `Check_signals -> sprintf "check_signals"
  | `Ext_call (i, n) -> sprintf "ext_call %d %s" i n
  | `Const i -> sprintf "const %d" i
  | `Neg -> sprintf "neg"
  | `Binop `Add -> sprintf "add"
  | `Binop `Sub -> sprintf "sub"
  | `Binop `Mul -> sprintf "mul"
  | `Binop `Div -> sprintf "div"
  | `Binop `Mod -> sprintf "mod"
  | `Binop `And -> sprintf "and"
  | `Binop `Or -> sprintf "or"
  | `Binop `Xor -> sprintf "xor"
  | `Binop `Lsl -> sprintf "lsl"
  | `Binop `Lsr -> sprintf "lsr"
  | `Binop `Asr -> sprintf "asr"
  | `Cmpop `Eq -> sprintf "eq"
  | `Cmpop `Neq -> sprintf "neq"
  | `Cmpop `Lt -> sprintf "lt_int"
  | `Cmpop `Le -> sprintf "le_int"
  | `Cmpop `Gt -> sprintf "gt_int"
  | `Cmpop `Ge -> sprintf "ge_int"
  | `Cmpop `Ult -> sprintf "ult_int"
  | `Cmpop `Uge -> sprintf "uge_int"
  | `Offset_int i -> sprintf "offset_int %d" i
  | `Offset_ref i -> sprintf "offset_ref %d" i
  | `Is_int -> sprintf "is_int"
  | `Branch_cmpop (`Eq, i, a) -> sprintf "beq %d @%d" i a
  | `Branch_cmpop (`Neq, i,a) -> sprintf "bneq %d @%d" i a
  | `Branch_cmpop (`Lt, i, a) -> sprintf "blt_int %d @%d" i a
  | `Branch_cmpop (`Le, i, a) -> sprintf "ble_int %d @%d" i a
  | `Branch_cmpop (`Gt, i, a) -> sprintf "bgt_int %d @%d" i a
  | `Branch_cmpop (`Ge, i, a) -> sprintf "bge_int %d @%d" i a
  | `Branch_cmpop (`Ult, i,a) -> sprintf "bult_int %d @%d" i a
  | `Branch_cmpop (`Uge, i,a) -> sprintf "buge_int %d @%d" i a
  | `Get_method -> sprintf "get_method"
  | `Get_pub_met i -> sprintf "get_pub_met %d" i
  | `Get_dyn_met -> sprintf "get_dyn_met"
  | `Stop -> sprintf "stop"
  | `Event -> sprintf "event"
  | `Break -> sprintf "break"

let print_segment fp code data =
  for i = 0 to Array.length code - 1 do
    fprintf fp "  %s\n%!" (string_of_instr code.(i) data)
  done

let print_program fp data segments =
  debug "bc-emit" "printing global data" ;
  IntMap.iter
    (fun i (v, n) ->
      fprintf fp "@G%d (%s):\n%!" i (match n with None -> "unknwon" | Some n -> n);
      print_value fp 0 v)
    data ;
  debug "bc-emit" "printing program" ;
  IntMap.iter
    (fun i seg ->
      fprintf fp "@%d: /* oc$%s */\n%!" i (Utils.roman i);
      print_segment fp seg data)
    segments
    
