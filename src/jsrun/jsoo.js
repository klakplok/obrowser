// Caml name:  new_obj
// Caml type:  unit -> obj
function jsoo_new (o) {
  return {};
}
    
// Caml name:  eval
// Caml type:  string -> obj
function jsoo_eval (s) {
    try {
	var code = string_val (s) ;
	return eval (code);
    } catch (e) {
	caml_catch(e);
	caml_failwith("jsoo_eval: " + e.message);
    }
}

// Caml name:  get
// Caml type:  string -> obj -> obj
function jsoo_get (f, o) {
    return o[string_val (f)];
}

// Caml name:  set
// Caml type:  string -> obj -> obj -> unit
 function jsoo_set (f, v, o) {
     o[string_val (f)] = v;
     return UNIT;
}

// Caml name:  extract
// Caml type:  obj -> value
function jsoo_extract (o) {
  //   | Obj of obj        0
  //   | Num of float      1
  //   | String of string  2
  //   | Block of Obj.t    3
  //   | Nil
  if (o == null) {
      return 0;
  }
  if (typeof o == 'string') {
    return mk_block ([val_string (o)], 2);
  }
  if (typeof o == 'number') {
    return mk_block ([val_float (o)], 1);
  }
  if (is_block (o)) {
    return mk_block ([o], 3);
  }
  return mk_block ([o], 0);
}

// Caml name:  extract_bool
// Caml type:  obj -> bool
function jsoo_extract_bool (o) {
    if (o) return TRUE;
    return FALSE;
}

// Caml name:  inject
// Caml type:  value -> obj
function jsoo_inject (o) {
  if (!is_block(o))
    return null;
  if (block_tag (o) == 2) {
    return string_val (field (o, 0));
  }
  if (block_tag (o) == 1)
    return float_val (field (o, 0));
  return field (o, 0);
}

// Caml name:  call
// Caml type:  obj -> obj array -> obj -> obj
function jsoo_call (d, args, o) {
    try {
	return o.apply (d, args.c) ; /* WARNING: optimized */
    } catch (e) {
	caml_catch(e);
	caml_failwith("jsoo_call: " + e.message);
    }
}

// Caml name:  wrap_event
// Caml type:  (unit -> unit) > obj
function jsoo_wrap_event (clos, res) {
  return function (evt) {
    var pid = o$thread_new (clos);
    var p = o$ctx;
    do {
      if (p.pid == pid) {
	p.event_args = evt;
	break;
      }
      p = p.next;
    } while (p != o$ctx);
    setTimeout (o$loop, 0);
    return false;
  }
}

// Caml name:  get_event_args
// Caml type:  unit -> obj array
function jsoo_get_event_args (unit) {
    return o$ctx.event_args;
}
