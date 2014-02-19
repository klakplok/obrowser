/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*                                                                     */
/*                        O'Browser II le retour                       */
/*                                                                     */
/*  Copyright 2011 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

/* MACHINE **************************************************************/

/* thread states */
var TH_GOT_RES  = 43 /* to be resumed after ressource aquired */
var TH_RUN      = 44 /* running, any state <= RUN means running */
var TH_WAIT_RES = 45 /* waiting for resource */
var TH_SLEEP    = 46 /* sleeping */

/* machine states */
var VM_NOT_LAUNCHED = 664 /* not launched */
var VM_STOPPED      = 665 /* launched but not running (all threads have been terminated) */
var VM_RUNNING      = 666 /* in an evaluation loop step */
var VM_PAUSED       = 667 /* paused between two evaluation loop steps  */
var o$status = VM_NOT_LAUNCHED

/* evaluation loop options */
var RR_QUOTA  = 2000    /* number of yields per evaluation loop step */
var SEG_QUOTA = 8000    /* number of code blocks to perform between two yields in an evaluation step */
var TIMEOUT   = 500    /* maximum time of an evaluation loop step (msecs) */

/* initialize the machine */
function o$init (args) {
  if (o$status != VM_NOT_LAUNCHED)
    throw new Error ("OBrowser: already running");

  /* init argv */
  for (var i = 0;i < args.length;i++)
    args[i] = val_string (args[i]);
  og$argv =  mk_block (args, 0);

  /* init state */
  o$status = VM_STOPPED;
  o$max_pid = 1;
  o$ctx = {
    fsp : { pc: oc$entry, accu: 0, args: [], clos: {pc:oc$entry, env:[], args:[]} },
    xsp : null,
    status : TH_RUN,
    pid : o$max_pid++
  };
  o$ctx.next = o$ctx ;
  o$ctx.prev = o$ctx ;
}

/* the main evaluation loop */
function o$loop () {
  if (o$status == VM_STOPPED) {
    o$status = VM_PAUSED;
    function step () {
      o$status = VM_RUNNING;
      var t0 = new Date ().getTime ();
      for (var i = 0;i < RR_QUOTA;i++) {
	try {
	  if (o$ctx.status == TH_GOT_RES) {
	    o$ctx.fsp.accu = o$ctx.cont ();
            delete o$ctx.cont;
	    o$ctx.status = TH_RUN;
	  }
	  if (o$ctx.status == TH_RUN) {
	    for (var j = 0;j < SEG_QUOTA && o$ctx;j++) o$ctx.fsp.pc () ;
	  }
	} catch (e) {
          if ((e & MAGIC_CAML_MASK) != MAGIC_CAML_BASE) {
            if (e.message)
              o$message ("Uncaught JS exception '" + e.message + "'");
            else
              o$message ("Uncaught JS exception '" + e + "'");
            if (e.stack)
              o$message ("Stack: " + e.stack);
            o$thread_kill (o$ctx.pid);
            break;
          }
	}
	if (o$ctx == null || ! o$yield ()) {
	  o$status = VM_STOPPED;
	  break;
	}
	if (new Date ().getTime () - t0 > TIMEOUT)
          break;
      }
      if (o$status == VM_RUNNING) {
	o$status = VM_PAUSED;
        setTimeout (step);
      }
    }
    setTimeout (step);
  }
}

/* META INSTRUCTIONS ***************************************************/
function o$branch (cond, addr) {
  if (cond) {
    o$ctx.fsp.pc = addr;
    return true;
  }
  return false;
}

function o$closure (addr, arity, env) {
  return { pc: addr, arity: arity, env: env, args: [], t: CLOSURE_TAG }
}

function o$offset_closure (addr, arity) {
  return { pc: addr, arity: arity, env: o$ctx.fsp.clos.env, args: [], t: CLOSURE_TAG }
}

function o$appterm (clos, args) {
  var nargs = clos.args.concat (args).concat (o$ctx.fsp.args.slice (o$ctx.fsp.clos.arity));
  if (nargs.length >= clos.arity) {
    o$ctx.fsp = {
      pc: clos.pc,
      args: nargs,
      clos: clos,
      next: o$ctx.fsp.next
    }
    return true;
  } else {
    o$ctx.fsp = o$ctx.fsp.next;
    o$ctx.fsp.accu = { pc: clos.pc, arity: clos.arity, env: clos.env, args: nargs, t: CLOSURE_TAG};
    return false;
  }
}

function o$apply (clos, args, ret) {
  o$ctx.fsp.pc = ret;
  if (clos.args.length + args.length >= clos.arity) {
    o$ctx.fsp = {
      pc: clos.pc,
      args: clos.args.concat (args),
      clos: clos,
      next: o$ctx.fsp
    }
    return;
  } else {
    o$ctx.fsp.accu = { pc: clos.pc, arity: clos.arity, env: clos.env, args: clos.args.concat (args), t: CLOSURE_TAG};
    return;
  }
}

function o$return (ret) {
  if (o$ctx.fsp.clos.arity < o$ctx.fsp.args.length) {
    o$appterm (ret, []) ;
  } else {
    o$ctx.fsp = o$ctx.fsp.next ;
    o$ctx.fsp.accu = ret;
  }
  return true;
}

function o$push_trap (ret) {
  o$ctx.xsp = { pc: ret, sp: o$ctx.fsp, next: o$ctx.xsp }
}

function o$pop_trap () {
  o$ctx.xsp = o$ctx.xsp.next
}

function o$stop () {
  o$thread_stop ()
}

function o$checkpoint () {
  return;
}

function o$tuple (c, t) {
  return mk_block (c, t)
}

function o$alloc (c, t) {
  return mk_block (c, t)
}

function o$size (b) {
  return block_size (b)
}

function o$tag (b) {
  return block_tag (b)
}

function o$is_int (b) {
  return is_int (b)
}

function o$switch (val, i, a) {
  if (is_block (val)) {
    if (o$tag (val) >= a.length) o$fatal ("switch");
    o$ctx.fsp.pc = a[o$tag (val)]
  } else {
    if (val >= i.length) o$fatal ("switch");
    o$ctx.fsp.pc = i[val]
  }
}

function o$lookup (obj, lab) {
  var meths = field (obj, 0);
  var li = 3;
  var hi = field (meths, 0) * 2 + 1;
  while (li < hi) {
    var mi = ((li + hi) >> 1) | 1;
    if (lab < field (meths, mi))
      hi = mi - 2;
    else
      li = mi;
  } 
  return field (meths, li - 1);
}

/* THREADING META INSTRUCTIONS *****************************************/

/* switch to the next runable thread (round robin algorithm) */
function o$yield () {
  var p = o$ctx.next;
  do {
    if (p.status <= TH_RUN) {
      o$ctx = p;
      return true;
    }
  } while (p != o$ctx && (p = p.next));
  return false;
}

/* put the current thread to sleep, waiting for a resource availability to be notified */
function o$thread_wait (res, cont) {
  o$ctx.w8n4 = res;
  o$ctx.status = TH_WAIT_RES;
  o$ctx.cont = cont;
  throw MAGIC_CAML_CONT;
}

/* wake up all threads waiting for a resource */
function o$thread_notify_all (res) {
  var p = o$ctx;
  if (!p) return;
  do {
    if (p.status == TH_WAIT_RES && p.w8n4 === res) {
      p.status = TH_GOT_RES;
      delete p.w8n4;
    }
    p = p.next;
  } while (p != o$ctx);
  if (o$status == VM_STOPPED)
    setTimeout (o$loop, 0);
}

/* wake up one thread waiting for a resource */
function o$thread_notify_one (res) {
  var p = o$ctx;
  if (!p) return;
  do {
    if (p.status == TH_WAIT_RES && p.w8n4 === res) {
      p.status = TH_GOT_RES;
      delete p.w8n4;
      break;
    }
    p = p.next;
  } while (p != o$ctx);
  if (o$status == VM_STOPPED)
    setTimeout (o$loop, 0);
}

/* makes a new thread */
function o$thread_new (clos, arg) {
  o$max_pid++;
  var t = {
    fsp: { pc : closure_code (clos), args: clos.args.concat([arg]), clos: clos },
    xsp : null,
    status : TH_RUN,
    pid : o$max_pid
  };
  if (o$ctx == null) {
    t.next = t;
    t.prev = t;
    o$ctx = t;
  } else {
    t.next = o$ctx.next;
    t.prev = o$ctx;
    o$ctx.next.prev = t;
    o$ctx.next = t;
  }
  return o$max_pid;
}

/* kills a specific thread */
function o$thread_kill (pid) {
  var p = o$ctx;
  do {
    if (p.pid == pid) {
      p.prev.next = p.next;
      p.next.prev = p.prev;
      if (o$ctx == p) {
	if (p == p.next) {
	  o$ctx = null;
	} else {
	  o$ctx = p.next;
	}
	throw MAGIC_CAML_CONT;
      }
    }
    p = p.next;
  } while (p != o$ctx);
}

/* kills the current thread */
function o$thread_stop () {
  if (o$ctx == o$ctx.next) {
    o$ctx = null;
  } else {
    o$ctx.prev.next = o$ctx.next;
    o$ctx.next.prev = o$ctx.prev;
    o$ctx = o$ctx.next;
  }
}

/* EXCEPTIONS **********************************************************/

/* recognizable "magic" exceptional values */
var MAGIC_CAML_EX    = 0x0E1EE1E0 /* an OCaml exception has been raised, the value is in ctx.fsp.accu */
var MAGIC_CAML_CONT  = 0x0E2EE2E0 /* an OCaml external has performed an async operation, the continuation is in ctx.continuation */
var MAGIC_CAML_MASK  = 0x0F0FF0F0 /* used to detect magic */
var MAGIC_CAML_BASE  = 0x0E0EE0E0 /* used to detect magic */

/* kill the current thread with a message */
function o$fatal (msg) {
  if (o$status != VM_RUNNING) {
    msg = "OBrowser fatal error (outside of loop): " + msg;
    o$message (msg);
    throw new Error (msg);
  } else {
    o$message ("OBrowser fatal error: " + msg);
    o$thread_kill (o$ctx.pid);
  }
}

/* check point */
function o$assert (cond, msg) {
  if (!cond) o$fatal (msg);
}

/* kill the whole machine with a message */
function o$lethal (msg) {
  if (o$status != VM_RUNNING) {
    msg = "OBrowser fatal error (outside of loop): " + msg;
    o$message (msg);
    throw new Error (msg);
  } else {
    o$message ("OBrowser fatal error: " + msg);
    o$ctx = null;
    throw MAGIC_CAML_EX;
  }
}

/* raise a valued exception for the current thread */
function o$raise (exn) {
  if (!o$ctx.xsp /* empty exn stack*/) {
    o$fatal ("Uncaught exception "
	     + string_val (field (field (exn, 0), 0))
	     + (block_size (exn) == 2
		?(" " + o$string_repr (field (exn, 1), 1000))
		:""));
  } else {
    o$ctx.fsp = o$ctx.xsp.sp;
    o$ctx.fsp.pc = o$ctx.xsp.pc;
    o$ctx.fsp.accu = exn;
    o$ctx.xsp = o$ctx.xsp.next;
  }
}

/* filter OCaml exceptions */
function o$rethrow_magic (e) {
  if ((e & MAGIC_CAML_MASK) == MAGIC_CAML_BASE) throw (e);
}

/* VARIOUS INTERNAL STUFF **********************************************/

/* number -> 31 bit integer */
function $Z(x) {
  return (x & 0xBFFFFFFF) | ((x & 0x80000000) >>> 1) ;
}

/* unsigned comparison */
function ult (a,b) { return ((a >= 0) ? ((b < 0) || (a < b)) : ((b < 0) && (a < b))); }
function uge (a,b) { return !ult (a, b); }

/* very simple console to display fatal errors and similar stuff */
function o$message (s) {
  var div = document.getElementById ("caml_io_console");
  if (div == null) {
    div = document.createElement ("DIV");
    div.id = "caml_io_console";
    var sty = { position: "absolute", whiteSpace: "pre", left: "5px",
                bottom: "5px", backgroundColor: "lightgrey", padding: "5px" };
    for (i in sty) div.style[i] = sty[i];
    document.body.appendChild (div);
  }
  div.innerHTML += s + "\n";
}

/* value to string dumper (limit is the number of printed sub-values) */
function o$string_repr (v, limit) {
  var s = "";
  function string_repr_rec (v) {
    if (limit-- < 0) { s += "..."; return; }
    if (v instanceof Function) {
      s += sprintf ("<fun>", v);
    } else if (is_long (v)) {
      s += sprintf ("0x%X", v);
    } else {
      switch (block_tag (v)) {
      case STRING_TAG:
	s += "\"" + string_val (v) + "\"";
	break;
      case DOUBLE_TAG:
	s += float_val (v).toExponential ();
	break;
      default: {
	s += sprintf ("[(0x%02X) ", block_tag (v));
	for (var i = 0;i < block_size (v) - 1;i++) {
          if (limit-- < 0) { s += "..."; break; }
	  string_repr_rec (field (v, i));
	  s += ", ";
	}
        if (i < block_size (v)) {
          if (limit-- < 0) {
            s += "..." ;
          } else {
            string_repr_rec (field (v, i));
          }
        }
	s += "]";
      }
      }
    }
  }
  string_repr_rec (v);
  return s;
}

/* THAT'S ALL FOLKS ! **************************************************/
