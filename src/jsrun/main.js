/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*                                                                     */
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2011 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

#define INT32(x) ((x) & (-1))

var TH_GOT_RES  = 43
var TH_RUN      = 44 /* everything <= RUN means running */
var TH_WAIT_RES = 45
var TH_SLEEP    = 46
var TH_WAIT     = 47

var VM_NOT_LAUNCHED = 664 /* not launched */
var VM_STOPPED      = 665 /* launched but not running (no alive thread) */
var VM_RUNNING      = 666 /* in a running step */
var VM_PAUSED       = 667 /* in pause between two running steps  */

var SEG_QUOTA = 200000
var RR_QUOTA  = 2000
var TIMEOUT   = 5000

var STACK_SIZE = 4000

var MAGIC_CAML_EX    = 0x0E1EE1E0
var MAGIC_CAML_CONT  = 0x0E2EE2E0

var MAGIC_CAML_MASK  = 0x0F0FF0F0
var MAGIC_CAML_BASE  = 0x0E0EE0E0


function caml_catch (e) {
  if ((e & MAGIC_CAML_MASK) == MAGIC_CAML_BASE) {
    throw (e) ;
  }
}

#include <utils.js>
#include <mlvalues.js>
#include <custom.js>
#include <marshall.js>
#include <pervasives.js>
#include <sys.js>
#include <md5.js>

#include <threads.js>
#include <rtjs.js>
#include <jsoo.js>
#include <regexp.js>
#include <camlinternalOO.js>
#include <graphics.js>
#include <lexing.js>

function O_debug (s) {
#ifdef DEBUG
  console.debug ("OBrowser (debug): " + s);
#endif
}

#ifdef DEBUG
#define O_debug(s) console.debug ("OBrowser (debug): " + s);
#else
#define O_debug(s)
#endif

function O_write (s) {
    var div = document.getElementById ("caml_io_console");
    if (div == null) {
	div = document.createElement ("DIV");
	div.style.position = "absolute";
	div.style.left = "5px";
	div.style.bottom = "5px";
	div.style.padding = "5px";
	div.style.backgroundColor = "lightgrey";
	div.style.color = "black";
	div.style.fontSize = "10px";
	div.style.whiteSpace = "pre";
	div.id = "caml_io_console";
	document.body.appendChild (div);
    }
    div.innerHTML += s;
    return UNIT;
}

function O_fatal (msg) {
  if (O_status != VM_RUNNING) {
    msg = "OBrowser fatal error (outside of loop): " + msg;
    O_write (msg);
    throw new Error (msg);
  } else {
    O_write ("OBrowser fatal error: " + msg);
    O_thread_kill (O_ctx.pid);
  }
}

function O_assert (cond, msg) {
  if (!cond) O_fatal (msg);
}

var O_status = VM_NOT_LAUNCHED

function caml_main () {
  if (O_status != VM_NOT_LAUNCHED) throw "OBrowser: already running";
  var argv = [];
  for (var i = 0;i < arguments.length;i++)
    argv[i] = val_string (arguments[i]);
  O_argv =  mk_block (argv, 0);
  O_started = true;
  O_status = VM_STOPPED;
  O_max_pid = 1;
  O_ctx = {
    pc : OAAA,
    sp : STACK_SIZE,
    xsp : -1,
    a : UNIT,
    s : [],
    e : ATOM,
    xa : 0,
    status : TH_RUN,
    pid : O_max_pid++
  };
  O_ctx.next = O_ctx ;
  O_ctx.prev = O_ctx ;
  O_loop ();
}

function O_raise (e) {
  O_ctx.a = e;
  if (O_ctx.xsp == -1) {
    O_fatal ("Uncaught exception "
	     + string_val (field (field (O_ctx.a, 0), 0))
	     + (block_size (O_ctx.a) == 2
		?(" " + O_repr (field (O_ctx.a, 1), 1000))
		:""));
  } else {
    O_ctx.sp = O_ctx.xsp;
    O_ctx.pc = O_ctx.s[O_ctx.sp];
    O_ctx.xsp = O_ctx.s[O_ctx.sp + 1];
    O_ctx.e = O_ctx.s[O_ctx.sp + 2];
    O_ctx.xa = O_ctx.s[O_ctx.sp + 3];
    O_ctx.sp += 4;
  }
}

#ifdef DEBUG
var O_hist = []
function O_record (pc) {
  for (var i = 0;i < 99;i++)
    O_hist[i] = O_hist[i + 1];
  O_hist[99] = pc;
  return true;
}
#endif

function O_loop () {
  if (O_status == VM_STOPPED) {
    O_status = VM_PAUSED;
    function _run () {
      O_status = VM_RUNNING;
      var t0 = new Date ().getTime ();
      for (var i = 0;i < RR_QUOTA;i++) {
	try {
	  if (O_ctx.status == TH_GOT_RES) {
	    O_ctx.a = O_ctx.cont ();
            delete O_ctx.cont;
	    O_ctx.status = TH_RUN;
	  }
	  if (O_ctx.status == TH_RUN) {
	    for (var j = 0;j < SEG_QUOTA /* && O_record (O_ctx.pc) */ && O_ctx.pc (O_ctx);j++)
	      /* nop*/ ;
	  }
	} catch (e) {
          if ((e & MAGIC_CAML_MASK) != MAGIC_CAML_BASE)
            throw e;
          //            O_fatal ("uncaught JS exception '" + e.message + "'");
	}
	if (O_ctx == null || !O_yield ()) {
	  O_status = VM_STOPPED;
	  break;
	}
	if (new Date ().getTime () - t0 > TIMEOUT) break;
      }
      if (O_status == VM_RUNNING) {
	O_status = VM_PAUSED;
	setTimeout (_run, 0);
      }
    }
    setTimeout (_run, 0);
  }
}

function thread_notify_all (res) {
  var p = O_ctx;
  if (!p) return;
  do {
    if (p.status == TH_WAIT_RES && p.w8n4 === res) {
      p.status = TH_GOT_RES;
      delete p.w8n4;
    }
    p = p.next;
  } while (p != O_ctx);
  if (O_status == VM_STOPPED)
    setTimeout (O_loop, 0);
}

function thread_notify_one (res) {
  var p = O_ctx;
  if (!p) return;
  do {
    if (p.status == TH_WAIT_RES && p.w8n4 === res) {
      p.status = TH_GOT_RES;
      delete p.w8n4;
      break;
    }
    p = p.next;
  } while (p != O_ctx);
  O_loop ();
}

function thread_wait (res, cont) {
  O_ctx.w8n4 = res;
  O_ctx.status = TH_WAIT_RES;
  O_ctx.cont = cont;
  throw MAGIC_CAML_CONT;
}

function O_yield () {
  var p = O_ctx.next;
  do {
    if (p.status <= TH_RUN) {
      O_ctx = p;
      return true;
    }
    p = p.next;
  } while (p != O_ctx);
  return false;
}

function O_thread_new (clos, arg1) {
  var t = {
    pc : closure_code (clos),
    sp : 0,
    xsp : -1,
    a : ((arg1 == undefined) ? UNIT : arg1),
    s : new Array (),
    e : clos,
    xa : 0,
    status : TH_RUN,
    pid : O_max_pid
  };
  if (O_ctx == null) {
    t.next = t;
    t.prev = t;
    O_ctx = t;
  } else {
    t.next = O_ctx.next;
    t.prev = O_ctx;
    O_ctx.next.prev = t;
    O_ctx.next = t;
  }
  return O_max_pid++;
}

function O_thread_kill (pid) {
  var p = O_ctx;
  do {
    if (p.pid == pid) {
      p.prev.next = p.next;
      p.next.prev = p.prev;
      if (O_ctx == p) {
	if (p == p.next) {
	  O_ctx = null;
	} else {
	  O_ctx = p.next;
	}
	throw MAGIC_CAML_CONT;
      }
    }
    p = p.next;
  } while (p != O_ctx);
}

function O_thread_stop () {
  if (O_ctx == O_ctx.next) {
    O_ctx = null;
  } else {
    O_ctx.prev.next = O_ctx.next;
    O_ctx.next.prev = O_ctx.prev;
    O_ctx = O_ctx.next;
  }
}

#include <exceptions.js>
#include <callback.js>
