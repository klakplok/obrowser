/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*                                                                     */
/*                        O'Browser II le retour                       */
/*                                                                     */
/*  Copyright 2011 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Caml name: thread_initialize
// Type:      unit -> unit
function thread_initialize () {
  return UNIT;
}

// Caml name: thread_initialize_preemption
// Type:      unit -> unit
function thread_initialize_preemption () {
  return UNIT;
}

// Caml name: thread_new
// Type:      (unit -> unit) -> t
function thread_new (clos) {
  // type t = pid
  return o$thread_new (clos);
}

// Caml name: thread_self
// Type:      unit -> t
function thread_self (unit) {
  return o$ctx.pid;
}

// Caml name: thread_kill
// Type:      t -> unit
function thread_kill (pid) {
  o$thread_kill (pid);
  return UNIT;
}

// Caml name: thread_yield
// Type:      unit -> unit
thread_request_reschedule = thread_yield = function () {
  o$ctx.fsp.accu = UNIT;
  o$yield ();
  throw MAGIC_CAML_CONT;
}

// Caml name: id
// Type:      t -> int
function thread_id (pid) {
  throw pid;
}

// Caml name: thread_sleep
// Type:      unit -> unit
function thread_sleep () {
  o$ctx.status = TH_SLEEP;
  o$ctx.fsp.accu = UNIT;
  throw MAGIC_CAML_CONT;
}

// Caml name: thread_delay
// Type:      float -> unit
function thread_delay (s) {
  function make_resumer (ctx) {
    return function () {
      ctx.fsp.accu = UNIT;
      ctx.status = TH_RUN;
      delete ctx.delay_id;
      if (o$status == VM_STOPPED)
        setTimeout (o$loop, 0);
    }
  }
  o$ctx.status = TH_SLEEP;
  o$ctx.delay_id = setTimeout (make_resumer (o$ctx), Math.round (float_val (s) * 1000));
  throw MAGIC_CAML_CONT;
}

// Caml name: thread_wakeup
// Type:      t -> unit
function thread_wakeup (pid) {
  var p = o$ctx;
  do {
    if (p.pid == pid) {
      p.status = TH_RUN;
      if (p.delay_id) {
	clearTimeout (p.delay_id);
	delete p.delay_id;
      }	
    }
    p = p.next;
  } while (p != o$ctx);
  o$loop (); /* rerun if necessary */
  return UNIT;
}

// Caml name: thread_wait_pid, thread_join
// Type:      t -> unit
thread_wait_pid =
  thread_join = function (pid) {
    o$ctx.status = TH_WAIT;
    o$ctx.w8n4 = pid;
  }

// Caml name: thread_uncaught_exception
// Type:      exn -> unit
function thread_uncaught_exception (e) {
  caml_raise (e);
}

// Caml name: create
// Type:      unit -> t
function caml_js_mutex_create (u) {
  var mutex = { locked: false, owner:0 };
  return box_abstract (mutex);
}

// Caml name : lock
// Type:       t -> unit
function caml_js_mutex_lock (m) {
  var mutex = unbox_abstract (m);
  if (mutex.locked) {
    o$thread_wait (mutex, function () {
      caml_js_mutex_lock (m);
    });
  } else {
    mutex.locked = true;
    mutex.owner = o$ctx.pid;
    return UNIT ;
  }
}

// Caml name : try_lock
// Type:       t -> bool
function caml_js_mutex_try_lock (m) {
  var mutex = unbox_abstract (m);
  if (mutex.locked) {
    return val_bool (false) ;
  } else {
    mutex.locked = true;
    mutex.owner = o$ctx.pid;
    return val_bool (true) ;
  }
}

// Caml name: unlock
// Type:      t -> unit
function caml_js_mutex_unlock (m) {
  var mutex = unbox_abstract (m);
  if (mutex.locked && mutex.owner == o$ctx.pid) {
    mutex.locked = false;
    o$thread_notify_one (mutex);
  }
  return UNIT;
}
