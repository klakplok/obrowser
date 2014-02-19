/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*                                                                     */
/*                        O'Browser II le retour                       */
/*                ReImplementation of the Sys Library                  */
/*                                                                     */
/*  Copyright 2012 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Caml name: get_argv
// Type:      unit -> string * string array
caml_sys_get_argv = function (unit) {
  return mk_block ([og$exe_name, og$argv], 0);
}

var init_time = (new Date ()).getTime () * 0.001;

// Caml name: time
// Type:      unit -> float
caml_sys_time = function (unit) {
  return val_float ((new Date ()).getTime () * 0.001 - init_time);
}

// Caml name: get_config
// Type:      unit -> string * int * byte_order
caml_sys_get_config = function (unit) {
  return mk_block ([val_string ("Unix"), val_int (32), val_int (0)], 0);
}

// Caml name: getenv
// Type:      string -> string
caml_sys_getenv = function (v) {
  return val_string (""); /* FIXME: OCAMLRUNPARAM */
  caml_raise_not_found ();
}

// Caml name: random_seed
// Type:      unit -> int array
caml_sys_random_seed = function (unit) {
  var s1 = Math.random() * Math.pow(2, 31);
  var s2 = Math.random() * Math.pow(2, 31);
  var s3 = Math.random() * Math.pow(2, 31);
  return mk_block ([val_int (s1), val_int (s2), val_int (s3)], 0);
}

// Caml name: file_exists
// Type:      string -> bool
caml_primitive_not_implemented ("caml_sys_file_exists");

// Caml name: is_directory
// Type:      string -> bool
caml_primitive_not_implemented ("caml_sys_is_directory");

// Caml name: remove
// Type:      string -> unit
caml_primitive_not_implemented ("caml_sys_remove");

// Caml name: rename
// Type:      string -> string -> unit
caml_primitive_not_implemented ("caml_sys_rename");

// Caml name: command
// Type:      string -> int
caml_primitive_not_implemented ("caml_sys_system_command");

// Caml name: chdir
// Type:      string -> unit
caml_primitive_not_implemented ("caml_sys_chdir");

// Caml name: getcwd
// Type:      unit -> string
caml_primitive_not_implemented ("caml_sys_getcwd");

// Caml name: readdir
// Type:      string -> string array
caml_primitive_not_implemented ("caml_sys_read_directory");

// Caml name: signal
// Type:      int -> signal_behavior -> signal_behavior
caml_primitive_stub("caml_install_signal_handler", UNIT);

// Caml name: exit
// Type:      unit -> unit
caml_sys_exit = function () {
  o$thread_kill (o$ctx.pid);
}
