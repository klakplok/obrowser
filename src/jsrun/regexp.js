/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*                                                                     */
/*                        O'Browser II le retour                       */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Caml name: make
// Type:      string -> string -> t
function caml_regexp_make (vs, vf) {
    var s = string_val (vs);
    var f = string_val (vf);
    try { 
	return box_abstract (new RegExp (s, f));
    } catch (e) {
	caml_failwith ("Regexp.make: " + e.message);
    }
}

// Caml name: last_index
// Type:      t -> int
function caml_regexp_last_index (vr) { 
    var r = unbox_abstract (vr) ;
    return r.lastIndex;
}

// Caml name: test
// Type:      t -> string -> bool
function caml_regexp_test (vr, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_val (vs) ;
    return val_bool (r.test (s));
}

// Caml name: exec
// Type:      t -> string -> string array
function caml_regexp_exec (vr, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_val (vs) ;
    var res = r.exec (s);
    if (res == null) {
	caml_raise_not_found ();
    } else {
	var vres = mk_block (res.length, 0);
	for (var i = 0;i < res.length;i++) {
	  store_field (vres, i, val_string (res[i]));
	}
	return vres;
    }
}

// Caml name: index
// Type:      t -> string -> int
function caml_regexp_index (vr, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_val (vs) ;
    var i = s.search (r);
    if (i == -1) {
	caml_raise_not_found ();
    } else {
	return i;
    }
}

// Caml name: replace
// Type:      t -> string -> string -> string
function caml_regexp_replace (vr, vsub, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_val (vs) ;
    var sub = string_val (vsub) ;
    return (val_string (s.replace (r, sub)));
}

// Caml name: replace_fun
// Type:      t -> (int -> string array -> string) -> string -> string
function caml_regexp_replace_fun (vr, vf, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_val (vs) ;
    var f = function () {
	var vargs = mk_block (arguments.length - 2, 0);
	for (var i = 0; i < arguments.length - 2;i++) {
	    vargs.set (i, val_string (arguments[i]));
	}
	var res = caml_callback (vf, [arguments[arguments.length - 2], vargs]);
	return string_val (res);
    }
    return (val_string (s.replace (r, f)));
}

// Caml name: split
// Type:      t -> string -> string array
function caml_regexp_split (vr, vs) {
    var r = unbox_abstract (vr) ;
    var s = string_val (vs) ;
    var res = s.split (r);
    var vres = mk_block (res.length, 0);
    for (var i = 0;i < res.length;i++) {
      store_field (vres, i, val_string (res[i]));
    }
    return vres;
}

