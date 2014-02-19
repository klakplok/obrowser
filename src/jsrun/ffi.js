/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*                                                                     */
/*                        O'Browser II le retour                       */
/*                                                                     */
/*  Copyright 2011 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

/* to be put at the beginning of each catch block for OCaml exceptions to pass through */
function caml_catch (exn) {
  o$rethrow_magic (exn)
}

/* the main entry point for OBrowser, takes strings as parameters which can be retrieved using Sys.argv */
function caml_main () {
    o$init (arguments);
    o$loop ();
}

/* for async externals, pause th current threas waiting for a resource to be nitified
 * takes the continuation of the external. */
function wait_for (res, cont) {
  o$thread_wait (res, cont);
}

/* notifies all threads waiting on a specific resource */
function notify_all (res) {
  o$thread_notify_all (res);
}

/* notifies one thread waiting on a specific resource */
function notify (res) {
  o$thread_notify_one (res);
}

/* build a stub, failing primitive */
function caml_primitive_not_implemented (name) {
  window[name] = function () {
    o$fatal ("primitive not implemented: " + name);
  }
}

/* build a stub, non failing primitive, returning always the same value */
function caml_primitive_stub (name, val) {
  window[name] = function () {
    // console.debug ("primitive " + name + " is just a stub");
    return val;
  }
}

/* INTS & BLOCKS *******************************************************/

/* gives an OCaml int from a JS number (truncated to 31 bits)  */
function val_int (x) {
  return $Z(x);
}

/* gives a JS number from an OCaml int*/
function int_val (x) {
  return x;
}

/* constants */
var NIL   = 0
var UNIT  = 0
var FALSE = 0
var TRUE  = 1

/* convert a JS bool to an OCaml bool */
function val_bool (v) {
  return (v ? TRUE : FALSE)
}

/* convert an OCaml bool to a JS bool */
function bool_val (v) {
  return (v == TRUE ? true : false)
}

/* OCaml tags : */
var NO_SCAN_TAG       = 251
var FORWARD_TAG       = 250
var INFIX_TAG         = 249
var OBJECT_TAG        = 248
var CLOSURE_TAG       = 247
var LAZY_TAG          = 246
var ABSTRACT_TAG      = 251
var STRING_TAG        = 252
var DOUBLE_TAG        = 253
var DOUBLE_ARRAY_TAG  = 254
var CUSTOM_TAG        = 255

/* build an OCaml block from a size/an array of values and a tag */
function o$block (c, t) {
  if (c instanceof Array) {
    this.c = c;
    this.t = t;
    this.s = c.length;
  } else {
    this.c = [];
    this.t = t;
    this.s = c;
  }
}
function mk_block (c, t) {
  return new o$block (c, t)
}

/* number of values in a block */
function block_size (b) {
  return b.s;
}

/* tag of a block */
function block_tag (b) {
  return b.t;
}

/* change the tag of a block */
function set_block_tag (x, t) {
  x.t = t;
}

/* change the size of a block */
function set_block_size (x, size) {
  x.s = size;
}

/* make a shallow copy of a block */
block_dup = dup_block = function (b) {
  return mk_block (b.c.slice (), b.t);
}

/* true is the argument is an OCaml block */
function is_block (b) {
  //return (b instanceof o$block); //FIXME: lol
  return (b instanceof Object);
}

/* true is the argument is not an OCaml block */
is_int = is_long = function (b) {
  return !(is_block (b)) ;
}

/* n^th field of a block */
function field (b, n) {
#ifdef DEBUG
  if (!is_block (b))
    o$fatal (b + " is not a block");
  if (n < 0 || n >= block_size (b))
    o$fatal ("overflow");
#endif
  return b.c[n];
}

/* set the n^th field of a block */
function store_field (b,n,v) {
#ifdef DEBUG
  if (!is_block (b))
    o$fatal (b + " is not a block");
  if (n < 0 || n >= block_size (b))
    o$fatal ("overflow");
#endif
  b.c[n] = v;
}


/* atoms (zero size blocks) */
var atoms = [];
function mk_atom (t) {
  if (!atoms[t])
    atoms[t] = mk_block (0, t);
  return atoms[t];
}
var ATOM = mk_atom (0);

/* CLOSURES ************************************************************/

/* extracts the code pointer from a closure */
function closure_code (v) {
#ifdef DEBUG
  if (block_tag (v) != CLOSURE_TAG && block_tag (v) != INFIX_TAG) {
    console.debug (v);
    o$fatal (v + " is not a closure");
  }
#endif
  return v.pc ;
}


/* SHORTCUTS FOR OCAML COMMON TYPES ************************************/

/* allocate a pair */
function mk_pair (v0, v1) {
  return mk_block ([v0, v1], 0);
}

/* first projection of a pair */
function fst (p) {
  return field (p, 0);
}

/* second projection of a pair */
function snd (p) {
  return field (p, 1);
}

/* build a list from its head and tail */
function list_cons (v0, v1) {
  return mk_block ([v0, v1], 0);
}

/* head of a list */
function list_hd (p) {
  return field (p, 0);
}

/* tail of a list */
function list_tl (p) {
  return field (p, 1);
}

/* wraps an abstract (JS value opaque from OCaml) value */
function box_abstract (v0) {
  return mk_block ([v0], ABSTRACT_TAG);
}

/* unwraps an abstract value */
function unbox_abstract (v) {
  return field (v, 0);
}

/* build an OCaml array from a JS array of OCaml values */
function array_of_js (a) {
  return mk_block (a, 0);
}

/* build an OCaml list from a JS array of OCaml values */
function list_of_js (a) {
  var r = 0;
  for (var i = a.length - 1;i >=0;i--)
    r = mk_block ([a[i], r], 0);
  return r;
}


/* FLOATS **************************************************************/

/* convert a JS number to an OCaml float */
function val_float (x) {
  return x; // return mk_block ([x], DOUBLE_TAG);
}

/* convert an OCaml float to a JS number */
function float_val (v) {
  return v; // return field (v, 0);
}

/* converts an OCaml float to an OCaml int */
function int_of_float (v) {
  return val_int(float_val (v));
}

/* converts an OCaml int to an OCaml float */
function float_of_int (x) {
  return val_float (int_val (x));
}

/* decode a JS float from an array of 8 bytes (mostly for internal use) */
function float_of_bytes (bytes) {
  /* sign & exponent */
  var sign = ((bytes[0] >>> 7) == 1);
  var exponent = (((bytes[0] & 0x7F) << 4) | (bytes[1] >> 4 )) - 1023;
  /* mantissa in a bool array */
  var ba = [];
  for (var b = 1;b < 8;b++)
    for (var d = 0;d < 8;d++)
      ba[(b - 1) * 8 + d - 4] = (((bytes[b] >> (7 - d)) & 1) == 1);
  /* proceed */
  var m = Number (1);
  for (var i = 0;i < 52;i++)
    if (ba[i])
      m += Math.pow (2, -(i + 1));
  return val_float ((sign ? (-1) : 1) * m * Math.pow (2, exponent));
}

/* encode a JS float to an array of 8 bytes (mostly for internal use) */
function bytes_of_float (x) {
  var x = float_val (x);
  var e = Math.ceil (Math.log (Math.abs (x)) / Math.log (2));
  var m = Math.abs (x * Math.pow (2, -e)) * 2 - 1;
  e += 1022;
  var bits = [];
  bits[0] = (x < 0);
  for (var i = 0;i <= 52 ; i++) {
    bits [11 + i] = (m >= 1);
    m = (m - Math.floor (m)) * 2;
  }
  for (var i = 0;i <= 10 ; i++) {
    bits [11 - i] = (((e >>> i) & 1) == 1);
  }
  var bytes = [0,0,0,0,0,0,0,0];
  for (var i = 0;i < 8 ; i++) {
    for (var j = 0;j < 8 ; j++) {
      bytes[i] = (bytes[i] * 2) | (bits[8 * i + j] ? 1 : 0);
    }
  }
  return bytes;
}

/* STRINGS *************************************************************
 *
 * OCaml strings are mutable so not directly JS string.
 * An OCaml string contains :
 *  s.c (regular block contents): an array of chars, possibly sparse
 *  s.init: the initial filling char
 *  s.jsstr: the JS version of the string, lazily updated
 *  s.utd: a flag set if s.jsstr is up to date or not
 * Any JS string resulting of a conversion from an OCaml one stores :
 *  s.ocstr: the OCaml corresponding string
 */

/* build an OCaml string from its size and a filling character */
function mk_string (sz, init) {
  var b = mk_block (sz, STRING_TAG);
  b.init = init;
  b.utd = false;
  b.toString = function () {string_val (this)};
  return b;
}
/* make an OCaml string from a JS string */
function val_string (s) {
  if (s.ocstr) {
    /* reuse existing */
    return s.ocstr;
  } else {
    /* build a new, UTD OCaml string */
    var b = mk_block (s.length, STRING_TAG);
    b.jsstr = s;
    s.ocstr = b;
    b.utd = true;
    b.toString = function () {string_val (this)};
    return b;
  }
}
/* make a JS string from an OCaml string, recomputing the cached JS string if needed */
function string_val (v) {
  if (!v.utd) {
    /* update cached JS string */
    v.jsstr = update_jsstr (v);
    v.utd = true;
  }
  return v.jsstr.valueOf ();
}
/* string size */
function string_size (s) {
  return block_size (s) ;
}
/* set a char, invalidating the cached JS string */
function string_set (s, i, c) {
  store_field (s, i, c);
  s.utd = false;
}
/* string char access, using both cached JS string and mutations */
function string_get (s, i) {
  if (field (s, i) != undefined) {
    return field (s, i);
  } else {
    if (s.jsstr != undefined) {
      return s.jsstr.charCodeAt (i);
    } else {
      return s.init;
    }
  }
}
/* to update the cached JS string (for internal use) */
function update_jsstr (v) {
  function update_jsstr_rec (s, e) {
    if (s == e) {
      if (field (v, s) != undefined) {
        return String.fromCharCode (field (v, s));
      } else {
        return v.jsstr.charAt (s);
      }
    } else {
      return update_jsstr_rec (s, (e + s) >>> 1) + update_jsstr_rec (((e + s) >>> 1) + 1, e);
    }
  }
  function update_jsstr_rec_init (s, e) {
    if (s == e) {
      if (field (v, s) != undefined) {
        return String.fromCharCode (field (v, s));
      } else {
        return String.fromCharCode (v.init);
      }
    } else {
      return update_jsstr_rec_init (s, (e + s) >>> 1) + update_jsstr_rec_init (((e + s) >>> 1) + 1, e);
    }
  }
  if (block_size (v) == 0) {
    return "";
  } else {
    if (v.jsstr) {
      /* OCaml string has been created from a JS string */
      return update_jsstr_rec (0, block_size (v) - 1);
    } else {
      /* OCaml string has been created from an init char and a length */
      return update_jsstr_rec_init (0, block_size (v) - 1);
    }
  }
}

/* EXCEPTIONS **********************************************************/

/* raise an exception, takes the OCaml exn value */
function caml_raise (e) {
    o$raise (e);
    throw MAGIC_CAML_EX;
}

/* raise Invalid_argument */
function caml_invalid_arg (msg) {
    caml_raise (mk_block ([og$Invalid_argument, val_string (msg)], 0));
}

/* raise Failure */
function caml_failwith (msg) {
    caml_raise (mk_block ([og$Failure, val_string (msg)], 0));
}

/* raise Invalid_argument "index out of bounds" */
function caml_array_bound_error () {
    caml_invalid_arg ("index out of bounds");
}

/* raise End_of_file */
function caml_raise_end_of_file () {
    caml_raise (mk_block ([og$End_of_file], 0));
}

/* raise Not_found */
function caml_raise_not_found () {
    caml_raise (mk_block ([og$Not_found], 0));
}

/* THAT'S ALL FOLKS ! **************************************************/
