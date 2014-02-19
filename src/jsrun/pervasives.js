/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*                                                                     */
/*                        O'Browser II le retour                       */
/*             ReImplementation of the Pervasives Library              */
/*                                                                     */
/*  Copyright 2012 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

/* STRINGS *************************************************************/

// Caml name: is_printable
// Type:      char -> bool
function caml_is_printable (c) {
  return val_bool (c >= 0x20);
}

// Caml name: unsafe_fill
// Type:      string -> int -> int -> char -> unit
function caml_fill_string (s, st, len, c) {
  for (var i = 0;i < len;i++)
    string_set (s, st + i, c);
  return UNIT;
}

// Caml name: string_create
// Type:      int -> string
function caml_create_string (len) {
  return mk_string (len, '\0');
}

// Caml name: string_blit
// Type:      string -> int -> string -> int -> int -> unit
function caml_blit_string (s1, ofs1, s2, ofs2, n) {
  for (var i = 0;i < n;i++) {
    string_set (s2, ofs2 + i, string_get (s1, ofs1 + i));
  }
  return UNIT;
}

// Caml name: get
// Type:      string -> int -> char
caml_string_get =
ml_get_char = function (s, idx) {
  if (idx >= 0 && idx < string_size (s)) {
    return string_get(s, idx);
  }
  caml_array_bound_error ();
}

// Caml name: unsafe_get
// Type:      string -> int -> char
function caml_string_unsafe_get (s, idx) {
  return string_get(s, idx);
}

// Caml name: set
// Type:      string -> int -> char -> unit
caml_string_set =
ml_set_char = function (s, idx, val) {
  if (idx >= 0 && idx < string_size (s) - 1) {
    string_set(s, idx, val);
    return UNIT;
  }
  caml_array_bound_error ();
}

// Caml name: unsafe_set
// Type:      string -> int -> char -> unit
function caml_string_unsafe_set (s, idx, val) {
  string_set (s, idx, val);
  return UNIT;
}

// Caml name: string_length
// Type:      string -> int
function caml_ml_string_length (s) {
  return string_size (s);
}

/* ARRAYS **************************************************************/

// Caml name: get
// Type:      'a array -> int -> 'a
caml_array_get = 
  caml_array_get_addr = 
  caml_array_get_float = function (arr, idx) {
    if (idx >= 0 && idx < block_size (arr)) {
      return field (arr, idx);
    }
    caml_array_bound_error ();
  }

// Caml name: set
// Type:      'a array -> int -> 'a -> unit
caml_array_set = 
  caml_array_set_addr = 
  caml_array_set_float = function (arr, idx, val) {
    if (idx >= 0 && idx < block_size (arr)) {
      store_field (arr, idx, val);
      return UNIT;
    }
    caml_array_bound_error ();
  }

// Caml name: unsafe_get
// Type:      'a array -> int -> 'a
caml_array_unsafe_get = 
caml_array_unsafe_get_addr = 
caml_array_unsafe_get_float = function (arr, idx) {
  return field (arr, idx);
}

// Caml name: unsafe_set
// Type:      'a array -> int -> 'a -> unit
caml_array_unsafe_set = 
  caml_array_unsafe_set_addr = 
  caml_array_unsafe_set_float = function (arr, idx, val) {
    store_field (arr, idx, val);
    return UNIT;
  }

// Caml name: make
// Type:      int -> 'a -> 'a array
caml_make_vect = function (len, init) {
  /* TODO: lazy init ? */
  var b = mk_block (len, 0);
  for (var i = 0;i < len;i++) {
    store_field (b, i, init);
  }
  return b;
}

// Caml name: make_array
// Type:      'a array -> 'a array
caml_make_array = function (init) {
  return dup_block (init);
}

// Caml name: blit
// Type:      'a array -> int -> 'a array -> int -> int -> unit
function caml_array_blit (a1, ofs1, a2, ofs2, n) {
  for (var i = 0;i < n;i++) {
    store_field (a2, ofs2 + i, field (a1, ofs1 + i));
  }
  return UNIT;
}

// Caml name: sub
// Type:      'a array -> int -> int -> 'a array
function caml_array_sub(a, ofs, len) {
  return mk_block (a.c.slice (ofs, ofs + len), 0)
}

// Caml name: append
// Type:      'a array -> 'a array -> 'a array
function caml_array_append(a1, a2) {
  return mk_block (a1.c.concat(a2.c), 0);
}

// Caml name: concat
// Type:      'a array list -> 'a array
caml_array_concat = function (list) {
  var c = [], i = 0;
  while (is_block (list)) {
    var tab = field (list, 0)
    for (var j = 0;j < block_size (tab);j++, i++)
      c[i] = field (tab, j)
    list = field (list, 1)
  }
  return mk_block (c, 0)
}

/* COMPARISON **********************************************************/

// Caml name: compare
// Type:      'a -> 'a -> int
caml_string_compare =
  caml_compare = function (a,b) {
    if (a == b)	return 0;
    if (!is_block(a)) {
      if (is_block(b)) {
	if (block_tag (b) == FORWARD_TAG)
	  return caml_compare (a, field (b, 0));
	return -1;
      }
      return a - b;
    }
    if (!is_block(b)) {
      if (block_tag (a) == FORWARD_TAG)
	return caml_compare (field (a, 0), b);
      return 1;
    }
    if (block_tag (a) == CLOSURE_TAG || block_tag (a) == INFIX_TAG)
      caml_invalid_arg ("equal: functional value");
    if (block_tag (a) != block_tag (b))
      return block_tag (a) - block_tag (b);
    if (block_tag (a) == CUSTOM_TAG)
      return custm_ops (a).compare (a, b);
    if (block_tag (a) == STRING_TAG) {
      /* WARNING: compare forces conversion */
      a = string_val (a);
      b = string_val (b);
      if (a == b) return 0;
      return ((a < b) ? -1 : 1);
    }
    if (block_tag (a) == DOUBLE_TAG) {
      a = float_val (a);
      b = float_val (b);
      if (a == b) return 0;
      return ((a < b) ? -1 : 1);
    }
    if (block_size (a) != block_size (b))
      return block_size (a) - block_size (b);
    for (var i = 0;i < block_size (a);i++) {
      t = caml_compare (field (a, i), field (b, i));
      if (t != 0) return t;
    }
    return 0;
  }

// Caml name: (=)
// Type:      'a -> 'a -> bool
function caml_equal (a, b) {
  return val_bool (caml_compare (a,b) == 0);
}

// Caml name: (<>)
// Type:      'a -> 'a -> bool
function caml_notequal (a, b) {
  return val_bool (caml_compare (a,b) != 0);
}

// Caml name: (=)
// Type:      'a -> 'a -> bool
function caml_string_equal (a, b) {
  return val_bool (caml_compare (a,b) == 0);
}

// Caml name: (<>)
// Type:      'a -> 'a -> bool
function caml_string_notequal (a, b) {
  return val_bool (caml_compare (a,b) != 0);
}

// Caml name: (<)
// Type:      'a -> 'a -> bool
caml_lessthan =
  caml_lt_float = function (a, b) {
    return val_bool (caml_compare (a,b) < 0);
  }

// Caml name: (<=)
// Type:      'a -> 'a -> bool
caml_lessequal =
  caml_le_float = function (a, b) {
    return val_bool (caml_compare (a,b) <= 0);
  }

// Caml name: (>)
// Type:      'a -> 'a -> bool
caml_greaterthan =
  caml_gt_float = function (a, b) {
    return val_bool (caml_compare (a,b) > 0);
  }

// Caml name: (>=)
// Type:      'a -> 'a -> bool
caml_greaterequal =
  caml_ge_float = function (a, b) {
    return val_bool (caml_compare (a,b) >= 0);
  }

// Caml name: (=)
// Type:      'a -> 'a -> bool
caml_eq_float = function (a, b) {
  return val_bool (caml_compare (a,b) == 0);
}

/* HASHING *************************************************************/

// Caml name: old_hash_param
// Type:      int -> int -> 'a -> int
function caml_hash_univ_param (count, limit, obj) {
  hash_univ_limit = limit;
  hash_univ_count = count;
  hash_accu = 0;
  hash_aux (obj);
  return (hash_accu & 0x3FFFFFFF);
}

// Caml name: seeded_hash_param
// Type:      int -> int -> int -> 'a -> int
function caml_hash (count, limit, seed, obj) {
  hash_univ_limit = limit;
  hash_univ_count = count;
  hash_accu = seed; // FIXME: check
  hash_aux (obj);
  return (hash_accu & 0x3FFFFFFF);
}

#define ALPHA 65599
#define BETA  19
#define COMBINE(n)  (hash_accu = hash_accu * ALPHA + (n))
#define COMBINE_SMALL(n) (hash_accu = hash_accu * BETA + (n))

function hash_aux(obj){
  hash_univ_limit--;
  if (hash_univ_count < 0 || hash_univ_limit < 0) return;
  
  if (is_long (obj)) {
    hash_univ_count--;
    COMBINE(obj);
    return;
  }
  
  switch (block_tag (obj)) {
  case STRING_TAG: {
    hash_univ_count--;
    for (var p = 0;p < block_size (obj) - 1; p++)
      COMBINE_SMALL(field (obj, p));
    break;
  }
  case DOUBLE_TAG: {
    hash_univ_count--;
    var bytes = bytes_of_float (obj);
    for (var p = 7; p >= 0; p--)
      COMBINE_SMALL(bytes[p]);
    break;
  }
  case DOUBLE_ARRAY_TAG: {
    hash_univ_count--;
    for (var j = 0; j < obj.size; j++) {
      var bytes = bytes_of_float (val_float (obj.get (j)));
      for (var p = 7; p >= 0; p--)
        COMBINE_SMALL(bytes[p]);
    }
    break;
  }
  case ABSTRACT_TAG:
    break;
  case INFIX_TAG:
    /* FIXME */
    break;
  case FORWARD_TAG:
    hash_univ_limit++;
    hash_aux (field (obj, 0));
    break;
  case OBJECT_TAG:
    hash_univ_count--;
    COMBINE(field (obj, 1));
    break;
  case CUSTOM_TAG:
    if (custom_ops (obj).hash != null) {
      hash_univ_count--;
      COMBINE(custom_ops (obj).hash (obj));
    }
    break;
  default: {
    hash_univ_count--;
    COMBINE_SMALL(obj_tag (obj));
    var i = obj_size (obj);
    while (i != 0) {
      i--;
      hash_aux(field (obj, i));
    }
    break;
  }
  }
  return;
}

/* CONVERSIONS *********************************************************/

// Caml name: format_int
// Type:      string -> int -> string
function caml_format_int (fmt, x) {
  /* FIXME: to review */
  var fmt = string_val (fmt);
  var t = fmt[fmt.length - 1];
  var n = Number (x).toString (t == 'd' ? 10 : (t == 'o' ? 8 : (t == 'x' ? 16 : 10 /* err */)));
  var l = 0, c = '0';
  if (fmt.length == 3) {
    l = fmt.charCodeAt (1) - "0".charCodeAt (0);
  } else {
    if (fmt.length >= 4) {
      c = fmt.charAt(1);
      for (var i = 2;i <= fmt.length - 2;i++)
	l = l * 10 + (fmt.charCodeAt (i) - "0".charCodeAt (0));
    }
  }
  var rem = l - n.length;
  for (var i = 0;i < rem;i++)
    n = c + n;
  return val_string (n);
}

// Caml name: format_float
// Type:      string -> float -> string
function caml_format_float (fmt, x) {
  /* FIXME: format unused */
  return val_string (float_val (x).toString (10));
}

// Caml name: int_of_string
// Type:      string -> int
function caml_int_of_string (s) {
  var res = parseInt (string_val (s));
  if (isNaN (res)) {
    caml_failwith ("int_of_string");
  } else {
    return res ;
  }
}

// Caml name: float_of_string
// Type:      string -> int
function caml_float_of_string (s) {
  return val_float (parseFloat (string_val (s)));
}

// Caml name: int_of_float
// Type:      float -> int
function caml_int_of_float (x) {
  return int_of_float (x);
}

// Caml name: float_of_int, float
// Type:      int -> float
function caml_float_of_int (x) {
  return float_of_int (x);
}

// Caml name: float_of_bits
// Type:      int64 -> float
function caml_int64_float_of_bits (i) {
  return float_of_bytes (int64_to_bytes (i));
}

/* OPERATIONS **********************************************************/

// Caml name: ( *. )
// Type:      float -> float -> float
function caml_mul_float (a, b) {
  return val_float (float_val (a) * float_val (b));
}

// Caml name: ( /. )
// Type:      float -> float -> float
function caml_div_float (a, b) {
return val_float (float_val (a) / float_val (b));
}

// Caml name: (~-.)
// Type:      float -> float
function caml_neg_float (a) {
return val_float (- float_val (b));
}
// Caml name: (+.)
// Type:      float -> float -> float
function caml_add_float (a, b) {
return val_float (float_val (a) + float_val (b));
}
// Caml name: (-.)
// Type:      float -> float -> float
function caml_sub_float (a, b) {
return val_float (float_val (a) - float_val (b));
}

// Caml name: ( ** )
// Type:      float -> float -> float
caml_power_float =
pow = function (a, b) {
return val_float (Math.pow (float_val (a), float_val (b)));
}

// Caml name: exp
// Type:      float -> float
caml_exp_float =
exp = function (a, b) {
return val_float (Math.exp (float_val (a), float_val (b)));
}

// Caml name: acos
// Type:      float -> float
caml_acos_float =
acos = function (x) {
return val_float (Math.acos (float_val (x)));
}

// Caml name: asin
// Type:      float -> float
caml_asin_float =
asin = function (x) {
return val_float (Math.asin (float_val (x)));
}

// Caml name: atan
// Type:      float -> float
caml_atan_float =
atan = function (x) {
return val_float (Math.atan (float_val (x)));
}

// Caml name: atan2
// Type:      float -> float -> float
caml_atan2_float =
atan2 = function (a, b) {
return val_float (Math.atan2 (float_val (a), float_val (b)));
}

// Caml name: cos
// Type:      float -> float
caml_cos_float =
cos = function (x) {
return val_float (Math.cos (float_val (x)));
}

// Caml name: cosh
// Type:      float -> float
caml_cosh_float =
cosh = function (x) {
x = float_val (x);
return val_float ((Math.exp (x) + Math.exp (-x)) / 2);
}

// Caml name: log
// Type:      float -> float
caml_log_float =
log = function (x) {
return val_float (Math.log (float_val (x)));
}

// Caml name: log10
// Type:      float -> float
caml_log10_float =
log10 = function () {
return val_float (Math.log (float_val (x)) / Math.log (10));
}

// Caml name: sin
// Type:      float -> float
caml_sin_float =
sin = function (x) {
return val_float (Math.sin (float_val (x)));
}

// Caml name: sinh
// Type:      float -> float
caml_sinh_float =
sinh = function (x) {
x = float_val (x);
return val_float ((Math.exp (x) - Math.exp (-x)) / 2);
}

// Caml name: sqrt
// Type:      float -> float
caml_sqrt_float =
sqrt = function (x) {
return val_float (Math.sqrt (float_val (x)));
}

// Caml name: tan
// Type:      float -> float
caml_tan_float =
tan = function (x) {
return val_float (Math.tan (float_val (x)));
}

// Caml name: tanh
// Type:      float -> float
caml_tanh_float =
tanh = function (x) {
return val_float (Math.tanh (float_val (x)));
}

// Caml name: ceil
// Type:      float -> float
caml_ceil_float =
ceil = function (x) {
return val_float (Math.ceil (float_val (x)));
}

// Caml name: floor
// Type:      float -> float
caml_floor_float =
floor = function (x) {
return val_float (Math.floor (float_val (x)));
}

// Caml name: unsafe_string
// Type:      string -> int -> int -> t
caml_md5_string = function (v, ofs, len) {
    var s = [];
    for (var i = 0;i < len;i++)
      s[i] = string_get (v, ofs + i);
    return array_of_js (md5 (s));
}

// Caml name: channel
caml_md5_chan = function (v, ofs, len) {
    caml_failwith ("not implemented in obrowser");
}

/* NOT FOR THE CASUAL USER ;-) *****************************************/

var o$named = {};
#define caml_named_value(vname) o$named[string_val (vname)]

// Caml name: register_named_value
// Type:      string -> 'a -> unit
function caml_register_named_value (vname, val) {
o$named[string_val (vname)] = val;
return UNIT;
}

// Type:      int -> unit
function caml_ensure_stack_capacity (required_space) {
/* FIXME: stack capacity */
return UNIT;
}

// Caml name: is_block
// Type:      t -> bool
function caml_obj_is_block (x) {
return val_bool (is_block (x));
}
// Caml name: is_int
// Type:      t -> bool
function caml_obj_is_int (x) {
return val_bool (is_long (x));
}
// Caml name: tag
// Type:      t -> int
function caml_obj_tag (x) {
return block_tag (x);
}
// Caml name: set_tag
// Type:      t -> int -> unit
function caml_obj_set_tag (x, tag) {
set_block_tag (x, tag);
return UNIT;
}
// Caml name: size
// Type:      t -> int
function caml_obj_size (x) {
return block_size (x);
}
// Caml name: field
// Type:      t -> int -> t
function caml_obj_field (x, i) {
return field (x, i);
}
// Caml name: set_field
// Type:      t -> int -> t -> unit
function caml_obj_set_field (x, i, v) {
store_field (x, i, v);
return UNIT;
}
// Caml name: new_block
// Type:      int -> int -> t
function caml_obj_block (tag, size) {
  return mk_block (size, tag);
}

// Caml name: dup
// Type:      t -> t
function caml_obj_dup (v) {
if (is_long (v)) return v;
return block_dup (v);
}

// Caml name: truncate
// Type:      t -> int -> unit
function caml_obj_truncate (x, size) {
set_block_size (x, size);
return UNIT;
}

// Caml name: ldexp
// Type:      float -> int -> float
function caml_ldexp_float (m, e) {
return val_float (m * Math.pow (2, float_val (e)));
}

// Caml name: frexp
// Type:      float -> float * int
function caml_frexp_float (v) {
var x = float_val (v);
var e = Math.ceil (Math.log (Math.abs (x)) / Math.log (2));
return pair (val_float (x * Math.pow (2, -e)), e);
}

caml_alloc_dummy_float = function (size) {
return mk_block (size, DOUBLE_ARRAY_TAG);
}

caml_alloc_dummy = function (size) {
return mk_block (size, 0);
}

function caml_update_dummy (dummy, newval) {
//    if (!is_block(dummy) || !is_block(newval) || dummy.size != newval.size) {
//	this.failwith ("caml_update_dummy");
//    }
for (var i = 0;i < block_size (dummy);i++) {
store_field (dummy, i, field (newval, i));
}
return UNIT;
}

/* FIXME: what what ? */

function caml_ml_out_channels_list () {
  return 0;
}

caml_primitive_stub ("caml_ml_output_char", 0);
caml_primitive_stub ("caml_ml_output", 0);
caml_primitive_stub ("caml_ml_flush", 0);
caml_primitive_stub ("caml_ml_flush_partial", 0);
