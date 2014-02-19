/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*                                                                     */
/*                        O'Browser II le retour                       */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/


/* CUSTOMS INFRASTRUCTURE **********************************************/
/*
 * Custom blocks use an 'ops' JS object to decribe the specific
 * operations using the following fields :
 *  - id (string): a unique ID as with the C ffi
 *  - compare (value -> value -> int): a comparison function
 *  - hash (value -> int): a hashing function
 *  - serialize/deserialize: marshalling functions, see marshall.js
 */

var o$custom_ops = {};

/* register an ops object for a specific id */
function register_custom (ops) {
  o$assert (ops["id"], "custom ops must define field 'id'");
  var req = ["compare", "hash", "serialize", "deserialize"];
  for (i in req)
    o$assert (ops[req[i]], "custom ops '" + ops.id + "' must define field '" + req[i] + "'");
  o$custom_ops[ops.id] = ops;
}

/* find the ops object registered for a specific id */
function find_custom (id) {
  o$assert (o$custom_ops[id], "no such custom '" + id + "'");
  return o$custom_ops[id];
}

/* build a custom block from an ops object (not its id) and a JS value */
function mk_custom (ops, val) {
  return mk_block ([ops, val], CUSTOM_TAG);
}

/* ops object of a custom block */
function custom_ops (val) {
  return field (val, 0);
}

/* JS value of a custom block */
function custom_val (val) {
  return field (val, 1);
}

/* INT32 ***************************************************************/

function val_int32 (t) { return mk_custom (int32_ops, t); }
function int32_val (v) { return custom_val (v); }

int32_ops = {
  id : "_i",
  compare :  function (a,b) {
    return (custom_val (a) - custom_val (b));
  },
  hash : function (a) {
    return a;
  },
  serialize : function (v, writer) {
    writer.write (8, (custom_val (v) >> 24) & 0xFF);
    writer.write (8, (custom_val (v) >> 16) & 0xFF);
    writer.write (8, (custom_val (v) >> 8) & 0xFF);
    writer.write (8, custom_val (v) & 0xFF);
    writer.size_32 += 2 + ((4 + 3) >> 2);
    writer.size_64 += 2 + ((4 + 7) >> 3);
  },
  deserialize : function (reader) {
    return mk_custom (int32_ops, reader.read32u ());
  }
};

register_custom (int32_ops);

// Caml name: Int32.neg
// Caml type: int32 -> int32
function caml_int32_neg (a) {
  var a = custom_val (a);
  return val_int32 ((-a) & -1);
}
// Caml name: Int32.add
// Caml type: int32 -> int32 -> int32
function caml_int32_add (a,b) {
  return val_int32 ((custom_val (a) + custom_val (b)) & -1);
}
// Caml name: Int32.sub
// Caml type: int32 -> int32 -> int32
function caml_int32_sub (a,b) {
  return val_int32 ((custom_val (a) - custom_val (b)) & -1);
}
// Caml name: Int32.mul
// Caml type: int32 -> int32 -> int32
function caml_int32_mul (a,b) {
  /* FIXME */
  return val_int32 ((custom_val (a) * custom_val (b)) & -1);
}
// Caml name: Int32.div
// Caml type: int32 -> int32 -> int32
function caml_int32_div (a,b) {
  /* FIXME */
  return val_int32 ((custom_val (a) / custom_val (b)) & -1);
}
// Caml name: Int32.rem
// Caml type: int32 -> int32 -> int32
function caml_int32_mod (a,b) {
  /* FIXME */
  return val_int32 ((custom_val (a) % custom_val (b)) & -1);
}
// Caml name: Int32.logand
// Caml type: int32 -> int32 -> int32
function caml_int32_and (a,b) {
  return val_int32 (custom_val (a) & custom_val (b));
}
// Caml name: Int32.logor
// Caml type: int32 -> int32 -> int32
function caml_int32_or (a,b) {
  return val_int32 (custom_val (a) | custom_val (b));
}
// Caml name: Int32.logxor
// Caml type: int32 -> int32 -> int32
function caml_int32_xor (a,b) {
  return val_int32 (custom_val (a) ^ custom_val (b));
}
// Caml name: Int32.shift_left
// Caml type: int32 -> int -> int32
function caml_int32_lsl (a,b) {
  return val_int32 ((custom_val (a) << b) & -1);
}
// Caml name: Int32.shift_right
// Caml type: int32 -> int -> int32
function caml_int32_asr (a,b) {
  return val_int32 (custom_val (a) >> b);
}
// Caml name: Int32.shift_right_logical
// Caml type: int32 -> int -> int32
function caml_int32_lsr (a,b) {
  return val_int32 (custom_val (a) >>> b);
}
// Caml name: Int32.of_int
// Caml type: int -> int32
function caml_int32_of_int (a) {
  return val_int32 (a);
}
// Caml name: Int32.to_int
// Caml type: int32 -> int
function caml_int32_to_int (a) {
  return val_int (custom_val (a));
}
// Caml_name: Int32.of_float
// Caml type: float -> int32
caml_int32_of_float = int32_of_float = function (x) {
  return val_int32 (new Int32 (Math.floor (x)));
}
// Caml_name: Int32.to_float
// Caml type: int32 -> float
caml_int32_to_float = int32_to_float = function (x) {
  return val_float (custom_val (x).lo + custom_val (x).hi * (1 << 30) * 4);
}
// Caml name: Int32.format
// Caml type: string -> int32 -> string
function caml_int32_format (f, v) {
  return caml_format_int.call (this, f, custom_val (v));
}
// Caml name: Int32.of_string
// Caml type: string -> int32
function caml_int32_of_string (s) {
  return val_int32 (caml_int_of_string.call (this, s));
}


/* NATIVEINT ***********************************************************/

/* to and fro JS numbers */
function val_nativeint (t) { return mk_custom (nativeint_ops, t); }
function nativeint_val (v) { return custom_val (v); }

nativeint_ops = {
  id : "_n",
  compare : function (a,b) {
    return (custom_val (a) - custom_val (b));
  },
  hash : function (a) {
    return a;
  },
  serialize : function (v, writer) {
    writer.write (8, 1);
    writer.write (8, (custom_val (v) >> 24) & 0xFF);
    writer.write (8, (custom_val (v) >> 16) & 0xFF);
    writer.write (8, (custom_val (v) >> 8) & 0xFF);
    writer.write (8, custom_val (v) & 0xFF);
    writer.size_32 += 2 + ((5 + 3) >> 2);
    writer.size_64 += 2 + ((5 + 7) >> 3);
  },
  deserialize : function (reader) {
    var l = reader.read8u ();
    if (l == 1)
      return val_nativeint (reader.read32u ());
    else
      throw new Error ("> 32 bits native int not supported yet");
  }
};

register_custom (nativeint_ops);

// Caml name: Nativeint.neg
// Caml type: nativeint -> nativeint
function caml_nativeint_neg (a) {
  return val_nativeint ((-custom_val (a)) & -1);
}
// Caml name: Nativeint.add
// Caml type: nativeint -> nativeint -> nativeint
function caml_nativeint_add (a,b) {
  return val_nativeint ((custom_val (a) + custom_val (b)) & -1);
}
// Caml name: Nativeint.sub
// Caml type: nativeint -> nativeint -> nativeint
function caml_nativeint_sub (a,b) {
  return val_nativeint ((custom_val (a) - custom_val (b)) & -1);
}
// Caml name: Nativeint.mul
// Caml type: nativeint -> nativeint -> nativeint
function caml_nativeint_mul (a,b) {
  return val_nativeint ((custom_val (a) * custom_val (b)) & -1);
}
// Caml name: Nativeint.div
// Caml type: nativeint -> nativeint -> nativeint
function caml_nativeint_div (a,b) {
  return val_nativeint ((custom_val (a) / custom_val (b)) & -1);
}
// Caml name: Nativeint.rem
// Caml type: nativeint -> nativeint -> nativeint
function caml_nativeint_mod (a,b) {
  return val_nativeint ((custom_val (a) % custom_val (b)) & -1);
}
// Caml name: Nativeint.logand
// Caml type: nativeint -> nativeint -> nativeint
function caml_nativeint_and (a,b) {
  return val_nativeint (custom_val (a) & custom_val (b));
}
// Caml name: Nativeint.logor
// Caml type: nativeint -> nativeint -> nativeint
function caml_nativeint_or (a,b) {
  return val_nativeint (custom_val (a) | custom_val (b));
}
// Caml name: Nativeint.logxor
// Caml type: nativeint -> nativeint -> nativeint
function caml_nativeint_xor (a,b) {
  return val_nativeint (custom_val (a) ^ custom_val (b));
}
// Caml name: Nativeint.shift_left
// Caml type: nativeint -> int -> nativeint
function caml_nativeint_shift_left (a,b) {
  return val_nativeint ((custom_val (a) << b) & -1);
}
// Caml name: Nativeint.shift_right
// Caml type: nativeint -> int -> nativeint
function caml_nativeint_shift_right (a,b) {
  return val_nativeint (custom_val (a) >> b);
}
// Caml name: Nativeint.shift_right_logical
// Caml type: nativeint -> int -> nativeint
function caml_nativeint_shift_right_logical (a,b) {
  return val_nativeint (custom_val (a) >>> b);
}
// Caml name: Nativeint.of_int
// Caml type: int -> nativeint
function caml_nativeint_of_int (a) {
  return val_nativeint (a);
}
// Caml name: Nativeint.to_int
// Caml type: nativeint -> int
function caml_nativeint_to_int (a) {
  return val_int (custom_val (a));
}
// Caml name: Nativeint.of_int
// Caml type: int -> nativeint
function caml_nativeint_of_int32 (a) {
  return val_nativeint (custom_val (a));
}
// Caml name: Nativeint.to_int
// Caml type: nativeint -> int
function caml_nativeint_to_int32 (a) {
  return val_int32 (nativeint_val (a));
}
// Caml_name: Nativeint.of_float
// Caml type: float -> nativeint
caml_nativeint_of_float = nativeint_of_float = function (x) {
  return val_nativeint (new Nativeint (Math.floor (x)));
}
// Caml_name: Nativeint.to_float
// Caml type: nativeint -> float
caml_nativeint_to_float = nativeint_to_float = function (x) {
  return val_float (custom_val (x).lo + custom_val (x).hi * (1 << 30) * 4);
}
// Caml name: Nativeint.format
// Caml type: string -> nativeint -> string
function caml_nativeint_format (f, v) {
  return caml_format_int.call (this, f, nativeint_val (v));
}
// Caml name: Nativeint.of_string
// Caml type: string -> nativeint
function caml_nativeint_of_string (s) {
  return val_nativeint (caml_int_of_string.call (this, s));
}

/* INT64 (requires Int64.js) *******************************************/

/* to and fro JS numbers */
function val_int64 (t) { return mk_custom (int64_ops, t); }
function int64_val (v) { return custom_val (v); }

int64_ops = {
  id : "_j",
  compare : function (a,b) {
    return int64_compare (a, b);
  },
  hash : function (a) {
    return custom_val (a).lo;
  },
  serialize : function (v, writer) {
    var t = int64_to_bytes (v);
    for (var j = 0;j < 8;j++)
      writer.write (8, t[j]);
    writer.size_32 += 2 + ((8 + 3) >> 2);
    writer.size_64 += 2 + ((8 + 7) >> 3);
  },
  deserialize : function (reader) {
    var t = [];
    for (var j = 0;j < 8;j++)
      t[j] = reader.read8u();
    return int64_of_bytes (t);
  }
};

register_custom (int64_ops);

caml_int64_compare = int64_compare = function (a, b) {
  return custom_val (a).compareTo (custom_val (b));
}
// Caml_name: Int64.add
// Caml type: int64 -> int64 -> int64
caml_int64_add = int64_add = function (a, b) {
  return val_int64 (custom_val (a).add (custom_val (b)));
}
// Caml_name: Int64.neg
// Caml type: int64 -> int64
caml_int64_neg = int64_neg = function (a) {
  return val_int64 (custom_val (a).neg ());
}
// Caml_name: Int64.sub
// Caml type: int64 -> int64 -> int64
caml_int64_sub = int64_sub = function (a, b) {
  return val_int64 (custom_val (a).sub (custom_val (b)));
}
// Caml_name: Int64.mul
// Caml type: int64 -> int64 -> int64
caml_int64_mul = int64_mul = function (a, b) {
  return val_int64 (custom_val (a).mul (custom_val (b)));
}
// Caml_name: Int64.div
// Caml type: int64 -> int64 -> int64
caml_int64_div = int64_div = function (a, b) {
  return val_int64 (custom_val (a).div (custom_val (b)));
}
// Caml_name: Int64.rem
// Caml type: int64 -> int64 -> int64
caml_int64_mod = int64_mod = function (a, b) {
  return val_int64 (custom_val (a).mod (custom_val (b)));
}
// Caml_name: Int64.logand
// Caml type: int64 -> int64 -> int64
caml_int64_and = int64_and = function (a, b) {
  return val_int64 (custom_val (a).and (custom_val (b)));
}
// Caml_name: Int64.logor
// Caml type: int64 -> int64 -> int64
caml_int64_or = int64_or = function (a, b) {
  return val_int64 (custom_val (a).or (custom_val (b)));
}
// Caml_name: Int64.logxor
// Caml type: int64 -> int64 -> int64
caml_int64_xor = int64_xor = function (a, b) {
  return val_int64 (custom_val (a).xor (custom_val (b)));
}
// Caml_name: Int64.shift_left
// Caml type: int64 -> int -> int64
caml_int64_lsl = int64_lsl = function (a, b) {
  return val_int64 (custom_val (a).lsl (custom_val (b)));
}
// Caml_name: Int64.shift_right
// Caml type: int64 -> int -> int64
caml_int64_lsr = int64_lsr = function (a, b) {
  return val_int64 (custom_val (a).lsr (custom_val (b)));
}
// Caml_name: Int64.shift_right_logical
// Caml type: int64 -> int -> int64
caml_int64_asr = int64_asr = function (a, b) {
  return val_int64 (custom_val (a).asr (custom_val (b)));
}
// Caml_name: Int64.of_int32
// Caml type: int32 -> int64
caml_int64_of_int32 = int64_of_int32 = function (x) {
  return val_int64 (new Int64 (custom_val (x)));
}
// Caml_name: Int64.to_int32
// Caml type: int64 -> int32
caml_int64_to_int32 = int64_to_int32 = function (x) {
  return val_custom (int32_ops, custom_val (x).lo); // FIXME: precision
}
// Caml_name: Int64.of_nativeint
// Caml type: nativeint -> int64
caml_int64_of_nativeint = int64_of_nativeint = function (x) {
  return val_int64 (new Int64 (custom_val (x)));
}
// Caml_name: Int64.to_nativeint
// Caml type: int64 -> nativeint
caml_int64_to_nativeint = int64_to_nativeint = function (x) {
  return val_custom (nativeint_ops, custom_val (x).lo); // FIXME: precision
}
// Caml_name: Int64.of_int
// Caml type: int -> int64
caml_int64_of_int = int64_of_int = function (x) {
  return val_int64 (new Int64 (x));
}
// Caml_name: Int64.to_int
// Caml type: int64 -> int
caml_int64_to_int = int64_to_int = function (x) {
  return val_int (custom_val (x).lo); // FIXME: precision
}
// Caml_name: Int64.of_float
// Caml type: float -> int64
caml_int64_of_float = int64_of_float = function (x) {
  return val_int64 (new Int64 (Math.floor (x))); // FIXME: precision
}
// Caml_name: Int64.to_float
// Caml type: int64 -> float
caml_int64_to_float = int64_to_float = function (x) {
  return val_float (custom_val (x).lo + custom_val (x).hi * (1 << 30) * 4);
}
// Caml_name: Int64.format
// Caml type: string -> int64 -> string
function caml_int64_format (fmt, x) {
  var fmt = string_from_value (fmt);
  var t = fmt[fmt.length - 1];
  var n = int64_val (x).toString (t == 'd' ? 10 : (t == 'o' ? 8 : (t == 'x' ? 16 : 10 /* default */)));
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
  return value_from_string (n);
}
// Caml_name: Int64.of_string
// Caml type: string -> int64
function caml_int64_of_string (s) {
  return val_int64 (parseInt64 (s));
}

function int64_of_bytes (bytes) {
  var lo = bytes[7] | (bytes[6] << 8) | (bytes[5] << 16) | (bytes[4] << 24);
  var hi = bytes[3] | (bytes[2] << 8) | (bytes[1] << 16) | (bytes[0] << 24);
  return val_int64 (new Int64 (lo, hi));
}
function int64_to_bytes (v) {
  return [
    (custom_val (v).hi >> 24) & 0xFF,
    (custom_val (v).hi >> 16) & 0xFF,
    (custom_val (v).hi >> 8) & 0xFF,
    (custom_val (v).hi >> 0) & 0xFF,
    (custom_val (v).lo >> 24) & 0xFF,
    (custom_val (v).lo >> 16) & 0xFF,
    (custom_val (v).lo >> 8) & 0xFF,
    (custom_val (v).lo >> 0) & 0xFF
  ];
}
