/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*                                                                     */
/*                        O'Browser II le retour                       */
/*             ReImplementation of the Graphics Library                */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/


/* JAVASCRIPT IMPLEMENTATION *******************************************/

// Object to represent a Graphics window.
// Several can coexist in the page.
function OCamlGraphics (width, height, container) {
  /* ML interface */
  this.color = 0; // same format as Graphics: _8 R8 G8 B8
  this.width = width;
  this.height = height;
  this.line_width = 1;
  // this.st stores the immediate state (for polling)
  // this.last_answer stores the state resulting of a listened event (for waiting)
  this.st = { button: false, x: 0, y: 0,keypressed: false, key: 0 };

  /* DOM interface */
  this.canvas = document.createElement ("canvas");
  this.canvas.setAttribute ("width", width);
  this.canvas.setAttribute ("height", height);
  var win = this;
  this.canvas.onmouseover = function (e) { win.grab_input (e) } ;
  this.canvas.onmouseout = function (e) { win.release_input (e) } ;
  this.canvas.onmousedown = function (e) { win.mousedown (e) } ;
  this.canvas.onmousemove = function (e) { win.mousemove (e) } ;
  this.canvas.onmouseup = function (e) { win.mouseup (e) } ;
}

OCamlGraphics.prototype.close = function () {
  if (this.canvas.parent)
    this.canvas.parent.removeChild (this.canvas);
}

OCamlGraphics.prototype.grab_input = function () {
  var win = this;
  window.onkeydown = function (e) { win.keydown (e) } ;
  window.onkeyup = function (e) { win.keyup (e) } ;
}

OCamlGraphics.prototype.release_input = function () {
  delete window.onkeydown ;
  delete window.onkeyup ;
}

OCamlGraphics.prototype.set_mask = function (mask) {
  this.mask = mask;
}

var MASK_BUTTON_DOWN  = 1
var MASK_BUTTON_UP    = 2
var MASK_KEY_PRESSED  = 4
var MASK_MOUSE_MOTION = 8
var MASK_POLL         = 16

OCamlGraphics.prototype.setxy = function (e) {
  if (! e.hasOwnProperty('offsetX')) {
    var t = e.target;
    this.st.x = e.layerX - t.offsetLeft;
    this.st.y = e.layerY - t.offsetTop;
    while (t.offsetParent) {
      t = t.offsetParent;
      this.st.x -= t.offsetLeft;
      this.st.y -= t.offsetTop;
    }
    this.st.y = this.height - this.st.y;
  } else {
    this.st.x = e.offsetX;
    this.st.y = this.height - (e.offsetY);
  }
}

OCamlGraphics.prototype.mouseup = function (e) {
  this.setxy (e);
  this.st.button = false;

  if (this.mask & MASK_BUTTON_UP) {
    this.last_answer = {
      x : this.st.x,
      y : this.st.y,
      button : true,
      keypressed : this.st.keypressed,
      key : this.st.key
    };
    this.mask = 0;
    notify_all (this);
  }
}

OCamlGraphics.prototype.mousedown = function (e) {
  this.setxy (e);
  this.st.button = true;

  if (this.mask & MASK_BUTTON_DOWN) {
    this.last_answer = {
      x : this.st.x,
      y : this.st.y,
      button : true,
      keypressed : this.st.keypressed,
      key : this.st.key
    };
    this.mask = 0;
    notify_all (this);
  }
}

OCamlGraphics.prototype.keydown = function (e) {
  function char_from_event (e) {
    if (e.charCode)
      return e.charCode;
    if (e.shiftKey)
      return e.keyCode;
    if (e.keyCode >= 65 && e.keyCode <= 90)
      return e.keyCode + 32;
    return e.keyCode;
  }
  this.st.keypressed = true;
  this.st.key = char_from_event (e);
}

OCamlGraphics.prototype.keyup = function (e) {
  this.st.keypressed = false;

  if (this.mask & MASK_KEY_PRESSED) {
    this.last_answer = {
      x : this.st.x,
      y : this.st.y,
      button : this.st.button,
      keypressed : true,
      key : this.st.key
    };
    this.mask = 0;
    notify_all (this);
  }
}

OCamlGraphics.prototype.mousemove = function (e) {
  this.setxy (e);

  if (this.mask & MASK_MOUSE_MOTION) {
    this.last_answer = {
      x : this.st.x,
      y : this.st.y,
      button : this.st.button,
      keypressed : this.st.keypressed,
      key : this.st.key
    };
    this.mask = 0;
    notify_all (this);
  }
}

OCamlGraphics.prototype.resize = function (w, h) {
  this.width = w;
  this.height = h;
  this.canvas.width = w;
  this.canvas.height = h;
}

OCamlGraphics.prototype.get_context = function () {
  if (this.ctx == null) {
    var ctx = this.canvas.getContext ("2d");
    this.font_size = 12;
    this.font = "Sans";
    ctx.font = this.font_size + "px " + this.font;
    var r = (this.color >> 16) & 0xFF;
    var g = (this.color >> 8) & 0xFF;
    var b = this.color & 0xFF;
    ctx.strokeStyle = ctx.fillStyle = "rgb("+r+","+g+","+b+")";
    ctx.lineWidth = 1;
    ctx.lineCap = "round";
    ctx.lineJoin = "round";
    ctx.save ();
    ctx.translate (0, this.height);
    ctx.scale (1, -1);
    ctx.translate (0.5, 0.5);
    this.ctx = ctx;
  }
  return this.ctx;
}

OCamlGraphics.prototype.get_canvas = function () {
  return this.canvas;
}

/* OCAML EXTERNALS *****************************************************/

function assert_graphics_opened () {
  if (!og$graphics) failwith ("Graphics window not opened") ;
}

// Caml name: raw_open_graph
// Type:      string -> unit
function caml_gr_open_graph (fmt) {
  var scan = / ([0-9]+)x([0-9]+)/.exec (string_val (fmt)) ;
  var width = fmt ? parseInt (scan[1]) : 100 ;
  var height = fmt ? parseInt (scan[2]) : 100 ;
  var container = document.getElementById ("camlgraphics");
  if (!container) {
    container = document.createElement ("div");
    og$graphics = new OCamlGraphics (width, height, container);
    document.body.appendChild (container);
    container.appendChild (og$graphics.get_canvas ());
  } else {
    og$graphics = new OCamlGraphics (width, height);
    container.appendChild (og$graphics.get_canvas ());
  }
  caml_gr_clear_graph ();
  caml_gr_set_color (0);
  return UNIT;
}

// Caml name: raw_close_graph
// Type:      unit -> unit
function caml_gr_close_graph (unit) {
  assert_graphics_opened ();
  og$graphics.close ();
  delete window.og$graphics;
  return UNIT;
}

// Caml name: set_window_title
// Type:      string -> unit
function caml_gr_set_window_title (t) {
  assert_graphics_opened ();
  og$graphics.set_title (s);
  return UNIT;
}

// Caml name: resize_window
// Type:      int -> int -> unit
function caml_gr_resize_window (w, h) {
  assert_graphics_opened ();
  og$graphics.resize (w, h);
  return UNIT;
}

// Caml name: clear_graph
// Type:      unit -> unit
function caml_gr_clear_graph (unit) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  ctx.strokeStyle = ctx.fillStyle = "white";
  ctx.fillRect (0, 0, og$graphics.width, og$graphics.height);
  return UNIT;
}

// Caml name: size_x
// Type:      unit -> int
function caml_gr_size_x (unit) {
  assert_graphics_opened ();
  return og$graphics.width;
}

// Caml name: size_y
// Type:      unit -> int
function caml_gr_size_y (unit) {
  assert_graphics_opened ();
  return og$graphics.height;
}

// Caml name: display_mode
// Type:      bool -> unit
caml_primitive_stub ("caml_gr_display_mode", UNIT);

// Caml name: remember_mode
// Type:      bool -> unit
caml_primitive_stub ("caml_gr_remember_mode", UNIT);

// Caml name: synchronize
// Type:      unit -> unit
caml_primitive_stub ("caml_gr_synchronize", UNIT);

// Caml name: sigio_signal
// Type:      unit -> int
caml_primitive_stub ("caml_gr_sigio_signal", 0);

// Caml name: sigio_handler
// Type:      int -> unit
caml_primitive_stub ("caml_gr_sigio_handler", UNIT);

// Caml name: set_color
// Type:      color -> unit
function caml_gr_set_color (color) {
  assert_graphics_opened ();
  og$graphics.color = color;
  var r = (color >> 16) & 0xFF;
  var g = (color >> 8) & 0xFF;
  var b = color & 0xFF;
  var ctx = og$graphics.get_context ();
  ctx.strokeStyle = ctx.fillStyle = "rgb("+r+","+g+","+b+")";
  return UNIT;
}

// Caml name: plot
// Type:      int -> int -> unit
function caml_gr_plot (x, y) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  ctx.fillRect (x - 0.5, y - 0.5, 1, 1);
  og$graphics.x = x;
  og$graphics.y = y;
  return UNIT;
}

// Caml name: point_color
// Type:      int -> int -> color
function caml_gr_point_color (x, y) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  var tmp = ctx.getImageData (x,og$graphics.height - y - 1,1,1);
  return (tmp.data[0] << 16) | (tmp.data[1] << 8) | tmp.data[2];
}

// Caml name: moveto
// Type:      int -> int -> unit
function caml_gr_moveto (x, y) {
  assert_graphics_opened ();
  og$graphics.x = x;
  og$graphics.y = y;
  return UNIT;
}

// Caml name: current_x
// Type:      unit -> int
function caml_gr_current_x (unit) {
  assert_graphics_opened ();
  return og$graphics.x;
}

// Caml name: current_y
// Type:      unit -> int
function caml_gr_current_y (unit) {
  assert_graphics_opened ();
  return og$graphics.y;
}

// Caml name: lineto
// Type:      int -> int -> unit
function caml_gr_lineto (x, y) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  ctx.beginPath ();
  ctx.moveTo (og$graphics.x, og$graphics.y);
  ctx.lineTo (x, y);
  ctx.stroke ();
  og$graphics.x = x;
  og$graphics.y = y;
}

// Caml name: raw_draw_rect
// Type:      int -> int -> int -> int -> unit
function caml_gr_draw_rect (x, y, w, h) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  ctx.beginPath ();
  ctx.moveTo (x,y);
  ctx.lineTo (x + w,y);
  ctx.lineTo (x + w,y + h);
  ctx.lineTo (x,y + h);
  ctx.lineTo (x,y);
  ctx.stroke ();
  return UNIT;
}

// Caml name: raw_draw_arc
// Type:      int -> int -> int -> int -> int -> int -> unit
function caml_gr_draw_arc (x,y,rx,ry,a1,a2) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  ctx.beginPath ();
  ctx.arc (x, y, rx, a1 * Math.PI / 180, a2 * Math.PI / 180, false);
  ctx.stroke ();
  return UNIT;
}

// Caml name: raw_set_line_width
// Type:      int -> unit
function caml_gr_set_line_width (lw) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  og$graphics.line_width = lw;
  ctx.lineWidth = lw;
  return UNIT;
}

// Caml name: raw_fill_rect
// Type:      int -> int -> int -> int -> unit
function caml_gr_fill_rect (x, y, w, h) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  ctx.fillRect (x - 0.5, y - 0.5, w, h);
  return UNIT;
}

// Caml name: fill_poly
// Type:      (int * int) array -> unit
function caml_gr_fill_poly (p) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  ctx.beginPath ();
  if (block_size (p) > 0)
    ctx.moveTo (fst (field (p, block_size (p) - 1)) - 0.5, snd (field (p, block_size (p) - 1)) - 0.5);
  for (var i = 0;i < block_size (p);i++) {
    ctx.lineTo (fst (field (p, i)) - 0.5, snd (field (p, i)) - 0.5);
  }
  ctx.fill ();
  return UNIT;
}

// Caml name: raw_fill_arc
// Type:      int -> int -> int -> int -> int -> int -> unit
function caml_gr_fill_arc (x,y,rx,ry,a1,a2) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  ctx.beginPath ();
  ctx.arc (x, y, rx, a1 * Math.PI / 180, a2 * Math.PI / 180, false);
  ctx.fill ();
  return UNIT;
}

// Caml name: draw_char
// Type:      char -> unit
function caml_gr_draw_char (c) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  ctx.save ();
  ctx.scale (1, -1);
  ctx.translate (0, -og$graphics.height);
  ctx.textBaseline = "baseline";
  var m = ctx.measureText (c);
  ctx.fillText (c, og$graphics.x, og$graphics.y);
  og$graphics.x += Math.round (m.width);
  og$graphics.y += Math.round (og$graphics.font_size);
  ctx.restore ();
  return UNIT;
}

// Caml name: draw_string
// Type:      string -> unit
function caml_gr_draw_string (s) {
  assert_graphics_opened ();
  s = string_val (s);
  var ctx = og$graphics.get_context ();
  ctx.save ();
  ctx.scale (1, -1);
  ctx.translate (0, -og$graphics.height);
  ctx.textBaseline = "baseline";
  var m = ctx.measureText (s);
  ctx.fillText (s, og$graphics.x, og$graphics.height - og$graphics.y);
  og$graphics.x += Math.round (m.width);
  og$graphics.y += Math.round (og$graphics.font_size);
  ctx.restore ();
  return UNIT;
}

// Caml name: set_font
// Type:      string -> unit
function caml_gr_set_font (s) {
  assert_graphics_opened ();
  og$graphics.font = string_val (s);
  var ctx = og$graphics.get_context ();
  ctx.font = og$graphics.font_size + "px " + og$graphics.font;
  return UNIT;
}

// Caml name: set_text_size
// Type:      int -> unit
function caml_gr_set_text_size (sz) {
  assert_graphics_opened ();
  og$graphics.font_size = sz;
  og$graphics.get_context ().font = og$graphics.font_size + "px " + og$graphics.font;
  return UNIT;
}

// Caml name: text_size
// Type:      string -> int * int
function caml_gr_text_size (s) {
  assert_graphics_opened ();
  s = string_val (s);
  var m = og$graphics.get_context ().measureText (s);
  return mk_block ([Math.round (m.width), Math.round (og$graphics.font_size)], 0);
}

// Caml name: make_image
// Type:      color array array -> image
function caml_gr_make_image (caa) {
  assert_graphics_opened ();
  /* FIXME: probably doesn't work anymore */
  var imgdata = new Object ();
  for (var y = 0;y < block_size (caa);y++) {
    var row = field (caa, y) ;
    for (var x = 0;x < block_size (row);x++) {
      var cell = field (row, x);
      imgdata.data[(y * imgdata.width + x) * 4 + 0] = (cell >> 16) & 0xFF;
      imgdata.data[(y * imgdata.width + x) * 4 + 0] = (cell >> 8) & 0xFF;
      imgdata.data[(y * imgdata.width + x) * 4 + 0] = cell & 0xFF;
      imgdata.data[(y * imgdata.width + x) * 4 + 0] = 0;
    }
  }
  return box_abstract (imgdata);
}

// Caml name: dump_image
// Type:      image -> color array array
function caml_gr_dump_image (img) {
  assert_graphics_opened ();
  var caa = mk_block (img.height, 0);
  for (var y = 0;y < img.height;y++) {
    var row = mk_block (img.width, 0);
    store_field (caa, y, row);
    for (var x = 0;x < img.width;x++) {
      store_field (row, x,
		   (img.data[(y * imgdata.width + x) * 4 + 0] << 16)
		   | (img.data[(y * imgdata.width + x) * 4 + 1] << 8)
		   | img.data[(y * imgdata.width + x) * 4 + 1]);
    }
  }
  return caa;
}

// Caml name: draw_image
// Type:      image -> int -> int -> unit
function caml_gr_draw_image (bimg, x, y) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  var img = unbox_abstract (bimg);
  ctx.putImageData (img, x, og$graphics.height - y - img.height);
  return UNIT;
}

// Caml name: create_image
// Type:      int -> int -> image
function caml_gr_create_image (w, h) {
  assert_graphics_opened ();
  /* FIXME: probably doesn't work anymore */
  var img = {
    width: w,
    height: h,
  };
  img.data = new Array ();
  for (var y = 0;y < h;y++)
    for (var x = 0;x < w;x++) {
      img.data[(y * w + x) * 4 + 0] = 0;
      img.data[(y * w + x) * 4 + 1] = 0;
      img.data[(y * w + x) * 4 + 2] = 0;
      img.data[(y * w + x) * 4 + 3] = 255;
    }
  return box_abstract (img);
}

// Caml name: blit_image
// Type:      image -> int -> int -> unit
function caml_gr_blit_image (bimg, x, y) {
  assert_graphics_opened ();
  var ctx = og$graphics.get_context ();
  var img = unbox_abstract (bimg);
  store_field (bimg, 0, ctx.getImageData (x, og$graphics.height - y - img.height, img.width, img.height));
  return UNIT;
}

// Caml name: wait_next_event
// Type:      event list -> status
function caml_gr_wait_event (evl) {
  assert_graphics_opened ();
  var mask = 0;
  while (is_block (evl)) {
    mask |= 1 << list_hd (evl, 0);
    evl = list_tl (evl, 1);
  }

  if (mask & MASK_POLL) {
    return (mk_block([
      og$graphics.st.x,
      og$graphics.st.y,
      og$graphics.st.button,
      og$graphics.st.keypressed,
      og$graphics.st.key]));
  } else {
    function gr_wait_cont () {
      if (!og$graphics.last_answer)
	wait_for (og$graphics, gr_wait_cont);
      var st = mk_block([
	og$graphics.last_answer.x,
	og$graphics.last_answer.y,
	og$graphics.last_answer.button,
	og$graphics.last_answer.keypressed,
	og$graphics.last_answer.key]);
      delete og$graphics.last_answer;
      return st;
    }
    og$graphics.set_mask (mask);
    wait_for (og$graphics, gr_wait_cont);
  }
}

// Caml name: sound
// Type:      int -> int -> unit
function caml_gr_sound (f, d) {
  assert_graphics_opened ();
  return UNIT;
}
