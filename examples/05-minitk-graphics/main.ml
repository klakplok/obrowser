open Graphics

type geom = {
  x: int ; y: int ; w : int ; h: int ;
}

module type PALETTE = sig
  val hilight_color : color
  val shadow_color : color
  val background_color : color
  val text_color : color
end

module type SKIN = sig
  module Palette : PALETTE
  val button_size : int * int -> int * int
  val draw_button : geom -> geom
  val draw_button_hilight : geom -> geom
  val input_size : int * int -> int * int
  val draw_input : geom -> geom
  val draw_input_hilight : geom -> geom
  val frame_size : int * int -> int * int
  val frame_inside : geom -> geom
  val draw_frame : geom -> geom
  val label_size : int * int -> int * int
  val draw_label : geom -> geom
end

module Make (Skin : SKIN) = struct
  module Palette = Skin.Palette

  exception Exit
  exception Redraw

  type g = geom = {
  x: int ; y: int ; w : int ; h: int ;
}

  type 'a ui_item = {
    size: unit -> int * int ;
    redraw:  geom -> unit ;
    click:  geom -> int -> int -> 'a option ;
    enter:  geom -> unit ;
    leave:  geom -> unit ;
    move: geom -> int -> int -> unit ;
    press:  geom -> char -> 'a option ;
  }

type 'a ui_layout_contents =
| Absolute of geom * 'a ui_layout
| Centered of 'a ui_layout
| Vbox of bool * 'a ui_layout list
| Hbox of bool * 'a ui_layout list
| Frame of 'a ui_layout
| Setable of 'a ui_layout ref
| Item of 'a ui_item
| Stretch of 'a ui_layout

and 'a ui_layout = {
  layout : 'a ui_layout_contents ;
  mutable where : geom
}

let redraw_ui layout =
  let rec redraw_layout layout =
    (*set_color green ;*)
    (*draw_rect layout.where.x layout.where.y layout.where.w layout.where.h ;*)
    match layout.layout with
    | Frame layout' ->
      ignore (Skin.draw_frame layout.where) ;
      redraw_layout layout'
    | Absolute (_, layout)
    | Setable { contents = layout }
    | Stretch layout
    | Centered layout ->
      redraw_layout layout
    | Vbox (_, layouts) | Hbox (_, layouts) ->
      List.iter redraw_layout layouts
    | Item item -> item.redraw layout.where
  in     
  redraw_layout layout

let compute_ui layout =
  (* step one: sizes -> where.w ad where.h give the minimal sizes *)
  let rec size_layout layout =
    match layout.layout with
    | Vbox (_, layouts) ->
      let w, h =
	List.fold_left
	  (fun (rw, rh) layout' ->
	    let w, h = size_layout layout' in
	    (max w rw, h + rh))
	  (0, 0)
	  layouts
      in
      layout.where <- { x = 0 ; y = 0 ; w = w ; h = h } ;
      (w, h)
    | Hbox (_, layouts) ->
      let w, h =
	List.fold_left
	  (fun (rw, rh) layout' ->
	    let w, h = size_layout layout' in
	    (w + rw, max h rh))
	  (0, 0)
	  layouts
      in
      layout.where <- { x = 0 ; y = 0 ; w = w ; h = h } ;
      (w, h)
    | Frame layout' ->
      let w, h = Skin.frame_size (size_layout layout') in
      layout.where <- { x = 0 ; y = 0 ; w = w ; h = h } ;
      (w, h)      
    | Absolute (_, layout')
    | Stretch layout'
    | Setable { contents = layout' }
    | Centered layout' ->
      let w, h = size_layout layout' in
      layout.where <- { x = 0 ; y = 0 ; w = w ; h = h } ;
      (w, h)
    | Item item -> 
      let w, h = item.size () in
      layout.where <- { x = 0 ; y = 0 ; w = w ; h = h } ;
      (w, h)
  in
  (* step two: relocate using minimal sizes *)
  let rec relocate_layout layout =
    match layout.layout with
    | Vbox (_, layouts) ->
      let y = ref 0 in
      let hstretch =
	let nb = List.fold_left (fun n c -> match c.layout with Stretch _ -> n + 1 | _ -> n) 0 layouts in
	let total = List.fold_left (fun r l -> r + l.where.h) 0 layouts in
	if nb > 0 then max 0 ((layout.where.h - total) / nb) else 0
      in
      List.iter
	(fun child ->
	  let dh = match child.layout with Stretch _ -> hstretch | _ -> 0 in
	  child.where <- { x = layout.where.x ;
			   y = layout.where.y + !y ;
			   w = layout.where.w ;
			   h = child.where.h + dh } ;
	  relocate_layout child ;
	  y := !y + child.where.h)
	(List.rev layouts)
    | Hbox (_, layouts) ->
      let x = ref 0 in
      let wstretch =
	let nb = List.fold_left (fun n c -> match c.layout with Stretch _ -> n + 1 | _ -> n) 0 layouts in
	let total = List.fold_left (fun r l -> r + l.where.w) 0 layouts in
	if nb > 0 then max 0 ((layout.where.w - total) / nb) else 0
      in
      List.iter
	(fun child ->
	  let dw = match child.layout with Stretch _ -> wstretch | _ -> 0 in
	  child.where <- { x = layout.where.x + !x ;
			   y = layout.where.y ;
			   w = child.where.w + dw ;
			   h = layout.where.h } ;
	  relocate_layout child ;
	  x := !x + child.where.w)
	layouts
    | Setable { contents = layout' }
    | Stretch layout' ->
      layout'.where <- layout.where ;
      relocate_layout layout'
    | Frame layout' ->
      layout'.where <- Skin.frame_inside layout.where ;
      relocate_layout layout'
    | Absolute (geom, layout') ->
      layout'.where <- { x = layout.where.x + geom.w ;
			 y = layout.where.y + geom.h ;
			 w = geom.w ;
			 h = geom.h } ;
      relocate_layout layout'
    | Centered layout' ->
      layout'.where <- { x = layout.where.x + layout.where.w / 2 - layout'.where.w / 2;
			 y = layout.where.y + layout.where.h / 2 - layout'.where.h / 2;
			 w = layout'.where.w ;
			 h = layout'.where.h } ;
      relocate_layout layout'
    | Item item -> ()
  in
  ignore (size_layout layout) ;
  layout.where <- { x = 0 ; y = 0 ; w = size_x () ; h = size_y () } ;
  relocate_layout layout
 
let locate_ui layout x y =
  let res = ref None in
  let rec locate_layout layout =
    match layout.layout with
    | Stretch layout
    | Frame layout
    | Setable { contents = layout }
    | Absolute (_, layout)
    | Centered layout ->
      locate_layout layout
    | Vbox (_, layouts) | Hbox (_, layouts) ->
      List.iter locate_layout layouts
    | Item item ->
      if x >= layout.where.x && x <= layout.where.x + layout.where.w
	&& y >= layout.where.y && y <= layout.where.y + layout.where.h then (
	res := Some (item, layout.where) ;
	raise Exit
      )
  in
  try
    locate_layout layout ;
    None
  with Exit -> !res

let button label callback =
  let wt, ht = text_size (if label = "" then "|" else label) in
  let draw geom =
    let geom = Skin.draw_button geom in
    set_color Palette.text_color ;
    moveto (geom.x + geom.w/2 - wt/2) (geom.y + geom.h/2 - ht/2) ;
    draw_string label
  in
  let draw_hilight geom =
    let geom = Skin.draw_button_hilight geom in
    set_color Palette.text_color ;
    moveto (geom.x + geom.w/2 - wt/2) (geom.y + geom.h/2 - ht/2) ;
    draw_string label
  in
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout =  Item { size = (fun () -> Skin.button_size (wt, ht)) ;
		     redraw = (fun geom -> draw geom) ;
		     click = (fun geom _ _ -> callback ()) ;
		     enter = (fun geom -> draw_hilight geom) ;
		     move = (fun geom x y -> ()) ;
		     leave = (fun geom -> draw geom) ;
		     press = (fun geom _ -> None) } }

let field text width =
  let wt, ht = text_size "x" in
  let wt, ht = wt * width, ht in
  let add_char k =
    if Char.code k >= 0x20 then
      text := !text ^ String.make 1 k
    else if Char.code k = 0x08 && !text <> "" then
      text := String.sub !text 0 (String.length !text -1)
    else ()
  in
  let draw geom =
    let geom = Skin.draw_input geom in
    set_color Palette.text_color ;
    moveto geom.x geom.y ;
    draw_string !text ;
  in
  let draw_hilight geom =
    let geom = Skin.draw_input_hilight geom in
    let wt, ht = text_size !text in
    set_color Palette.text_color ;
    moveto geom.x geom.y ;
    draw_string !text ;
    moveto (geom.x + wt + 2) (geom.y) ;
    rlineto 0 geom.h
  in
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout =  Item { size = (fun () -> Skin.input_size (wt, ht)) ;
		     redraw = (fun geom -> draw geom) ;
		     click = (fun geom _ _ -> None) ;
		     move = (fun geom x y -> ()) ;
		     enter = (fun geom -> draw_hilight geom) ;
		     leave = (fun geom -> draw geom) ;
		     press = (fun geom k -> add_char k ; draw_hilight geom ; None) } }

let label ?(centered = true) ?(colors = (Palette.background_color, Palette.text_color)) ?(border = (black, 0)) label =
  let wt, ht = text_size label in
  let redraw geom =
    let geom = Skin.draw_label geom in
    set_color (fst colors) ;
    fill_rect geom.x geom.y geom.w geom.h ;
    if snd border > 0 then (
      set_line_width (snd border) ;
      set_color (fst border) ;
      draw_rect geom.x geom.y geom.w geom.h ;
    ) ;
    set_color (snd colors) ;
    if centered then
      moveto (geom.x + geom.w/2 - wt/2) (geom.y + geom.h/2 - ht/2)
    else
      moveto geom.x (geom.y + geom.h/2 - ht/2) ;
    draw_string label
  in
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout =  Item { size = (fun () -> Skin.label_size (wt, ht)) ;
		     redraw = (fun geom -> redraw geom) ;
		     click = (fun geom _ _ -> None) ;
		     move = (fun geom x y -> ()) ;
		     enter = (fun geom -> ()) ;
		     leave = (fun geom -> ()) ;
		     press = (fun geom _ -> None) } }

let dummy () =
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout =  Item { size = (fun () -> (1, 1)) ;
		     redraw = (fun geom -> ()) ;
		     click = (fun geom _ _ -> None) ;
		     move = (fun geom x y -> ()) ;
		     enter = (fun geom -> ()) ;
		     leave = (fun geom -> ()) ;
		     press = (fun geom _ -> None) } }

let custom ?size ?redraw ?click ?move ?enter ?leave ?press () =
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout =  Item { size = (match size with None -> (fun () -> (1, 1)) | Some f -> f) ;
		     redraw = (match redraw with None -> (fun geom -> ()) | Some f -> f);
		     click = (match click with None -> (fun geom _ _ -> None) | Some f -> f) ;
		     move = (match move with None -> (fun geom x y -> ()) | Some f -> f) ;
		     enter = (match enter with None -> (fun geom -> ()) | Some f -> f) ;
		     leave = (match leave with None -> (fun geom -> ()) | Some f -> f) ;
		     press = (match press with None -> (fun geom k -> None) | Some f -> f) } }

let stretch child =
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout = Stretch child }

let glue () =
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout = Stretch (dummy ()) }

let absolute x y w h child =
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout = Absolute ({ x = x ; y = y ; w = w ; h = h }, child) }

let centered child =
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout = Centered child }

let vbox ?(fill = false) children =
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout = Vbox (fill, children) }

let hbox ?(fill = true) children =
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout = Hbox (fill, children) }

let frame child =
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout = Frame child }

let setable child =
  { where = { x = 0 ; y = 0 ; w = 0 ; h = 0 } ;
    layout = Setable (ref child) }

let set_setable layout child =
  match layout.layout with
  | Setable r -> r := child
  | _ -> invalid_arg "set_setable"

let run_ui ?(idle = (fun () -> None)) ui =
  synchronize () ;
  let gbuf = get_image 0 0 (size_x ()) (size_y ()) in
  auto_synchronize false ;
  compute_ui ui ;
  redraw_ui ui ;
  synchronize () ;
  let delay t = Thread.delay t in
  let current = ref None in
  let res = ref None in
  let win_size = ref (size_x (), size_y ()) in
  let set_res = function None -> () | Some r -> res := Some r ; raise Exit in
  try
    while true do
      delay 0.02 ;
      try
	let (winw, winh) as nwin_size = (size_x (), size_y ()) in
	if !win_size <> nwin_size then (win_size := nwin_size ; raise Redraw) ;
	set_res (idle ()) ;
	let st = wait_next_event [ Poll ; Button_down ; Key_pressed ; Mouse_motion ] in
	begin match !current, locate_ui ui st.mouse_x st.mouse_y with
	| None, None -> ()
	| Some (o, go, _, _), None ->
	  o.leave go ;
	  current := None
	| Some (o, go, x, y), Some (o', go') when o == o' ->
	  if st.keypressed then (
	    ignore (wait_next_event [ Key_pressed ]) ;
	    set_res (o.press go st.key)
	  ) ;
	  if st.button && st.mouse_y >= 0 && st.mouse_y < winh && st.mouse_x >= 0 && st.mouse_x < winw then (
	    while (wait_next_event [ Poll ; Button_down ; Button_up ]).button do delay 0.005 done ;
	    set_res (o.click go st.mouse_x st.mouse_y)
	  ) ;
	  if x <> st.mouse_x || y <> st.mouse_y then (
	    o.move go st.mouse_x st.mouse_y ;
	    current := Some (o, go, st.mouse_x, st.mouse_y)
	  )
	| Some (o, go, _, _), Some (o', go') ->
	  o.leave go ;
	  o'.enter go' ;
	  current := Some (o', go', st.mouse_x, st.mouse_y)
	| None, Some (o, go) ->
	  o.enter go ;
	  current := Some (o, go, st.mouse_x, st.mouse_y)
	end ;
	synchronize ()
      with
      | Redraw ->
	clear_graph () ;
	draw_image gbuf 0 0 ;
	compute_ui ui ;
	redraw_ui ui ;
	current := None ;
	synchronize ()
    done ; assert false
  with Exit ->
    draw_image gbuf 0 0 ;
    !res

let run_dialog ?(idle = (fun () -> None)) text buttons =
  run_ui ~idle (centered (frame (vbox [ label text ; hbox buttons ])))
end

module Platinium : SKIN = struct
  module Palette = struct
    let hilight_color = rgb 160 200 240
    let shadow_color = rgb 100 100 100
    let background_color = rgb 255 255 255
    let text_color = rgb 0 0 0
  end
    
  open Palette

  let padding = 5
  let button_size (w, h) = (w + 4 * padding, h + 4 * padding)
  let input_size (w, h) = (w + 4 * padding, h + 4 * padding)
  let frame_size (w, h) = (w + 2 * padding, h + 2 * padding)
  let label_size (w, h) = (w + 2 * padding, h + 2 * padding)

  let draw_rounded_rect ?(radius = 5) x y width height =
    let radius = min (height / 3) radius in
    let radius = min (width / 3) radius in
    moveto (x + radius) y ; lineto (x + width - radius) y ;
    moveto (x + radius) (y + height) ; lineto (x + width - radius) (y + height) ;
    moveto x (y + radius) ; lineto x (y + height - radius) ;
    moveto (x + width) (y + radius) ; lineto (x + width) (y + height - radius) ;
    draw_arc (x + radius) (y + radius) radius radius 180 270 ;
    draw_arc (x + width - radius) (y + height - radius) radius radius 0 90 ;
    draw_arc (x + radius) (y + height - radius) radius radius 90 180 ;
    draw_arc (x + width - radius) (y + radius) radius radius (-90) 0
      
  let fill_rounded_rect ?(radius = 5) x y width height =
    let radius = min (height / 3) radius in
    let radius = min (width / 3) radius in
    fill_rect (x + radius) y (width - 2 * radius) height ;
    fill_rect x (y + radius) width (height - 2 * radius) ;
    fill_arc (x + radius) (y + radius) radius radius 180 270 ;
    fill_arc (x + width - radius) (y + height - radius) radius radius 0 90 ;
    fill_arc (x + radius) (y + height - radius) radius radius 90 180 ;
    fill_arc (x + width - radius) (y + radius) radius radius (-90) 0

  let frame_inside geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    { x = x ; y = y ; w = w ; h = h }

  let draw_frame geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    set_line_width 1 ;
    set_color shadow_color ;
    fill_rounded_rect ~radius:7 (x + 2) (y - 2) w h ;
    set_color background_color ;
    fill_rounded_rect ~radius:7 x y w h ;
    set_color text_color ;
    draw_rounded_rect ~radius:7 x y w h ;
    { x = x ; y = y ; w = w ; h = h }
      
  let draw_label geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    { x = x ; y = y ; w = w ; h = h }

  let draw_button geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    set_line_width 1 ;
    set_color shadow_color ;
    fill_rounded_rect ~radius:7 (x + 2) (y - 2) w h ;
    set_color background_color ;
    fill_rounded_rect ~radius:7 x y w h ;
    set_color text_color ;
    draw_rounded_rect ~radius:7 x y w h ;
    { x = x + padding ; y = y + padding ; w = w - 2 * padding ; h = h - 2 * padding }

  let draw_button_hilight geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    set_line_width 1 ;
    set_color shadow_color ;
    fill_rounded_rect ~radius:7 (x + 2) (y - 2) w h ;
    set_color hilight_color ;
    fill_rounded_rect ~radius:7 x y w h ;
    set_color background_color ;
    fill_rounded_rect ~radius:3 (x + padding) (y + padding) (w - 2 * padding) (h - 2 * padding) ;
    set_color text_color ;
    draw_rounded_rect ~radius:7 x y w h ;
    { x = x + padding ; y = y + padding ; w = w - 2 * padding ; h = h - 2 * padding }

  let draw_input geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    set_line_width 1 ;
    set_color shadow_color ;
    fill_rounded_rect ~radius:7 x y w h ;
    set_color background_color ;
    fill_rounded_rect ~radius:5 (x + 3) y (w - 3) (h - 3) ;
    set_color text_color ;
    draw_rounded_rect ~radius:7 x y w h ;
    { x = x + padding ; y = y + padding ; w = w - 2 * padding ; h = h - 2 * padding }

  let draw_input_hilight geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    set_line_width 1 ;
    set_color shadow_color ;
    fill_rounded_rect ~radius:7 x y w h ;
    set_color hilight_color ;
    fill_rounded_rect ~radius:5 (x + 3) y (w - 3) (h - 3) ;
    set_color text_color ;
    draw_rounded_rect ~radius:7 x y w h ;
    { x = x + padding ; y = y + padding ; w = w - 2 * padding ; h = h - 2 * padding }

end

module Chicago : SKIN = struct
  module Palette = struct
    let hilight_color = rgb 100 100 255
    let shadow_color = rgb 20 20 20
    let background_color = rgb 180 180 180
    let text_color = rgb 0 0 0
  end
    
  open Palette

  let padding = 5
  let button_size (w, h) = (w + 4 * padding, h + 4 * padding)
  let input_size (w, h) = (w + 4 * padding, h + 4 * padding)
  let frame_size (w, h) = (w + 2 * padding, h + 2 * padding)
  let label_size (w, h) = (w + 2 * padding, h + 2 * padding)

  let draw_raised x y width height =
    set_color background_color ;
    fill_rect x y width height ;
    set_color black ;
    draw_rect x y width height ;
    set_color (rgb 255 255 255) ;
    moveto (x + 2) (y + 2) ;
    rlineto 0 (height - 4) ;
    rlineto (width - 4) 0 ;
    set_color (rgb 80 80 80) ;
    moveto (x + 1) (y + 1) ;
    rlineto (width - 2) 0 ;
    rlineto 0 (height - 2) ;
    set_color (rgb 120 120 120) ;
    moveto (x + 2) (y + 2) ;
    rlineto (width - 4) 0 ;
    rlineto 0 (height - 4)

  let draw_lowered x y width height =
    set_color background_color ;
    fill_rect x y width height ;
    set_color black ;
    draw_rect x y width height ;
    set_color (rgb 255 255 255) ;
    moveto (x + 2) (y + 2) ;
    rlineto (width - 4) 0 ;
    rlineto 0 (height - 4) ;
    set_color (rgb 80 80 80) ;
    moveto (x + 1) (y + 1) ;
    rlineto 0 (height - 2) ;
    rlineto (width - 2) 0 ;
    set_color (rgb 120 120 120) ;
    moveto (x + 2) (y + 2) ;
    rlineto 0 (height - 4) ;
    rlineto (width - 4) 0
      
  let frame_inside geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    { x = x ; y = y ; w = w ; h = h }

  let draw_frame geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    set_line_width 1 ;
    draw_raised geom.x geom.y geom.w geom.h ;
    { x = x ; y = y ; w = w ; h = h }
      
  let draw_label geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    { x = x ; y = y ; w = w ; h = h }

  let draw_button geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    set_line_width 1 ;
    draw_raised x y w h ;
    set_color background_color ;
    draw_rect (x - 1) (y - 1) (w + 2) (h + 2) ;
    { x = x + padding ; y = y + padding ; w = w - 2 * padding ; h = h - 2 * padding }

  let draw_button_hilight geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    set_line_width 1 ;
    draw_raised x y w h ;
    set_color hilight_color ;
    draw_rect (x - 1) (y - 1) (w + 2) (h + 2) ;
    { x = x + padding ; y = y + padding ; w = w - 2 * padding ; h = h - 2 * padding }

  let draw_input geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    set_line_width 1 ;
    draw_lowered x y w h ;
    set_color background_color ;
    draw_rect (x - 1) (y - 1) (w + 2) (h + 2) ;
    { x = x + padding ; y = y + padding ; w = w - 2 * padding ; h = h - 2 * padding }

  let draw_input_hilight geom =
    let x = geom.x + padding in
    let y = geom.y + padding in
    let w = geom.w - 2 * padding in
    let h = geom.h - 2 * padding in
    set_line_width 1 ;
    draw_lowered x y w h ;
    set_color hilight_color ;
    draw_rect (x - 1) (y - 1) (w + 2) (h + 2) ;
    { x = x + padding ; y = y + padding ; w = w - 2 * padding ; h = h - 2 * padding }
end

module TK = Make (Chicago)
  
let _ =
  let open TK in
  let nom = ref "" and prenom = ref "" in
  open_graph " 600x600" ;
  let desktop w =
    vbox [ stretch w ; frame (hbox [button "START" (fun () -> None) ; glue () ; field (ref "22:22") 6] )]
  in
  run_ui
    (desktop
       (centered
	  (frame
	     (vbox [
	       hbox [ label "Nom" ; stretch (field nom 20) ] ;
	    hbox [ label "Prénom" ; stretch (field prenom 20) ] ;
	    hbox [ button "Bonjour !" (fun () -> None) ; glue () ; button "Au revoir !" (fun () -> None) ]
	     ]))))
