open Obrowser_jsoo
open Graphics

class virtual shape =
object (self)
  method virtual is_pixel_in : int * int -> color option
end

let main (objs : #shape list) =
  open_graph " 256x256" ;
  for y = -128 to 127 do
    for x = -128 to 127 do
      let p = (x,y) in
      let rec draw_first l =
	match l with
	  | o :: os ->
	      begin match o#is_pixel_in p with
		| Some c ->
		    set_color c ;
		    plot (x + 128) (y + 128)
		| None ->
		    draw_first os
	      end
	  | [] -> ()
      in draw_first objs
    done
  done ;
  close_graph ()

class rectangle (x0,y0) (x1,y1) =
object
  inherit shape
  method is_pixel_in (x,y) =
    if (x0 <= x && x <= x1) && (y0 <= y && y <= y1) then
      Some black
    else
      None
end

class circle (xc,yc) r =
object
  inherit shape
  method is_pixel_in (x,y) =
    if ((xc - x) * (xc - x) + (yc - y) * (yc - y)) <= r * r then
      Some black
    else
      None
end

class checker b =
object
  inherit shape
  method is_pixel_in (x,y) =
    let mmod v b = if v >= 0 then (v mod b) else b - ((- v) mod b) in
      if (mmod x (2*b) / b) + (mmod y (2*b) / b) <> 1 then
	Some black
      else
	None
end

class colored color s =
object
  inherit shape
  method is_pixel_in p =
    match s#is_pixel_in p with
      | Some _ -> Some color
      | None -> None
end

class contour color s = object (self)
  inherit shape
  method is_pixel_in p =
    match s#is_pixel_in p, self#is_in_contour p with
      | _, true -> Some color
      | Some c, false -> Some c
      | None, false -> None
  method private is_in_contour p =
    let left (x,y) = (x-1, y)
    and right (x,y) = (x+1, y)
    and up (x,y) = (x, y-1)
    and down (x,y) = (x, y+1)
    in
      not (s#is_pixel_in p <> None)
      && (s#is_pixel_in (left p) <> None
	  || s#is_pixel_in (left p) <> None
	  || s#is_pixel_in (right p) <> None
	  || s#is_pixel_in (up p) <> None
	  || s#is_pixel_in (down p) <> None)
end

class compose op s1 s2 =
object
  inherit shape
  method is_pixel_in p =
    if op (s1#is_pixel_in p <> None) (s2#is_pixel_in p <> None) then
      Some black
    else
      None
end

let rem a b = match a,b with
  | false, false -> false
  | false, true -> false
  | true,  false -> true
  | true,  true -> false

class polar_transform tr sh = object (self)
  inherit shape
  method is_pixel_in (x, y) =
    let polar_of_rect (x, y) =
      let p = sqrt (x ** 2. +. y ** 2.) in
      let a = atan2 x y in
	(a, p)
    in
    let rect_of_polar (a, p) =
      (p *. sin a, p *. cos a)
    in
    let c = polar_of_rect (float_of_int x, float_of_int y) in
    let c' = tr c in
    let x', y' = rect_of_polar c' in
      sh#is_pixel_in (int_of_float x', int_of_float y')
end

let _ =
  let t0 = Sys.time () in
  main [
    new colored white
      (new polar_transform
	 (fun (a, p) -> (a +. p /. 100., p))
	 (new compose (||)
	    (new compose rem
	       (new circle (0,0) 90)
	       (new compose (||)
		  (new compose (||)
		     (new circle (-25,40) 30)
		     (new circle (25,30) 40))
		  (new circle (-40,-40) 20)))
	    (new compose (||)
	       (new circle (-35,30) 10)
	       (new circle (15,20) 10)))) ;
    new colored red
      (new polar_transform
	 (fun (a, p) -> (a -. p /. 200., p))
	 (new checker 40)) ;
    new rectangle (-128, -128) (255, 255)
  ] ;
  let text = Printf.sprintf "Time : %g" (Sys.time () -. t0) in
  let text =  eval "document" >>> call_method "createTextNode" [| string text |] in
  eval "document" >>> get "body" >>> call_method "appendChild" [| text |] >>> ignore ;
