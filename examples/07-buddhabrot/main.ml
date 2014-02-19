open Graphics

let ccs =
  [| 1.0, 0.0, 0.0 ;
     1.0, 1.0, 0.0 ;
     0.0, 1.0, 0.0 ;
     0.0, 1.0, 1.0 ;
     0.0, 0.0, 1.0 ;
     1.0, 0.0, 1.0 |]

let ccs =
  [| 0.0, 0.0, 0.0 ;
     0.0, 1.0, 0.0 ;
     1.0, 1.0, 0.0 ;
     1.0, 1.0, 1.0 |]

let ccn = Array.length ccs - 1
  
let cc v =
  let v = max 0. (min 0.999 v) *. float ccn in
  let m = int_of_float v in
  let fr = v -. float m in
  let r0, g0, b0 = ccs.(m)
  and r1, g1, b1 = ccs.(m + 1) in
  let r = int_of_float (255. *. (r1 *. fr +. r0 *. (1. -. fr)))
  and g = int_of_float (255. *. (g1 *. fr +. g0 *. (1. -. fr)))
  and b = int_of_float (255. *. (b1 *. fr +. b0 *. (1. -. fr))) in
  rgb r g b
;;

let buddha w nmax (x0, y0) (x1, y1) =
  open_graph (Printf.sprintf " %dx%d" w (w + 10)) ;
  auto_synchronize false ;
  for i = 0 to w - 1 do
    set_color (cc (float i /. float w)) ;
    moveto i w ;
    rlineto 0 10
  done ;
  let mat = Array.make_matrix w w 0 in
  let rec loop () =
    let x0 = min x0 x1 and x1 = max x0 x1 in
    let y0 = min y0 y1 and y1 = max y0 y1 in
    let dx = (x1 -. x0) /. float w in
    let dy = (y1 -. y0) /. float w in
    let imax = ref 0 in
    for i = 0 to 1000 do
      let x = x0 +. Random.float (x1 -. x0) in
      let y = y0 +. Random.float (y1 -. y0) in
      let rec seq n zx zy =
	let pzx = int_of_float ((zx -. x0) /. dx)  and pzy = int_of_float ((zy -. y0) /. dy) in
	let zx' = zx *. zx -. zy *. zy +. x in
	let zy' = 2. *. zx *. zy +. y in
	if n >= nmax then
	  false
	else (
	  if  zx *. zx +. zy *. zy > 4. || seq (n + 1) zx' zy' then (
	    if pzx >= 0 && pzx < w && pzy > 0 && pzy < w then (
	      mat.(pzx).(pzy) <- mat.(pzx).(pzy) + 1 ;
	      imax := max mat.(pzx).(pzy) !imax
	    ) ;
	    true
	  ) else false
	)
      in
      ignore (seq 0 x y)
    done ;
    let rec normamax n i =
      if n lsr i = 1 then
	1 lsl i
      else
	normamax n (i + 1)
    in
    imax := normamax !imax 0 ;
    for px = 0 to w - 1 do
      for py = 0 to w - 1 do
	set_color (cc (float mat.(px).(py) /. float !imax)) ;
	plot py (w - px)
      done
    done ;
    synchronize () ;
    loop ()
  in
  loop ()
;;

buddha 300 1000 (-1.5,-1.) (0.5,1.) ;;
