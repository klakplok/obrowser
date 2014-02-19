let width = 512 and height = 512 and unit = 2
let random_xy () = Random.(int (width / unit), int (height / unit))
let mix (ra,ga,ba) (rb,gb,bb) rat =
  Graphics.rgb
    (ra + int_of_float (float (rb - ra) *. rat))
    (ga + int_of_float (float (gb - ga) *. rat))
    (ba + int_of_float (float (bb - ba) *. rat))

let () =
  let nb_threads = 130 in
  let state = Array.make_matrix (height / unit) (width / unit) false in
  let lock = Mutex.create () in
  Random.self_init () ;
  Graphics.open_graph (Printf.sprintf " %dx%d" width height) ;
  Graphics.(set_color black ; fill_rect 0 0 width height) ;
  let rec run p =
    let d = Random.float 0.2 +. 0.01 in
    let max_len = Random.int 45 + 5 in
    try
      let r, g, b = Random.(int 256, int 256, int 256) in
      let c = Graphics.rgb r g b in
      let pos = ref [p] in
      let count_down = ref 10 in
      while true do
	let x, y = List.hd !pos in
	let nx, ny =
	  match Random.int 6 with
	  | 0 -> (x + 1) mod (width / unit), y
	  | 1 | 2 -> (x + (width / unit) - 1) mod (width / unit), y
	  | 3 | 4 -> x, (y + 1) mod (height / unit)
	  | 5 -> x, (y + (height / unit) - 1) mod (height / unit)
	  | _ -> assert false
	in
	Mutex.lock lock ;
	if state.(ny).(nx) then (
	  decr count_down ;
	  if !count_down = 0 then (
	    Graphics.(set_color (mix (r,g,b) (0,0,0) 0.8) ; fill_rect (x * unit) (y * unit) unit unit) ;
	    state.(y).(x) <- false ;
	    pos := List.rev (List.tl !pos) ;
	    if !pos = [] then
	      raise Exit
	  ) ;
	  Mutex.unlock lock ;
	  Thread.delay 0.01 ;
	) else (
	  count_down := 10 ;
	  state.(ny).(nx) <- true ;
	  Mutex.unlock lock ;
	  Graphics.(set_color c ; fill_rect (x * unit) (y * unit) unit unit) ;
	  pos := (nx, ny) :: !pos ;
	  if List.length !pos > max_len then
	    let rec extract acc = function
	      | x :: [e] -> List.rev (x :: acc), e
	      | x :: tl -> extract (x :: acc) tl
	      | [] -> assert false
	    in
	    let rem, (lx, ly) = extract [] !pos in
	    pos := rem ;
	    Graphics.(set_color (mix (r,g,b) (0,0,0) 0.8) ; fill_rect (lx * unit) (ly * unit) unit unit) ;
	    Mutex.lock lock ;
	    state.(ly).(lx) <- false ;
	    Mutex.unlock lock ;
	    Thread.delay d
	)
      done
    with Exit -> run (random_xy ())
  in
  for i = 1 to nb_threads do ignore (Thread.create run (random_xy ())) done ;
  let rec main_loop () =
    let open Graphics in
    let { mouse_x ; mouse_y ; button } = wait_next_event [ Mouse_motion ] in
    if button && mouse_x >= 0 && mouse_y >= 0 && mouse_x < width && mouse_y < height then (
      let x, y = mouse_x / unit, mouse_y / unit in
      Mutex.lock lock ;
      if not state.(y).(x) then (
	state.(y).(x) <- true ;
	Mutex.unlock lock ;
	set_color white ;
	fill_rect (x * unit) (y * unit) unit unit
      ) else Mutex.unlock lock
    ) ;
    Thread.yield () ;
    main_loop ()
  in
  main_loop ()
