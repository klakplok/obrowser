open Printf
open Obrowser_io

let body =
  get_element_by_id "body"

(* generic display panel *)
let make_box default =
  let div = Html.div
    ~style:"border: 1px black solid; background-color: white ;
            display: inline ; padding-right: .5em; padding-left: .5em;"
    [ Html.string default ] in
  let set v = Node.replace_all div (Html.string v) in
  (div, set)

(* score display panel *)
let make_rem () =
  let div, set = make_box "--" in
  (div, (fun i -> set (sprintf "%02d" i)))

(* clock display panel and start/stop *)
let make_clock () =
  let div, set = make_box "--:--:--" in
  let t0 = ref 0. and running = ref false in
  let run () =
    while true do
      if !running then (
	let secs = int_of_float (Sys.time () -. !t0) in
	set (sprintf "%02d:%02d:%02d"
	       (secs / 3600) ((secs / 60) mod 60) (secs mod 60))
      ) ;
      Thread.delay 1.
    done
  and start () = t0 := Sys.time () ; running := true
  and stop () = running := false in
  ignore (Thread.create run ()) ;
  (div, start, stop)

(* display "LOADING" while performing a task *)
let with_loading task arg =
  let div = Html.div
    ~style:"background-color: red;
            color: white; display:inline;
            position: absolute; top:2px; right:2px; padding: 2px 4px 2px 4px; border-radius: 2px;"
    [ Html.string "LOADING" ] in
  Node.append body div ;
  Thread.yield () ;
  let res = task arg in
  Node.remove body div ;
  res

(* process the content of a file *)
let process_file name process =
  with_loading
    process
    (http_get name)

(* fade in a page element *)
let fade elt t =
  let sty = Node.get_attribute elt "style" in
  let t0 = Sys.time () in	
  while Sys.time () -. t0 <= t do
    Node.set_attribute elt "style"
      (sprintf "%s opacity:%g;" sty ((Sys.time () -. t0) /. t)) ;
    Thread.delay 0.05 (* 20FPS max *)
  done ;
  Node.set_attribute elt "style" (sty ^ " opacity:1;")

(* type of cells *)
type cell =
  | Empty   | Grass   | Diamond
  | Boulder | Bam     | Door
  | End     | Guy     | Wall
(* game state *)
and state = {
  (* data *)
  map : cell array array ;      (* level map *)
  imgs : Node.t array array ;   (* references to img elements *)
  mutable pos : int * int ;     (* current pos *)
  mutable endpos : int * int ;  (* door *)
  mutable rem : int ;           (* remaining diamonds *)
  mutable dead : bool ;         (* are you dead ? *)
  (* call backs mutex *)
  mutable cb_mutex : Mutex.t ;
  (* workaround onMouseOut event dropping *)
  mutable pending_out_cb : (unit -> unit) option ref ;
}

let img_assoc = 
  [ (Empty, "sprites/empty.png");     (Bam, "sprites/bam.png");
    (Grass, "sprites/grass.png");     (Diamond, "sprites/diamond.png");
    (Boulder, "sprites/boulder.png"); (End, "sprites/end.png");
    (Door, "sprites/door.png");       (Guy, "sprites/guy.png");
    (Wall, "sprites/wall.png")]

(* updates a cell and its associated display *)
let set_cell state x y v = 
  state.map.(y).(x) <- v ;
  Node.set_attribute state.imgs.(y).(x) "src" (List.assoc v img_assoc)

(* 5th Symphony *)
exception Death

(* make boulders fall, and kill the guy if it must be so *)
let rec fall state =
  (* assumes wall borders *)
  let changed = ref false in
  for y = Array.length state.map - 2 downto 1 do
    for x = 1 to Array.length state.map.(y) - 2 do
      let sustaining = state.map.(y + 1).(x) = Guy && state.map.(y).(x) = Boulder in
      if (state.map.(y).(x) = Empty
	 && state.map.(y - 1).(x) = Boulder) then (
	set_cell state x (y - 1) Empty ;
	set_cell state x y Boulder ;
	changed := true
      ) ;
      if (state.map.(y).(x) = Empty
	 && state.map.(y - 1).(x) = Empty
	   && state.map.(y).(x - 1) = Boulder
	     && state.map.(y - 1).(x - 1) = Boulder) then (
	set_cell state (x - 1) (y - 1) Empty ;
	set_cell state x y Boulder ;
	changed := true
      ) ;
      if (state.map.(y).(x) = Empty
	 && state.map.(y - 1).(x) = Empty
	   && state.map.(y).(x + 1) = Boulder
	     && state.map.(y - 1).(x + 1) = Boulder) then (
	set_cell state (x + 1) (y - 1) Empty ;
	set_cell state x y Boulder ;
	changed := true
      ) ;
      if (not sustaining) && state.map.(y + 1).(x) = Guy && state.map.(y).(x) = Boulder then (
	set_cell state x (y + 1) Bam ;
	raise Death
      )
    done
  done ;
  if !changed then (
    Thread.delay 0.05 ;
    fall state
  )

(* main game loop *)
let rec loop state rem_set clock_stop =
  if state.pos = state.endpos then (
    clock_stop () ;
    clear_cbs state ;
    alert "YOU WIN !"
  ) else
    if state.dead then (
      clock_stop () ;
      clear_cbs state ;
      alert "YOU LOSE !"
    ) else (
      if state.rem = 0 then (
	(* open door *)
	let x,y = state.endpos in
	Node.set_attribute state.imgs.(y).(x) "src" "sprites/end.png" ;
	state.map.(y).(x) <- End  	
      ) ;
      clear_cbs state ;
      install_cbs state rem_set clock_stop
    )
(* remove all event cb *)
and clear_cbs state =
  for y = 0 to Array.length state.map - 1 do
    for x = 0 to Array.length state.map.(y) - 1 do
      Node.clear_event state.imgs.(y).(x) "onmouseover" () ;
      Node.clear_event state.imgs.(y).(x) "onmouseout" () ;
      Node.clear_event state.imgs.(y).(x) "onclick" ()
    done
  done
(* install cbs for a loop step *)
and install_cbs state rem_set clock_stop =
  (* is a cell walkable ? *)
  let walkable = function
    | Empty | Grass | Diamond | End -> true
    | _-> false
  in
  (* event cb combinators *)
  let inhibit f () =
  (* do not execute if another cb is executing *)
    if Mutex.try_lock state.cb_mutex then
      (f () ; Mutex.unlock state.cb_mutex)
  and set_pending_out f out () =
  (* set a corresponding out cb when called *)
    f () ; state.pending_out_cb := Some out
  and with_pending_out ?out f () =
  (* call previous out cb if present *)
    match !(state.pending_out_cb), out with
      | None, _ -> f ()
      | Some out, None -> out () ; state.pending_out_cb := None ; f ()
      | Some out, Some out' -> if out == out' then (out () ; state.pending_out_cb := None) ; f ()
  in
  (* install move event cbs *)
  let rec install_move (x, y) (dx, dy) img over_cont out_cont click_cont =
    if walkable state.map.(y).(x) then (
      let cur_img = Node.get_attribute state.imgs.(y).(x) "src" in
      let over () = Node.set_attribute state.imgs.(y).(x) "src" img ; over_cont ()
      and out () = Node.set_attribute state.imgs.(y).(x) "src" cur_img ; out_cont ()
      and click () =
	try
	  click_cont () ;
	  if state.map.(y).(x) = Diamond then state.rem <- state.rem - 1 ;
	  Thread.delay 0.05 ;
	  set_cell state (x - dx) (y - dy) Empty ;
	  set_cell state x y Guy ;
	  state.pos <- (x,y) ;
	  fall state ;
	with Death -> state.dead <- true ;
      in
      rem_set state.rem ;
      Node.register_event state.imgs.(y).(x) "onmouseover"
	(inhibit (set_pending_out (with_pending_out over) out)) () ;
      Node.register_event state.imgs.(y).(x) "onmouseout"
	(inhibit (with_pending_out ~out (fun () -> ()))) () ;
      Node.register_event state.imgs.(y).(x) "onclick"
	(inhibit (with_pending_out (fun () -> click () ; loop state rem_set clock_stop))) () ;
      if state.map.(y).(x) <> End then
	install_move (x + dx, y + dy) (dx, dy) img over out click
    ) 
  (* install push event cbs *)
  and update_push (x, y) (dx, dy) img img_guy=
    let x' = x + dx and y' = y + dy in
    let x'' = x' + dx and y'' = y' + dy in
    if (try
	  state.map.(y').(x') = Boulder && state.map.(y'').(x'') = Empty
      with Invalid_argument "index out of bounds" -> false) then (
      let over () =
	Node.set_attribute state.imgs.(y).(x) "src" img_guy ;
	Node.set_attribute state.imgs.(y').(x') "src" img
      in
      let out () =
	Node.set_attribute state.imgs.(y).(x) "src" "sprites/guy.png" ;
	Node.set_attribute state.imgs.(y').(x') "src" "sprites/boulder.png"
      in
      let click () =
	set_cell state x y Empty ;
	set_cell state x' y' Guy ;
	state.pos <- (x', y') ;
	set_cell state x'' y'' Boulder ;
	(try fall state with Failure "DEAD" -> state.dead <- true) ;
	loop state rem_set clock_stop
      in
      Node.register_event state.imgs.(y').(x') "onmouseover"
	(inhibit (set_pending_out (with_pending_out over) out)) () ;
      Node.register_event state.imgs.(y').(x') "onmouseout"
	(inhibit (with_pending_out (fun () -> ()))) () ;
      Node.register_event state.imgs.(y').(x') "onclick"
	(inhibit (with_pending_out click)) () ;
    )
  in
  let nil_cont () = () in
  let cx, cy = state.pos in
  install_move (cx + 1, cy) (1,  0) "sprites/R.png" nil_cont nil_cont nil_cont ;
  install_move (cx - 1, cy) (-1, 0) "sprites/L.png" nil_cont nil_cont nil_cont ;
  install_move (cx, cy - 1) (0, -1) "sprites/U.png" nil_cont nil_cont nil_cont ;
  install_move (cx, cy + 1) (0,  1) "sprites/D.png" nil_cont nil_cont nil_cont ;
  update_push (cx, cy) (1,  0) "sprites/bR.png" "sprites/push_r.png" ;
  update_push (cx, cy) (-1, 0) "sprites/bL.png" "sprites/push_l.png"

(* load list of levels *)
let load_levels () =
  process_file
    "maps.txt" 
    (fun txt ->
      let lines = Obrowser_regexp.split (Obrowser_regexp.make "\\n") txt in
      let pair_exp = "\"([^\"]+)\".*\"([^\"]+)\"" in
      let scan_pair line =
	let res = Obrowser_regexp.exec (Obrowser_regexp.make pair_exp) line in
	(res.(1), res.(2))
      in
      List.map scan_pair
	(List.filter
	   ((<>) "")
	   (Array.to_list lines)))

(* load level from file *)
let load_level_map file =
  process_file file (fun data ->
    let res = ref [] and row = ref [] in
    for i = 0 to String.length data - 1 do
      match data.[i] with
	| '\n' -> res := List.rev (!row) :: !res ; row := []
	| '#' -> row := Wall :: !row
	| '.' -> row := Grass :: !row
	| ' ' -> row := Empty :: !row
	| '+' -> row := Diamond :: !row
	| 'X' -> row := Boulder :: !row
	| 'W' -> row := Guy :: !row
	| 'E' -> row := Door :: !row
	| 'S' -> row := Guy :: !row
	| _ -> failwith "malformed level"
    done ;
    Array.of_list (List.map Array.of_list (List.rev !res)))

(* update game state with level *)
let load_level file board_div clock_start clock_stop rem_set =
  process_file file
    (fun data ->
      let map = load_level_map file in
      let gx = ref 0 and gy = ref 0 and ex = ref 0 and ey = ref 0 and rem = ref 0 in
      let imgs =
	Array.mapi (fun y ->
	  Array.mapi (fun x cell ->
	    (match cell with
	      | Guy -> gx := x ; gy := y
	      | Diamond -> incr rem
	      | Door -> ex := x ; ey := y
	      | _ -> ()) ;
	    Html.img ~src:(List.assoc cell img_assoc) ())) map
      in
      let table =
	Html.map_table
	  ~style:"border-collapse:collapse; line-height: 0;"
	  ~attrs:["align", "center"]
	  ~td_style:"padding: 0; width: 20px; height: 20px;"
	  imgs
      in
      loop
	{ map = map;
	  imgs = imgs ;
	  pos = (!gx, !gy) ;
	  endpos = (!ex, !ey) ;
	  cb_mutex = Mutex.create () ;
	  dead = false ;
	  rem = !rem ;
	  pending_out_cb = ref None }
	rem_set clock_stop ;
      Node.replace_all board_div table ;
      fade table 2. ;
      clock_start ()
    )

(* initialize page *)
let init () =
  let board_div = Html.div [] in
  let clock_div,clock_start,clock_stop = make_clock () in
  let rem_div, rem_set = make_rem () in
  Node.set_attribute body "style"
    "font-family: sans-serif;
     text-align: center;
     background-color: #e8e8e8;" ;
  Node.append body (Html.h1 [ Html.string "Boulder Dash in OCaml " ]) ;
  let levels = load_levels () in
  Node.append body
    (Html.div
       ([Html.string "Elapsed time: " ; clock_div ;
         Html.string " Remaining diamonds: " ; rem_div ;
	 Html.string " " ;
	 Html.br () ;
	 Html.br () ;
	 Html.string " - " ]
	@ (List.flatten
	     (List.map
		(fun (f, n) ->
		  [ Html.a ~onclick:(fun () ->
		    load_level f
		      board_div clock_start clock_stop rem_set)
		      [ Html.string n ] ;
		    Html.string " - "])
	     levels))
	@ [Html.br () ; Html.br () ;
	   board_div ]))

let _ = init ()
