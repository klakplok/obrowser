open Obrowser_jsoo

let lol s =
  eval "console" >>> call_method "debug" [| string s |] >>> ignore
    
let document = eval "document"
let svg = document >>> call_method "getElementsByTagName" [| string "svg" |] >>> get "0"
let init_t = Unix.gettimeofday ()

let wobble id phase period amplitude =
  let elt = document >>> call_method "getElementById" [| string id |] in
  let tr = svg >>> call_method "createSVGTransform" [| |] in
  elt >>> get "transform" >>> get "baseVal" >>> call_method "initialize" [| tr |] >>> ignore ;
  let rec loop () =
    let scale = 0.9 +. amplitude *. sin ((Unix.gettimeofday () -. init_t +. phase) *. 6.28 /. period) in
    let mat =
      svg >>> call_method "createSVGMatrix" [| |]
      >>> call_method "translate" [| int (128) ; int (190) |]
      >>> call_method "scale" [| float scale |] 
      >>> call_method "translate" [| int (-128) ; int (-190) |]
    in
    tr >>> call_method "setMatrix" [| mat |] >>> ignore ;
    Thread.delay 0.05 ;
    loop ()
  in
  loop ()

let run f x = ignore (Thread.create (fun () -> try f x with e -> lol (Printexc.to_string e)) ())
let () =
  run (fun () -> wobble "left_up_leg" 0. 1. 0.1 ) () ;
  run (fun () -> wobble "left_down_leg" 0.5 1. 0.1 ) () ;
  run (fun () -> wobble "right_down_leg" 1. 1. 0.1 ) () ;
  run (fun () -> wobble "right_up_leg" 1.5 1. 0.1 ) ()

