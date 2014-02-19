open Printf

let error section fmt =
  let write str =
    Printf.fprintf stderr "\027[31mError (%s): \027[0m%s\n%!" section str ;
    exit 1
  in
  Format.kprintf write fmt
    
let warning section fmt =
  let write str =
    Printf.fprintf stderr "\027[33mWarning (%s): \027[0m%s\n%!" section str
  in
  Format.kprintf write fmt
    
let debug ?(level = 0) section fmt =
  let write str =
    if !Config.debug_mode >= level then
      Printf.fprintf stderr "\027[32mDebug (%s): \027[0m%s\n%!" section str
  in
  Format.kprintf write fmt

let time section f =
  let t0 = Sys.time () in
  let r = f () in
  let t1 = Sys.time () in
  if t1 -. t0 > !Config.show_time_limit then
    Printf.fprintf stderr "\027[34mElapsed time (%s): \027[0m%gs\n%!" section (t1 -. t0) ;
  r

let fdebug ?(level = 0) fp fmt =
  let write str =
    if !Config.debug_mode >= level then
      Printf.fprintf fp "%s%!" str
  in
  Format.kprintf write fmt

let ensure b section fmt =
  let write str =
  if not b then
    error section "%s" str
  in
  Printf.kprintf write fmt

let ensure_all f l section fmt =
  let write str =
  if not (List.for_all f l) then
    error section "%s" str
  in
  Printf.kprintf write fmt
    
