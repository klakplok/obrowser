let main () =
  (* parse args *)
  let input_binary, output_file =
    match Sys.argv with
    | [| input |] -> input, stdin
    | [| input ; output|] -> input, open_out output
    | _ -> Printf.printf "Usage:\n  %s <path to bytecode file>\n%s <path to output assembly file>\n" Sys.exe_name Sys.exe_name
  in
  (* parse binary *)
  let data, code, _ = Reader.load input_binary in
  (* retrieve basic segments *)
  let segments = Stack_to_vars.decompose_into_basic_blocks code in
  (* print *)
  Bc_emit.print_program fp data segments ;
  close_out fp

let _ = main ()

