open Messages
open Utils
open Printf
open Types

(* call ocamlc on user sources with ad hoc options *)
let compile_sources input_files =
  let ocamlc_base = sprintf "ocamlc -vmthread threads.cma -I %s obrowser.cma" Config.path in
  let temp_file = Filename.temp_file "obro" ".byte" in
  let command = sprintf "%s -o \"%s\"%s" ocamlc_base temp_file (List.fold_left (sprintf "%s \"%s\"") "" input_files) in
  debug "call"  "%s" command ;
  match Sys.command command with
  | 0 -> temp_file
  | v -> error "call" "unable to compile source (ocamlc returned %d)" v

(* use a binary bytecode file provided by the user or compile sources *)
let obtain_binary input_files =
  match input_files with
  | [] -> Messages.error "main" "no input file specified"
  | [file] when not (Filename.check_suffix file ".ml") -> file
  | _ ->
    if List.for_all (fun s -> Filename.check_suffix s ".ml") input_files then
      compile_sources input_files
    else
      Messages.error "main" "must specify one input bytecode binary or one or more .ml sources"

(* compile a binary bytecode file to JS *)
let compile_to_js input_binary output_file =
  (* parse binary *)
  let data, code, dlls = Reader.load input_binary in
  let code = Stack_to_vars.decompose_into_basic_blocks code in  
  (* switch to variable based bytecode *)
  let globals, segments, functions = Stack_to_vars.bc_to_vbc data code in
  (* first translation : switch to variables *)
  let rec fix limit f x = if limit = 0 then (warning "optimizations-driver" "fix point not reached" ; x) else let r = f x in if r = x then r else fix limit f r in
  (* repeat optimizations until fixpoint (or max reached *)
  let segments, functions, globals = fix !Config.max_opt_passes
    (fun (segments, functions, globals) ->
      Optimizations.update_max_var segments ;
      let segments = Optimizations.inline_externals segments in
      let segments = Optimizations.drop_simple_aliases segments functions in
      let segments = Optimizations.remove_useless_assignments segments functions globals in
      let segments = Optimizations.partial_evaluation segments in
      let segments, globals = Optimizations.drop_global_consts segments functions globals in
      let segments, globals = Optimizations.unbox_globals segments functions globals in
      let segments, globals = Optimizations.lift_global_const_inits segments functions globals in
      let globals = Optimizations.remove_useless_globals segments functions globals in
      let segments, functions, globals = Optimizations.bypass_silly_jumps segments functions globals in
      let segments = Optimizations.join_segments segments functions in
      let segments = Optimizations.peephole_accu_removal segments in
      let segments, functions = Optimizations.simple_dead_code_removal segments functions globals in
      segments, functions, globals)
    (segments, functions, globals)
  in
  let segments, functions, globals = Optimizations.narrow segments functions globals in
  (* compilation step ok, print *)
  let fp = open_out output_file in
  List.iter
    (fun dll ->
      Printf.fprintf fp "/* included library %s */\n" dll ;
      write_file_contents fp (sprintf "%s/obrowser_%s.js" Config.path dll))
    dlls ;
  Js_emit.print_program fp globals segments functions (Filename.basename output_file)  

 (* Js_emit.print_annotated_program fp globals segments osegments functions (Filename.basename output_file)
    (fun s -> IntMap.fold (fun v c r -> r ^ Printf.sprintf "%d = %d," v (AddrSet.cardinal c)) s "")
    (Optimizations. collect_occurences segments functions)*)
(* Js_emit.print_annotated_program fp globals segments code functions (Filename.basename output_file)
    (fun s -> LvalueMap.fold (fun v c r -> r ^ Printf.sprintf "%s = %s," (Js_emit.string_of_lvalue v) (Js_emit.string_of_rvalue c)) s "")
    (Optimizations.collect_simple_aliases segments functions) *)
(* Js_emit.print_annotated_program fp globals segments code functions (Filename.basename output_file)
    (fun s -> IntMap.fold (fun v c r -> r ^ Printf.sprintf "v%d = %s," v (Optimizations.Value_analysis.string_of_abstract_value c)) s "")
    (Optimizations.Value_analysis.collect segments functions) *)
    
(* main entry point of the compiler *)
let main () =
  let output_file = ref "a.out.js" in
  let input_files = ref [] in
  Arg.parse
    (Arg.align
       [ "-o", Arg.Set_string output_file, "<file> output file name" ;
	 "-opt", Arg.Set_int Config.max_opt_passes, "<n> maximum number of optimization passes (n = 50 by def.)" ;
	 "-where", Arg.Unit (fun () -> print_endline Config.path ; exit 0), " prints the location of OBrowser's library and exits" ;
	 "-debug", Arg.Set_int Config.debug_mode, "<level> show compiler debug messages level (0 to 3)" ])
    (fun m -> input_files := m :: !input_files)
    ("obrowserc [options] <bytecode file (not ending in .ml)>\n"
     ^ "obrowserc [options] <source.ml> <source2.ml> ...") ;
  input_files := List.rev !input_files ;
  let input_binary = obtain_binary !input_files in
  compile_to_js input_binary !output_file
    
let _ = main ()

