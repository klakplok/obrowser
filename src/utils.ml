open Printf

let rec pairwise_fold f l =
  let rec fold l =
    match l with
    | [] -> assert false
    | [e] -> e
    | e :: tl -> let e' = fold tl in f e e'
  in
  fold l

let uniqsort l =
  let rec uniq acc = function
    | a :: (b :: _ as l) when a = b -> uniq acc l
    | a :: (b :: _ as l) -> uniq (a :: acc) l
    | [a] -> a :: acc
    | [] -> acc
  in
  List.rev (uniq [] (List.sort compare l))

let roman_n n v =
  let s = String.make n '_' in
  let v = ref v in
  for i = 0 to n - 1 do
    s.[i] <- Char.chr (Char.code 'A' + (!v mod 26)) ;
    v := !v / 26
  done ;
  s

let roman_tab =
  [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
     "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z";
     "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M";
     "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z" |]

let roman n =
  if n = 0 then
    "a"
  else
    let r = ref "" in
    let n = ref n in
    while !n <> 0 do
      r := roman_tab.(!n mod Array.length roman_tab) ^ !r ;
      n := !n / Array.length roman_tab
    done ;
    !r ;;

let read_whole_file path =
  let fp = open_in_bin path in
  let len = in_channel_length fp in
  let data = String.make len '\000' in
  really_input fp data 0 len ;
  close_in fp ;
  data

let write_file_contents outchan path =
  let data = read_whole_file path in
  output outchan data 0 (String.length data)

let prindent fp indent =
  for i = 1 to indent do
    fprintf fp "  "
  done

let rec sprintf_list fmt sep l =
  match l with
  | [] -> ""
  | [n] ->
    sprintf fmt n
  | e :: etl ->
    sprintf fmt e ^ sprintf "%s" sep ^ sprintf_list fmt sep etl

let sprintf_array fmt sep ar =
  sprintf_list fmt sep (Array.to_list ar)

let mapi f l =
  let rec loop acc i = function
    | [] -> List.rev acc
    | x :: xtl -> loop (f i x :: acc) (succ i) xtl
  in
  loop [] 0 l

let iteri f l =
  let rec loop i = function
    | [] -> ()
    | x :: xtl -> f i x ; loop (succ i) xtl
  in
  loop 0 l

let plural n =
  if n <= 1 then "" else "s"
