open Obrowser_io ;;
open Obrowser_jsoo ;;

let items = Array.to_list (Obrowser_regexp.split (Obrowser_regexp.make ~multi_line:true "\n") (http_get "list.txt")) ;;

let container = get_element_by_id "basket" ;;

Node.append
  container
  (Html.ol
     (List.map
	(fun n -> Html.li [Html.string n])
	(List.filter (fun x -> x <> "") items)))
;;

let parse_html_part s =
  let tmp = eval "document" >>> call_method "createElement" [| string "div" |] in
  tmp >>> set "innerHTML" (string s) ; tmp >>> get "firstChild"
;;

Node.append
  (eval "document" >>> get "body")
  (parse_html_part "<h1>LOADED !</h1>")
;;

