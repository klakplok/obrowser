(*****************************************************************************)
(*****  Devellopement d'applications avec Objective Caml                 *****)
(*****                                                                   *****)
(*****  Application :  Jeux a deux joueurs : Stone Henge                 *****)
(*****************************************************************************)

module Stone_rep = struct
  type joueur = bool 
  type pierre =  P of int
  let int_of_pierre = function P x -> x 

  type menhir = Rien | M of joueur

  type case = Vide | Occup of joueur*pierre
  let value_on_case = function
      Vide -> 0
    | Occup (j, x) -> int_of_pierre x

  type jeu = J of case array * menhir array * pierre list * pierre list
  type coup =  int * pierre 

  let lignes = [
    [0;1]; [2;3;4]; [5; 6; 7; 8;]; [9; 10; 11; 12; 13]; [14; 15; 16; 17];
    [0; 2; 5; 9]; [1; 3; 6; 10; 14]; [4; 7; 11; 15]; [8; 12; 16]; [13; 17];
    [9; 14]; [5; 10; 15]; [2; 6; 11; 16]; [0; 3; 7; 12; 17]; [1; 4; 8; 13]  ]

  let vecteur_l = Array.of_list lignes

  let lignes_par_case v =
    let t =  Array.length v  in
    let r = Array.create 18 [||] in
    for i = 0 to 17  do
      let w = Array.create 3 0
      and p = ref 0 in
      for j=0 to t-1 do if List.mem i v.(j) then (w.(!p) <- j; incr p)
      done;
      r.(i) <- w
    done;
    r

  let num_ligne_par_case = lignes_par_case vecteur_l
  let rec lignes_de_i i l = List.filter (fun t -> List.mem i t) l 
 
  let lignes_des_cases l =
    let a = Array.create 18 l in
    for i=0 to 17 do
      a.(i) <-  (lignes_de_i i l)
    done; a

  let ldc = lignes_des_cases lignes
      
  let jeu_depart ()= let lp = [6; 5; 4; 3; 3; 2; 2; 1; 1] in 
  J ( Array.create 18 Vide, Array.create 15 Rien,
      List.map (fun x -> P x) lp, List.map (fun x -> P x) lp )

  let rec unicite l = match l with
    [] -> []
  | h::t -> if List.mem h t then unicite t else h:: (unicite t)
						     
  let coups_legaux joueur (J (ca, m, r1, r2)) =
    let r = if joueur then r1 else r2 in
    if r = [] then []
      else
        let l = ref [] in
          for i = 0 to 17 do
            if value_on_case ca.(i) = 0 then l:= i:: !l
          done;
          let l2 = List.map 
	      (fun x-> List.map (fun y-> x,y) (List.rev(unicite r)) ) !l in
          List.flatten l2
  let copie_plateau p = Array.copy p

  let  copie_carnac m = Array.copy m
  let rec joue_pierre caillou l = match l with
    [] -> []
  | x::q -> if x=caillou  then q
  else x::(joue_pierre caillou q)

  let compte_case joueur case = match case with
    Vide -> 0
  | Occup (j,p) -> if j = joueur then (int_of_pierre p) else  0

  let compte_ligne joueur ligne pos =
    List.fold_left (fun x y -> x +  compte_case joueur pos.(y)) 0 ligne
      
  let rec compte_max n = function
      [] -> 0
    | t::q ->
        if (n>0) then (int_of_pierre t) + compte_max (n-1) q
        else 0
	    
  let rec nbr_cases_libres ca  l  = match l with
    [] -> 0
  | t::q -> let c = ca.(t) in
    match c with
      Vide -> 1 + nbr_cases_libres ca q
    |_ -> nbr_cases_libres ca q

  let a_menhir i ma =
    match ma.(i) with
      Rien -> false
    | _ -> true

  let quel_menhir i ma =
    match ma.(i) with
      Rien -> failwith "quel_menhir"
    | M j  -> j

  let est_pleine l ca =  nbr_cases_libres ca l = 0

  (* fonction jouer : arbitre du jeu *)
  let jouer joueur coup jeu =
    let (c, i) = coup in
    let J (p, m, r1, r2) = jeu in
    let nr1,nr2 = if joueur then joue_pierre i r1,r2 
    else r1,  joue_pierre i r2  in
    let np = copie_plateau p in
    let nm = copie_carnac m in
    np.(c)<-Occup(joueur,i);  (* on joue le coup *)
    let lignes_de_la_case = num_ligne_par_case.(c) in
    
    (* calcul des menhirs des 3 lignes de la case *)
    for k=0 to 2 do
      let l =  lignes_de_la_case.(k) in
      if not (a_menhir l nm) then (
        if est_pleine vecteur_l.(l) np then (
          let c1 = compte_ligne joueur vecteur_l.(l) np
          and c2 = compte_ligne (not joueur) vecteur_l.(l) np in
          if (c1 > c2) then nm.(l) <- M joueur
          else  ( if c2 > c1 then nm.(l) <- M (not joueur)
          else nm.(l) <- M (not joueur ))))
    done;

    (* calcul des autres menhirs *)
    for k=0 to 14 do
      if not (a_menhir k nm ) then
        if est_pleine vecteur_l.(k) np then failwith "joueur"
        else   
          let c1 = compte_ligne joueur  vecteur_l.(k) np
          and c2 = compte_ligne (not joueur) vecteur_l.(k) np in
          let cases_libres = nbr_cases_libres np vecteur_l.(k) in
          let max1  = compte_max cases_libres (if joueur then nr1 else nr2)
          and max2 = compte_max cases_libres (if joueur then nr2 else nr1) in
          if c1 >= c2 + max2 then nm.(k) <- M joueur
          else if c2 >= c1 + max1 then nm.(k) <- M (not joueur)
    done;
    J(np,nm,nr1,nr2)
end;;


module Stone_eval = struct
  open Stone_rep
  type jeu = Stone_rep.jeu
 
  exception Fini of bool
  let plusI = 1000 and  moinsI = -1000
      
  let nbr_lignes_gagnees  (J(ca, m,r1,r2))  =
    let c1,c2 = ref 0,ref 0 in
    for i=0 to 14 do
      if a_menhir i m then if quel_menhir i m then incr c1 else incr c2
    done;
    !c1,!c2

  let rec nbr_points_restant lig = match lig with 
    [] -> 0
  | t::q -> (int_of_pierre t) + nbr_points_restant q

  let evaluer joueur jeu  =
    let (J (ca,ma,r1,r2)) = jeu in
    let c1,c2  = nbr_lignes_gagnees  jeu in
    let pr1,pr2 = nbr_points_restant r1, nbr_points_restant r2 in
    match joueur with
      true -> if c1 > 7 then plusI else 50 * (c1 - c2) + 10 * (pr1 - pr2)
    | false -> if c2 > 7 then moinsI else 50 * (c1 - c2) + 10 * (pr1 - pr2)

  let est_feuille joueur jeu =
    let v = evaluer joueur jeu  in
    v = plusI or v = moinsI or coups_legaux joueur jeu  = []

  let est_stable joueur jeu = true

  type etat = G | P | N | C
  let etat_de joueur m =
    let v = evaluer joueur m in
    if v = plusI then if joueur then G else P
    else 
      if v = moinsI 
      then if joueur then P else G
      else if coups_legaux joueur  m = [] then  N 
      else C
end;;

(***************************************************************************)

module Stone_graph = struct
  open Stone_rep
  type pierre = Stone_rep.pierre
  type menhir = Stone_rep.menhir
  type case = Stone_rep.case
  type jeu = Stone_rep.jeu
  type coup = Stone_rep.coup

  (* le rayon pour un pion ou une case  *)
  let rayon = 20

  (* les couleurs  *)
  let cBlack  = Graphics.black
  let cRed    = Graphics.rgb 165 43 24
  let cYellow = Graphics.yellow
  let cGreen  = Graphics.rgb 31 155 33 (*Graphics.green*)
  let cWhite  = Graphics.white
  let cGray   = Graphics.rgb 128 128 128
  let cBlue   = Graphics.rgb 196 139 25 (*Graphics.blue*)

  (* la largeur et la hauteur  *)
  let largeur = 600
  let hauteur = 500

  (* le decalage du haut de l'ecran a partir duquel on commence a dessiner  *)
  let top_offset = 30

  (* la taille des rebords  *)
  let bounds = 5

  (* le decalage du bord gauche du tableau fictif *)
  let virtual_table_xoffset = 145

  (* le decalage gauche pour les pions de choix noirs *)
  let choix_noir_offset = 40

  (* le decalage gauche pour les pions de choix rouges *)
  let choix_rouge_offset = 560

  (* la taille d'une case pour le tableau fictif  *)
  let virtual_case_size = 60

  (* corresp : int*int -> int*int  *)
  (* etablit une correspondance entre une case de la matrice et *)
  (* une position dans un tableau fictif servant a dessiner  *)
  let corresp = function 
      0  -> (4,1)
    | 1 -> (6,1)
    | 2 -> (3,2)
    | 3 -> (5,2)
    | 4 -> (7,2)
    | 5 -> (2,3)
    | 6 -> (4,3)
    | 7 -> (6,3)
    | 8 -> (8,3)
    | 9 -> (1,4)
    | 10 -> (3,4)
    | 11 -> (5,4)
    | 12 -> (7,4)
    | 13 -> (9,4)
    | 14 -> (2,5)
    | 15 -> (4,5)
    | 16 -> (6,5)
    | 17 -> (8,5)
    | _ -> (0,0)

  let corresp2 = function
      (0,0) -> 0
    |  (0,1) -> 1
    |  (1,0) -> 2
    |  (1,1) -> 3
    |  (1,2) -> 4
    |  (2,0) -> 5
    |  (2,1) -> 6
    |  (2,2) -> 7
    |  (2,3) -> 8
    |  (3,0) -> 9
    |  (3,1) -> 10
    |  (3,2) -> 11
    |  (3,3) -> 12
    |  (3,4) -> 13
    |  (4,0) -> 14
    |  (4,1) -> 15
    |  (4,2) -> 16
    |  (4,3) -> 17
    | (x,y) -> failwith "corresp2"

  let col =5
  let lig = 5

  (* draw_background : unit -> unit  *)
  (* dessine le fond de l'ecran  *)
  let draw_background () =
    Graphics.clear_graph() ;
    Graphics.set_color cBlue ;
    Graphics.fill_rect bounds bounds largeur (hauteur-top_offset)


  (* draw_places : unit -> unit  *)
  (* dessine les cases du plateau de jeu  *)
  let draw_places () =
    for l = 0 to 17 do
      let cp = corresp l in
      if cp <> (0,0) then
        begin
          Graphics.set_color cBlack ;
          Graphics.draw_circle 
            ((fst cp)*30 + virtual_table_xoffset) 
	    (hauteur - ((snd cp)*55 + 25)-50) (rayon+1) ;
          Graphics.set_color cGray ;
          Graphics.fill_circle ((fst cp)*30 + virtual_table_xoffset) 
	    (hauteur - ((snd cp)*55 + 25)-50) rayon 
        end
    done

  (* draw_force_lines : unit -> unit  *)
  (* dessine les lignes de force  *)
  let draw_force_lines () =  
    Graphics.set_color cYellow ;
    let lst = 
      [((2,1),(6,1)); ((1,2),(7,2)); ((0,3),(8,3)); ((-1,4),(9,4)) ; 
       ((0,5),(8,5)); ((5,0),(1,4)); ((7,0),(2,5)); ((8,1),(4,5))  ; 
       ((9,2),(6,5)); ((10,3),(8,5)); ((3,6),(1,4)); ((5,6),(2,3)) ; 
       ((7,6),(3,2)); ((9,6),(4,1)); ((10,5),(6,1))                ] in
    let rec lines = function  
	[] -> ()
      | h::t -> 
	  let deb = fst h and fin = snd h in 
          Graphics.moveto ((fst deb)*30 + virtual_table_xoffset) 
	    (hauteur - ((snd deb)*55 + 25)-50) ;
          Graphics.lineto ((fst fin)*30 + virtual_table_xoffset) 
	    (hauteur - ((snd fin)*55 + 25)-50) ;
          lines t
    in lines lst 

  (* draw_final_places : unit -> unit  *)
  (* dessine les cases finales pour chaque ligne de force *)
  let draw_final_places () =
    let lst = [(2,1); (1,2); (0,3); (-1,4); (0,5); (3,6); (5,6); (7,6) ;
	       (9,6); (10,5); (10,3); (9,2); (8,1); (7,0); (5,0)       ] in
    let rec final = function 
        [] -> ()
      | h::t ->  Graphics.set_color cBlack ;
          Graphics.draw_circle ((fst h)*30 + virtual_table_xoffset) 
	    (hauteur - ((snd h)*55 + 25)-50) (rayon+1) ;
          Graphics.set_color cGreen ;
          Graphics.fill_circle ((fst h)*30 + virtual_table_xoffset) 
	    (hauteur - ((snd h)*55 + 25)-50) rayon ;
          final t
    in final lst


  (* draw_table : unit -> unit  *)
  (* dessine tout le plateau de jeu  *)
  let draw_table () =
    Graphics.set_color cYellow ;
    draw_background () ;
    Graphics.set_line_width 5 ;
    draw_force_lines () ;
    Graphics.set_line_width 2 ;
    draw_places () ;
    draw_final_places ();
    Graphics.set_line_width 1
  
  (* coup -> couleur -> unit  *)
  let draw_jeton  joueur  (n_case,P cp)  = (*   (n_caOccup(c,v),cp) col =*)
    Graphics.set_color (if joueur then cBlack else cRed); (*col;*)
    let co = corresp n_case in
    let x = ((fst co)*30 + 145) and y = (hauteur - ((snd co)*55 + 25)-50) in
    Graphics.fill_circle x y rayon ;
    Graphics.set_color cWhite ;
    Graphics.moveto (x - 3) (y - 3) ;
    let dummy = 5 in
    Graphics.draw_string (string_of_int cp) (*;*)

  (* conv : Graphics.status -> int  *)
  (* convertit un clic souris en une position dans un tableau fictif *)
  (* permettant de dessine  *)
  let conv st =
    let xx = st.Graphics.mouse_x and yy = st.Graphics.mouse_y in
    let y = (yy+10)/virtual_case_size - 6 in
    let dec = 
      if y = ((y/2)*2) then 60 else 40 in
    let offset = match (-1*y) with 
      0 -> -2
    | 1 -> -1
    | 2 -> -1
    | 3 -> 0
    | 4 -> -1
    | _ -> 12 in
    let x = (xx+dec)/virtual_case_size - 3 + offset in 
    (-1*y, x)

  (* line_number_to_aff : int -> int*int *)
  (* convertit un numero de ligne en une position dans le tableau fictif   *)
  (* servant a dessiner la coordonnee renvoyee correspond a la case finale *)
  (* de la ligne  *)
  let line_number_to_aff = function 
      0 -> (2,1)
    | 1 -> (1,2)
    | 2 -> (0,3)
    | 3 -> (-1,4)
    | 4 -> (0,5)
    | 5 -> (5,0)
    | 6 -> (7,0)
    | 7 -> (8,1)
    | 8 -> (9,2)
    | 9 -> (10,3)
    | 10 -> (3,6)
    | 11 -> (5,6)
    | 12 -> (7,6)
    | 13 -> (9,6)
    | 14 -> (10,5)
    | _ -> failwith "line" (*raise Rep.Out_of_bounds*)

  (* draw_lines_won : jeu -> unit  *)
  (* positionne un marqueur indiquant le joueur qui a remporte la ligne  *)
  (* ceci est fait pour toutes les lignes  *)
  let  drawb l i = 
    match l with
      Rien -> failwith "draw" 
    | M j  -> 
	let pos = line_number_to_aff i in
	Graphics.set_color (if j then cBlack else cRed);
        Graphics.fill_rect ((fst pos)*30 + virtual_table_xoffset-bounds)  
          (hauteur - ((snd pos)*55 + 25)-60) 20 40 

  let draw_lines_won om nm  = 
    for i=0 to 14 do 
      if om.(i) <> nm.(i) then drawb nm.(i) i
    done

  (* draw_poss : item list -> int -> unit  *)
  (* dessine les pions disponibles pour un joueur a partir d'une liste *)
  (* le parametre off indique la position ou mettre la liste  *)
  let draw_poss joueur lst off =
    let c = ref (1) in
    let rec draw = function 
	[] -> ()
      | v::t -> 
	  if joueur  then Graphics.set_color cBlack
	  else Graphics.set_color cRed;
          let x = off   
	  and y = 0+(!c)*50 in
          Graphics.fill_circle x y rayon ;
          Graphics.set_color cWhite ;
          Graphics.moveto (x - 3) (y - 3) ;
          Graphics.draw_string (string_of_int v) ;
          c := !c + 1 ;
          draw t 
    in draw (List.map (function P x -> x) lst)

  (* draw_choix : jeu -> unit *)
  (* dessine la liste des pions encore disponibles pour chaque joueur  *)
  let draw_choix (J (ca,ma,r1,r2)) =
    Graphics.set_color cBlue ;
    Graphics.fill_rect (choix_noir_offset-30) 10 60
      (hauteur - (top_offset+bounds)) ;
    Graphics.fill_rect (choix_rouge_offset-30) 10 60
      (hauteur - (top_offset+bounds)) ;
    draw_poss true r1 choix_noir_offset ;
    draw_poss false r2 choix_rouge_offset 

  (* wait_click : unit -> unit  *)
  (* attend un clic souris  *)
  let wait_click () = Graphics.wait_next_event [Graphics.Button_down]


  (* item list -> item  *)
  (* renvoie le pion choisi par l'utilisateur pour jouer  *)
  let select_pion joueur  lst =
    let ok = ref false and
	choix = ref 99 and
	pion = ref (P(-1))
    in
    while not !ok do
      let st = wait_click () in
      let size = List.length lst in
      let x = st.Graphics.mouse_x and y = st.Graphics.mouse_y in
      choix := (y+25)/50 - 1 ;
      ok := !choix <= size && 
	( (joueur && x < 65 ) || ( (not joueur) && (x > 535)) ) ;      
      if !ok then
	try 
          pion := (List.nth  lst !choix) ;
          Graphics.set_color cGreen ;
          Graphics.set_line_width 2 ;
          Graphics.draw_circle 
            (if joueur then choix_noir_offset else choix_rouge_offset) 
            ((!choix+1)*50) (rayon + 1)
	with _  -> ok := false ;
    done ;
    !pion

  (* choixH : jeu -> coup  *)
  (* retourne un coup du joueur humain  *)
  (* RETOURNE LE CHOIX DU NUMERO  LA CASE ET DE LA PIERRE *)

  let rec choix joueur jeu = match jeu with (J(ca,ma,r1,r2)) ->
    let choix = ref (P(-1))  
    and c = ref (-1, P(-1)) in
    let lcl = coups_legaux joueur jeu in 
    while not (List.mem !c lcl) do
      print_newline();print_string "CHOIX";
      List.iter (fun (c,P p) -> print_string "["; print_int c;print_string " ";
        print_int p;print_string "]") 
        (coups_legaux joueur jeu);
      draw_choix jeu;
      choix :=  select_pion joueur  (if joueur then r1 else r2) ;
      c :=  (corresp2  (conv (wait_click())), !choix)
    done ;
    !c (* case, pierre *)
      

  (* accueil : unit -> unit  *)
  (* affiche un message d'accueil a l'utilisateur  *)
  let accueil () =
    Graphics.open_graph
      (" " ^ (string_of_int (largeur + 10)) ^ "x" ^ 
       (string_of_int (hauteur + 10)) ^ "+50+50") ;
    Graphics.moveto (hauteur / 2) (largeur / 2) ;
    Graphics.set_color cBlue ;
    Graphics.draw_string "Stone henge" ;
    Graphics.set_color cBlack ;
    Graphics.moveto 2 2 ;
    Graphics.draw_string "Mixte Projets Maitrise & DESS GLA" ;
    ignore (wait_click ()) ;
    Graphics.clear_graph ()

  (* fin : unit -> unit *)
  (* on ferme le tout !  *)
  let fin () = Graphics.close_graph () 
      
  (* draw_boutton : int -> int -> int -> int -> string -> unit  *)
  (* dessine un bouton avec un message  *)
  let draw_boutton x y w h s =
    Graphics.set_line_width 1 ;
    Graphics.set_color cBlack ; 
    Graphics.moveto x y ;
    Graphics.lineto x (y+h) ;
    Graphics.lineto (x+w) (y+h) ;
    Graphics.lineto (x+w) y ;
    Graphics.lineto x y ;
    Graphics.moveto (x+bounds) (hauteur - (top_offset/2)) ;
    Graphics.draw_string s

  (* draw_message : string -> unit  *)
  (* affiche un message  *)
  let draw_message s = 
    Graphics.set_color cBlack;
    Graphics.moveto 3 (hauteur - (top_offset/2)) ;
    Graphics.draw_string s

  (* efface_message : unit -> unit  *)
  (* comme son nom l'indique  *)
  let efface_message () =
    Graphics.set_color Graphics.white;
    Graphics.fill_rect 0 (hauteur-top_offset+bounds) largeur top_offset 

  (* question : string -> bool  *)
  (* pose une question a l'utilisateur et attend une reponse oui ou non *)
  let question s = 
    let xb1 = (largeur/2) and xb2 = (largeur/2 + 30) and wb = 25 and hb = 16 
    and yb = hauteur - 20  in
    let rec attente () = 
      let e = wait_click () in
      if (e.Graphics.mouse_y < (yb+hb)) & (e.Graphics.mouse_y > yb) then 
	if (e.Graphics.mouse_x > xb1) & (e.Graphics.mouse_x < (xb1+wb)) then 
          true
	else 
          if (e.Graphics.mouse_x > xb2) & (e.Graphics.mouse_x < (xb2+wb)) then 
            false
          else 
            attente()
      else
	attente () in 
    draw_message s;
    draw_boutton xb1 yb wb hb "oui";
    draw_boutton xb2 yb wb hb "non"; 
    attente()

  (* q_commencer : unit -> bool *)
  (* demande si l'utilisateur desire jouer en premier ou non  *)
  let q_commencer () = 
    let b = question "Voulez-vous commencer ?" in
    efface_message();
    b

  (* q_continuer : unit -> bool *)
  (* demande si l'utilisateur desire rejouer un autre partie *)
  let q_continuer () = 
    let b = question "Encore une partie ?" in
    efface_message();
    b
  (* gagne : unit -> unit  *)
  (* un message indiquant que la machine a gagne  *)
  let gagne () = 
    draw_message "Je gagne :-)"; 
    ignore (wait_click()); 
    efface_message()
      
  (* perd : unit -> unit  *)
  (* un message indiquant que la machine a perdu *)
  let perd () = 
    draw_message "Vous gagnez :-("; 
    ignore (wait_click()); 
    efface_message()

  (* nul : unit -> unit  *)
  (* un message indiquant que personne ne peut gagner  *)
  let nul () = 
    draw_message "Partie nulle"; 
    ignore (wait_click()); 
    efface_message()

  (* init : unit -> unit *)
  (* dessine le plateau de jeu  *)
  let init () = let jeu = jeu_depart () in draw_table () ; draw_choix jeu 

  let print_menhir m = match m with
    Rien -> print_string "Rien "
  | M j -> print_string ("Jo "^(if j then "1 " else "2 "))
	
  let affiche joueur coup 
      (J(ca1,m1,r11,r12)) (J(ca2,m2,r21,r22) as nouveau_jeu) = 
    draw_jeton joueur coup;
    draw_choix nouveau_jeu;
    draw_lines_won m1 m2

  let q_jouer () = 
    let b = question "Est-ce une machine qui joue?" in 
    efface_message ();
    b
end;;

open Alphabeta 
module Stone_squeletteG = 
  FSquelette (Stone_rep) (Stone_graph) (Stone_eval)
    (FAlphabeta (Stone_rep) (Stone_eval)) ;;

Stone_squeletteG.prof := 2;;

module Stone_mainG = FMain(Stone_squeletteG);;

Stone_mainG.main () ;; 
















