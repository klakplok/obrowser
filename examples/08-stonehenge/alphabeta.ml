(*****************************************************************************)
(*****  Devellopement d'applications avec Objective Caml                 *****)
(*****                                                                   *****)
(*****  Application : Jeux a deux joueurs : Alpha-Beta                   *****)
(*****************************************************************************)

module type REPRESENTATION = sig
  type jeu
  type coup
  val jeu_depart : unit -> jeu
  val coups_legaux: bool -> jeu -> coup list
  val jouer: bool -> coup -> jeu -> jeu
end

module type EVAL = 
sig
  type jeu
  val evaluer: bool -> jeu -> int
  val plusI : int
  val moinsI: int
  val est_feuille: bool -> jeu -> bool
  val est_stable: bool -> jeu -> bool
  type etat = G | P | N | C
  val etat_de : bool -> jeu -> etat
end

module type ALPHABETA = sig 
  type jeu
  type coup
  val alphabeta : int -> bool -> jeu -> coup
end ;;

module type FALPHABETA  = 
  functor (Rep : REPRESENTATION) ->
  functor (Eval : EVAL with type jeu = Rep.jeu) ->
    ALPHABETA with type jeu = Rep.jeu and type coup = Rep.coup ;;

(***************************************************************************)

module FAlphabetaO 
    (Rep : REPRESENTATION)  (Eval : EVAL with type jeu = Rep.jeu)  = 
  struct
    type jeu = Rep.jeu
    type coup = Rep.coup
    exception AlphaCoupure of int 
    exception BetaCoupure of int 
 
    let maxmin_iter noeud minmax_cur beta alpha cp =
      let alpha_resu =  
        max alpha (minmax_cur (Rep.jouer true cp noeud) beta alpha) 
      in if alpha_resu >= beta then  raise (BetaCoupure alpha_resu)  
         else alpha_resu 

    let minmax_iter noeud maxmin_cur alpha beta cp =
      let beta_resu = 
        min beta (maxmin_cur (Rep.jouer false cp noeud) alpha beta) 
      in if beta_resu <= alpha then  raise (AlphaCoupure beta_resu)  
         else beta_resu     

    let rec maxmin prof noeud alpha beta =
      if    (prof < 1 & Eval.est_stable true noeud) 
         or Eval.est_feuille true noeud
      then Eval.evaluer true noeud  
      else 
        try  let prev = maxmin_iter noeud (minmax (prof - 1)) beta
             in List.fold_left prev alpha (Rep.coups_legaux true noeud)
        with BetaCoupure a -> a

    and minmax prof noeud beta alpha =
      if    (prof < 1 & Eval.est_stable false noeud) 
         or Eval.est_feuille false noeud
      then Eval.evaluer false noeud 
      else 
        try  let prev = minmax_iter noeud (maxmin (prof - 1)) alpha
             in List.fold_left prev beta (Rep.coups_legaux false noeud)
        with AlphaCoupure b -> b

    let rec cherche a l1 l2 = match (l1,l2) with 
        (h1::q1, h2::q2) -> if a = h1 then h2  else cherche a q1 q2
      | ([], []) -> failwith  ("AB: "^(string_of_int a)^" non trouve'")
      | (_ ,  _) -> failwith  "AB: longueur diff" 

    (* val alphabeta : int -> bool -> Rep.jeu -> Rep.coup *)
    let alphabeta prof joueur  racine = 
      let alpha = ref Eval.moinsI and beta = ref Eval.plusI in
      let l = ref [] in
      let cpl = Rep.coups_legaux joueur  racine in
      let eval =
        try 
          for i = 0 to (List.length cpl) - 1 do
            if joueur then 
              let b = Rep.jouer joueur (List.nth cpl i) racine in
              let a = minmax (prof-1) b !beta !alpha 
              in l := a :: !l ; 
                 alpha := max !alpha a ;
                 (if !alpha >= !beta then raise (BetaCoupure !alpha) )
            else
              let a = Rep.jouer joueur (List.nth cpl i) racine in
              let b = maxmin (prof-1) a !alpha !beta 
              in l := b :: !l ;
                 beta := min !beta b ;
                 (if !beta <= !alpha then raise (AlphaCoupure !beta))
            done ;
          if joueur then !alpha else !beta
        with 
              BetaCoupure a -> a
           |  AlphaCoupure b -> b
      in    
         l := List.rev !l ;
         cherche eval !l cpl
  end ;;

module FAlphabeta = (FAlphabetaO : FALPHABETA);;

(***************************************************************************)

module type SQUELETTE = sig 
  val accueil: unit -> unit
  val init: unit -> ((unit -> unit) * (unit -> unit))
  val encore: unit -> bool
  val fin: unit -> unit
  exception Gagne
  exception Perd
  exception Nul
  val gagne: unit -> unit
  val perd: unit -> unit
  val nul: unit -> unit
end ;;

module FMain (P : SQUELETTE) = 
  struct
    let ca_joue tours = while true do (fst tours) () ; (snd tours) () done

    let main () = let fini = ref false 
                  in P.accueil ();
                     while not !fini do
                       ( try  ca_joue (P.init ())
                         with P.Gagne -> P.gagne ()
                            | P.Perd  -> P.perd ()
                            | P.Nul   -> P.nul ()    );
                       fini := not (P.encore ())
                     done ;
                     P.fin ()
  end ;;

module type AFFICHAGE = sig 
  type jeu 
  type coup
  val accueil: unit -> unit
  val fin: unit -> unit
  val gagne: unit -> unit
  val perd: unit -> unit
  val nul: unit -> unit
  val init: unit -> unit
  val affiche : bool -> coup -> jeu -> jeu -> unit
  val choix : bool ->  jeu -> coup
  val q_jouer : unit -> bool   
  val q_commencer : unit -> bool
  val q_continuer : unit -> bool
end ;;

module FSquelette 
    (Rep   : REPRESENTATION) 
    (Aff   : AFFICHAGE with type jeu = Rep.jeu and  type coup = Rep.coup) 
    (Eval  : EVAL with type jeu = Rep.jeu) 
    (Alpha : ALPHABETA with type jeu = Rep.jeu and  type coup = Rep.coup) = 
  struct
    let prof = ref 4
    exception Gagne
    exception Perd
    exception Nul
    let gagne = Aff.gagne
    let perd = Aff.perd
    let nul = Aff.nul
    let encore = Aff.q_continuer
    let le_jeu = ref (Rep.jeu_depart())
    let fin = Aff.fin
    let accueil = Aff.accueil 

    let tourH joueur () = 
      let choix = Aff.choix joueur !le_jeu in
      let ancien_jeu = !le_jeu 
      in le_jeu := Rep.jouer joueur choix !le_jeu ;
         Aff.affiche joueur choix ancien_jeu !le_jeu ;
         match Eval.etat_de joueur !le_jeu with
             Eval.P -> raise Perd
           | Eval.G -> raise Gagne
           | Eval.N -> raise Nul
           | _      -> ()

    let tourM joueur () = 
      let choix = Alpha.alphabeta !prof joueur !le_jeu in 
      let ancien_jeu = !le_jeu 
      in le_jeu := Rep.jouer joueur choix !le_jeu ;
         Aff.affiche joueur choix ancien_jeu !le_jeu ;
         match Eval.etat_de joueur !le_jeu with
             Eval.G -> raise Gagne
           | Eval.P -> raise Perd
           | Eval.N -> raise Nul
           | _      -> ()

    let init () =  
       let a = Aff.q_jouer () in
       let b = Aff.q_jouer() 
       in le_jeu :=Rep.jeu_depart () ;
          Aff.init () ; 
          match (a,b) with  
              true,true   -> tourM true, tourM false
            | true,false  -> tourM true, tourH false
            | false,true  -> tourH true, tourM false
            | false,false -> tourH true, tourH false
  end ;;


