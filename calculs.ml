(*#load "graphics.cma";;
#load "types_et_donnees.cmo";;
#load "generation_de_terrains.cmo";; 
#load "fenetre_graphique.cmo";;*)
(* à décommenter si non compilé *)

open Graphics;;
open Types_et_donnees;;
open Generation_de_terrains;;
open Fenetre_graphique;;

(*Fonction pour calculer le score final*)
let score () = 
  let score = ref 0 in
  for i=0 to n-1 do
    for j=0 to m-1 do
      let case = terrain.(i).(j) in
      if (not(case.calcine)) then
	match case.element with
	  | Eau -> ()
	  | Centrale -> score := !score + 5
	  | Plaine -> score := !score + 1
	  | Foret -> score := !score + 2
	  | Maison -> score := !score + 3;
    done;
  done;
  !score;;
  
(*Fonction pour calculer le score maximal*)
let score_max () = 
  let score = ref 0 in
  for i=0 to n-1 do
    for j=0 to m-1 do
      let case = terrain.(i).(j) in
	match case.element with
	  | Eau -> ()
	  | Centrale -> score := !score + 5
	  | Plaine -> score := !score + 1
	  | Foret -> score := !score + 2
	  | Maison -> score := !score + 3;
    done;
  done;
  !score;;

(*Fonction auxiliaire pour mettre à jour la liste des pompiers*)
let actualise_pompier x y k l =
  let liste = !liste_pompiers in

  let rec aux = function
    | [] -> []
    | t::q when t = (x,y) -> (k,l)::q
    | t::q -> t::(aux q)
  in

liste_pompiers := aux liste;;

(*Fonction auxiliaire pour supprimer un pompier de la liste des pompiers*)
let supprimer_pompier x y =
  let liste = !liste_pompiers in

  let rec aux = function
    | [] -> []
    | t::q when t = (x,y) -> q
    | t::q -> t::(aux q)
  in

liste_pompiers := aux liste;;

let allumer_feu case =
  if not(case.calcine) then
    begin
      match case.element with
	| Eau -> ()
	| _ -> case.pompier <- 0; case.intensite_feu <- 1;
    end
;;

(*Fonction de triche*)
let eteindre_feu case =
  case.estompe <- false;
  case.intensite_feu <- 0;
  case.brule <- false;;

let maj_feu case i = (*i=1 pour augmenter feu; i=2 pour diminuer feu*)
  if (case.pompier = 0 && not(case.calcine)) then (*on vérifie qu'il n'y a pas de pompier sur la case et qu'elle n'a pas déjà brulée entièrement*)
    begin
      if (i=1) then
	begin
	  if (case.estompe) then
	    begin
	      case.intensite_feu <- case.intensite_feu-1;
	      if (case.intensite_feu = 0 && case.brule) then
		case.calcine <- true;
	    end
		else
	    begin
	      if (case.intensite_feu = 0) then
		allumer_feu case
	      else
		begin
		  case.intensite_feu <- case.intensite_feu+1;
		  if (case.intensite_feu = (intensite_max case.element)/2+1) then case.brule <- true;
		  if case.intensite_feu = intensite_max case.element then case.estompe <- true;
		end
	    end
	end
      else if (i=2) then
	begin
		if (!super_pompier) then eteindre_feu case
	  else begin
		case.intensite_feu <- case.intensite_feu-2;
		if (case.intensite_feu < 0) then case.intensite_feu <- 0;
		if (case.brule && case.intensite_feu = 0) then case.calcine <- true;
	  end
	end
    end
;;

(*fonction pour éviter de dédoubler les cases dans la liste du nuage*)
let rec add_nuage l x y = match l with
|[] -> [(x,y)]
|a::q when a=(x,y) -> a::q
|a::q -> a::(add_nuage q x y);;

(*mise à jour des coordonnées des cases quand le nuage bouge!*)
let rec deplacer_nuage l = match l with
|[] -> []
|(x,y)::q -> match !wind_direction with
	|Haut -> if y>0 then (x,y-1)::(deplacer_nuage q) else deplacer_nuage q (*si on sort du terrain, le nuage disparaît*)
	|Bas -> if y<n-1 then (x,y+1)::(deplacer_nuage q) else deplacer_nuage q
	|Gauche -> if x>0 then (x-1,y)::(deplacer_nuage q) else deplacer_nuage q
	|Droite -> if x<m-1 then (x+1,y)::(deplacer_nuage q) else deplacer_nuage q
	|NO -> if y>0 && x>0 then (x-1,y-1)::(deplacer_nuage q) else deplacer_nuage q
	|NE -> if y>0 && x<m-1 then (x+1,y-1)::(deplacer_nuage q) else deplacer_nuage q
	|SE -> if y<n-1 && x<m-1 then (x+1,y+1)::(deplacer_nuage q) else deplacer_nuage q
	|SO -> if y<n-1 && x>0 then (x-1,y+1)::(deplacer_nuage q) else deplacer_nuage q;;
	
(*Fonction de debug*)
let rec afficher_nuage l = match l with
|[] -> ()
|(x,y)::q -> print_string("("); print_int(x); print_string(","); print_int(y); print_string(")"); print_newline();;
	
(*Contamination*)
let rec contaminer l = match l with
|[] -> ()
|(x,y)::q -> if terrain.(y).(x).element = Foret || terrain.(y).(x).element = Plaine || terrain.(y).(x).element = Maison then	
				terrain.(y).(x).contamine <- true;
			contaminer q;; (*on passe aux autres cases du nuage*)
				
(*Pour enlever la vie aux pompiers s'il sont sur des cases contaminées*)
let rec maladie l = match l with
	|[] -> ()
	|(x,y)::q -> if (terrain.(y).(x).pompier > 0 && terrain.(y).(x).contamine) then (*vérification*)
			begin
				terrain.(y).(x).pv <- terrain.(y).(x).pv-1;
				if (terrain.(y).(x).pv = 0) then
					begin
						terrain.(y).(x).pompier <- 0;
						supprimer_pompier x y;
						dessine_case y x;
					end;
			end;
			maladie q;;



(*fonction pour gérer l'explosion d'une centrale nucléaire*)
let explosion carte k l =
  let (coeffx,coeffy) = (0,0) in
  for i=0 to n-1 do
    for j=0 to m-1 do
      if (carte.(i).(j).element <> Eau) then
	if ((abs (i+coeffx-k) + abs (j+coeffy-l)) < 5) then (*5 est le rayon d'explosion*)
	  (carte.(i).(j).intensite_feu <- 0;
	   carte.(i).(j).calcine <- true;
	   carte.(i).(j).pompier <- 0;	(*on supprime les pompiers morts de la carte*)
	   supprimer_pompier j i;	(*et de la liste*)
	   nuage := add_nuage (!nuage) j i)
	else if ((abs (i+coeffx-k) + abs (j+coeffy-l)) = 5) then
	  (allumer_feu carte.(i).(j);
	   carte.(i).(j).pompier <- 0;
	   supprimer_pompier j i;
	   nuage := add_nuage (!nuage) j i);
      
    done;
  done;;

(*crée une nouvelle case avec les mêmes valeurs*)
let clone case = {element = case.element; contamine = case.contamine; intensite_feu = case.intensite_feu; estompe = case.estompe; calcine = case.calcine; brule = case.brule; pompier = case.pompier; pv = case.pv};;

(*calcule le coefficient du au vent pour la simulation*)
(*i est le numéro de la case voisine selon ce schéma:
     1  2  3
	 4  X  6
	 7  8  9  où X est la case en feu*)
let coeff_vent i =
  match !wind_direction with
	| Haut -> if (i < 4) then 0.25 else if (i>6) then -1. else 0.
	| Bas -> if (i > 6) then 0.25 else if (i < 4) then -1. else 0.
	| Gauche -> if ((i mod 3) = 1) then 0.25 else if ((i mod 3) = 0) then -1. else 0.
	| Droite -> if ((i mod 3) = 0) then 0.25 else if ((i mod 3) = 1) then -1. else 0.
	| SO -> if (i=4 || i=7 || i=8) then 0.25 else if (i=2 || i=3 || i=6) then -1. else 0.
	| SE -> if (i=6 || i=8 || i=9) then 0.25 else if (i=1 || i=2 || i=4) then -1. else 0.
	| NO -> if (i=4 || i=7 || i=8) then -1. else if (i=2 || i=3 || i=6) then 0.25 else 0.
	| NE -> if (i=6 || i=8 || i=9) then -1. else if (i=1 || i=2 || i=4) then 0.25 else 0.;;

(*calcule la proba que la case "case" avec le numéro num par rapport à une case en feu (=i de la fonction précédente) brule*)
let calcul_proba case facteur_distance num =
  let facteur_terrain = ref 0. in
  let intensite_max = ref 0 in
  if (case.element = Foret) then
    (facteur_terrain := 1.; intensite_max := 4)
  else if (case.element = Plaine) then
    (facteur_terrain := 0.5; intensite_max := 2)
  else if (case.element = Maison) then
    (facteur_terrain := 0.5; intensite_max := 8);
  let v = ref 0. in
  let p = ref 0. in
  if ( wind ) then
    v := (coeff_vent num);
  if (rain) then
    p := 0.25;
  (!facteur_terrain) *. facteur_distance *. (0.75)**( float_of_int(!intensite_max - case.intensite_feu)) *. (1. +. !v -. !p);;

(*vérifie s'il reste des cases enflammables ou non, pour gérer l'appel aléatoire sur une case de la foudre et éviter les boucles infinies*)
let tout_en_feu () =
  let check = ref true in
  let i = ref 0 in
  let j = ref 0 in
  
  while (!i < n && !check) do
    while (!j < m && !check) do
      if (terrain.(!i).(!j).intensite_feu = 0) && (terrain.(!i).(!j).element <> Eau) && not(terrain.(!i).(!j).calcine) then check := false;
      incr j
    done;
    j := 0;
    incr i
  done;
  
  !check;;

let tout_eteint () =
  let check = ref true in
  let i = ref 0 in
  let j = ref 0 in
  
  while (!i < n && !check) do
    while (!j < m && !check) do
      if (terrain.(!i).(!j).intensite_feu > 0) then check := false;
      incr j
    done;
    j := 0;
    incr i
  done;
  
  !check;;


let move_pompier dir =
 (*print_string("move_pompier()");
   print_newline();*)
  let possible = match dir with
    | Up -> (!pompier_y)>0 && terrain.(!pompier_y-1).(!pompier_x).element != Eau && terrain.(!pompier_y-1).(!pompier_x).pompier = 0 && terrain.(!pompier_y-1).(!pompier_x).intensite_feu = 0
    | Down -> (!pompier_y)<n && terrain.(!pompier_y+1).(!pompier_x).element != Eau && terrain.(!pompier_y+1).(!pompier_x).pompier = 0 && terrain.(!pompier_y+1).(!pompier_x).intensite_feu = 0
    | Left -> (!pompier_x)>0 && terrain.(!pompier_y).(!pompier_x-1).element != Eau && terrain.(!pompier_y).(!pompier_x-1).pompier = 0 && terrain.(!pompier_y).(!pompier_x-1).intensite_feu = 0
    | Right -> (!pompier_x)<m && terrain.(!pompier_y).(!pompier_x+1).element != Eau && terrain.(!pompier_y).(!pompier_x+1).pompier = 0 && terrain.(!pompier_y).(!pompier_x+1).intensite_feu = 0 in
  (*possible vérifie si la case de destination est atteignable*)
  if (terrain.(!pompier_y).(!pompier_x).pompier > 0 && terrain.(!pompier_y).(!pompier_x).pompier < 4 && possible) then (*si le pompier peut encore bouger
																													et s'il y a bien un pompier...*)
    begin
      let (i,j) =
	match dir with
	  | Up ->  (!pompier_y-1,!pompier_x)
	  | Down ->  (!pompier_y+1,!pompier_x)
	  | Left -> (!pompier_y,!pompier_x-1)
	  | Right -> (!pompier_y,!pompier_x+1)
      in
      terrain.(i).(j).pompier <- terrain.(!pompier_y).(!pompier_x).pompier + 1;
	  terrain.(i).(j).pv <- terrain.(!pompier_y).(!pompier_x).pv;
      terrain.(!pompier_y).(!pompier_x).pompier <- 0; (*on déplace le pompier*)
      dessine_case (!pompier_y) (!pompier_x);
      actualise_pompier (!pompier_x) (!pompier_y) j i; (*on stocke les coords dans la liste sous la forme (x,y)*)
      pompier_x := j; (*(x,y) = (j,i)*)
      pompier_y := i;
      dessine_case (i) (j); (*maj graphique*)
    end;;

let changer_direction_vent () =
	let num_vent = ref (match !wind_direction with (*conversion direction <=> entier (plus facile à manipuler)*)
		| Haut -> 0
		| Bas -> 4
		| Gauche -> 2
		| Droite -> 6
		| SO -> 3
		| NO -> 1
		| SE -> 5
		| NE -> 7) in
	let random = Random.int 8 in (*1 chance sur (8/2) que le vent change de direction (max conseille=10, min=3)*)
	if (random = 0) then incr(num_vent)
	else if (random = 1) then decr(num_vent); (*le vent change de direction de façon "continue"*)
	num_vent := !num_vent mod 8;
	wind_direction := (match !num_vent with
	| 0 -> Haut
	| 4 -> Bas
	| 2 -> Gauche
	| 6 -> Droite
	| 3 -> SO
	| 1 -> NO
	| 5 -> SE
	| 7 -> NE
	| _ ->Haut);;


let unite_temps () = (*représente un tour de jeu*)
  let n = Array.length terrain in
  let m = Array.length terrain.(0) in
  let new_terrain = Array.make_matrix n m (init_case(Foret)) in
  
  for i=0 to n-1 do
    for j=0 to m-1 do
      new_terrain.(i).(j) <- clone (terrain.(i).(j));
			if (new_terrain.(i).(j).pompier > 0) then new_terrain.(i).(j).pompier <- 1;
    done;
  done;
  (*on fait une copie du terrain pour enflammer les cases correctement*)
  for i=0 to n-1 do
    for j=0 to m-1 do
	(*pour chaque case on calcule les probas des 8 cases voisines de l'enflammer*)
      let feu = ref false in
      let pompier_a_cote = ref false in
      if (i>0) then
	begin
	  if (j>0) then
	  (*case en haut à gauche*)
	    if (terrain.(i-1).(j-1).pompier > 0) then pompier_a_cote := true else (*si il y a un pompier le comportement sera différent*)
	      if (terrain.(i-1).(j-1).intensite_feu > 0 && (not(advanced_simulation) || (calcul_proba (terrain.(i-1).(j-1)) 25. 9) > (Random.float 100.)) ) then feu := true; (*le paramètre est 9 et non 1 car la case étudiée est en position 9 par rapport à la case en position 1!*)
	  
	  if (terrain.(i-1).(j).pompier > 0) then pompier_a_cote := true else
	    if (terrain.(i-1).(j).intensite_feu > 0 && (not(advanced_simulation) || (calcul_proba (terrain.(i-1).(j)) 75. 8) > (Random.float 100.)) ) then feu := true;
	  
	  if (j<m-1) then
	    if (terrain.(i-1).(j+1).pompier > 0) then pompier_a_cote := true else
	      if (terrain.(i-1).(j+1).intensite_feu > 0 && (not(advanced_simulation) || (calcul_proba (terrain.(i-1).(j+1)) 25. 7) > (Random.float 100.)) ) then feu := true;
	end;
      
      if (j<m-1) then
	if (terrain.(i).(j+1).pompier > 0) then pompier_a_cote := true else
	  if (terrain.(i).(j+1).intensite_feu > 0 && (not(advanced_simulation) || (calcul_proba (terrain.(i).(j+1)) 75. 4) > (Random.float 100.)) ) then feu := true;
      
      if (j>0) then
	if (terrain.(i).(j-1).pompier > 0) then pompier_a_cote := true else
	  if (terrain.(i).(j-1).intensite_feu > 0 && (not(advanced_simulation) || (calcul_proba (terrain.(i).(j-1)) 75. 6) > (Random.float 100.)) ) then feu := true;
      
      if (i<n-1) then
	begin
	  if (j>0) then
	    if (terrain.(i+1).(j-1).pompier > 0) then pompier_a_cote := true else
	      if (terrain.(i+1).(j-1).intensite_feu > 0 && (not(advanced_simulation) || (calcul_proba (terrain.(i+1).(j-1)) 25. 3) > (Random.float 100.)) ) then feu := true;
	  
	  if (terrain.(i+1).(j).pompier > 0) then pompier_a_cote := true else
	    if (terrain.(i+1).(j).intensite_feu > 0 && (not(advanced_simulation) || (calcul_proba (terrain.(i+1).(j)) 75. 2) > (Random.float 100.)) ) then feu := true;
	  
	  if (j<m-1) then
	    if (terrain.(i+1).(j+1).pompier > 0) then pompier_a_cote := true else
	      if (terrain.(i+1).(j+1).intensite_feu > 0 && (not(advanced_simulation) || (calcul_proba (terrain.(i+1).(j+1)) 25. 1) > (Random.float 100.)) ) then feu := true;
	end;
      
      if (terrain.(i).(j).intensite_feu > 0) then feu:=true;
      
      if (!pompier_a_cote) then maj_feu new_terrain.(i).(j) 2
      else if (!feu) then
	if new_terrain.(i).(j).element = Centrale then (*explosion ou non?*)
	  explosion new_terrain i j
	else
	  maj_feu new_terrain.(i).(j) 1;
    done;
  done;
  
  for i=0 to n-1 do
    for j=0 to m-1 do
      terrain.(i).(j) <- new_terrain.(i).(j);
    done;
  done;
  
  changer_direction_vent ();
  actualiser_vent (); (*maj graphique*)
  
  if (!foudre) then
    if not(tout_en_feu()) then
      begin
	let i_foudre = ref (Random.int n) in
	let j_foudre = ref (Random.int m) in
	while (not(terrain.(!i_foudre).(!j_foudre).element <> Eau && terrain.(!i_foudre).(!j_foudre).intensite_feu = 0 && not(terrain.(!i_foudre).(!j_foudre).calcine))) do
	  i_foudre := Random.int n;
	  j_foudre := Random.int m;
	done;
	(*on prend des cases au hasard jusqu'à tomber sur une inflammable*)

	allumer_feu terrain.(!i_foudre).(!j_foudre);
	dessine_foudre (!i_foudre) (!j_foudre);
      end;;

let imprimer_retry () =
  print_endline "Voulez-vous recommencer ? (y/N)";
  match read_line() with
    | "y" -> true
    | _ -> false
;;

let recommencer_partie () =
	(*permet de refaire une partie sur le même terrain*)
  if (imprimer_retry()) then
    begin
      reinitialiser_terrain();
      allumer_terrain quantite_feu;
      
      foudre := false;
      ia := false;
      compteur_tour := 1;
      action_souris := Pompier;
      pompier_x := 0;
      pompier_y := 0;
      compteur_pompiers := 0;
      liste_pompiers := [];
      clear_graph();
      dessine();
      dessine_raccourcis();
      false;
    end
  else true;
;;
