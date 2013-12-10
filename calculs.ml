(*#load "graphics.cma";;
#load "types_et_donnees.cmo";;
#load "generation_de_terrains.cmo";; 
#load "fenetre_graphique.cmo";;*)
(* à décommenter si non compilé *)

open Graphics;;
open Types_et_donnees;;
open Generation_de_terrains;;
open Fenetre_graphique;;


let score () = 
  let score = ref 0 in
  for i=0 to n-1 do
    for j=0 to m-1 do
      let case = terrain.(i).(j) in
      if (not(case.calcine)) then
	match case.element with
	  | Eau -> ()
	  | Plaine -> score := !score + 1
	  | Foret -> score := !score + 2
	  | Maison -> score := !score + 3;
    done;
  done;
  !score;;

let allumer_feu case =
  if not(case.calcine) then
    begin
      match case.element with
	| Eau -> ()
	| _ -> case.pompier <- 0; case.intensite_feu <- 1;
    end
;;

let maj_feu case i = (*i=1 pour augmenter feu; i=2 pour diminuer feu*)
  if (case.pompier = 0) then
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
	  case.intensite_feu <- case.intensite_feu-2;
	  if (case.intensite_feu < 0) then case.intensite_feu <- 0;
	  if (case.brule && case.intensite_feu = 0) then case.calcine <- true;
	end
    end
;;

let clone case = {element = case.element; intensite_feu = case.intensite_feu; estompe = case.estompe; calcine = case.calcine; brule = case.brule; pompier = case.pompier};;

let coeff_vent i =
  match wind_direction with
	| Haut -> if (i < 4) then 0.25 else if (i>6) then -1. else 0.
	| Bas -> if (i > 6) then 0.25 else if (i < 4) then -1. else 0.
	| Gauche -> if ((i mod 3) = 1) then 0.25 else if ((i mod 3) = 0) then -1. else 0.
	| Droite -> if ((i mod 3) = 0) then 0.25 else if ((i mod 3) = 1) then -1. else 0.
	| SO -> if (i=4 || i=7 || i=8) then 0.25 else if (i=2 || i=3 || i=6) then -1. else 0.
	| SE -> if (i=6 || i=8 || i=9) then 0.25 else if (i=1 || i=2 || i=4) then -1. else 0.
	| NO -> if (i=4 || i=7 || i=8) then -1. else if (i=2 || i=3 || i=6) then 0.25 else 0.
	| NE -> if (i=6 || i=8 || i=9) then -1. else if (i=1 || i=2 || i=4) then 0.25 else 0.;;

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

let move_pompier dir =
	let possible = match dir with
		|Up -> (!pompier_y)>0 && terrain.(!pompier_y-1).(!pompier_x).element != Eau && terrain.(!pompier_y-1).(!pompier_x).pompier = 0
		|Down -> (!pompier_y)<n && terrain.(!pompier_y+1).(!pompier_x).element != Eau && terrain.(!pompier_y+1).(!pompier_x).pompier = 0
		|Left -> (!pompier_x)>0 && terrain.(!pompier_y).(!pompier_x-1).element != Eau && terrain.(!pompier_y).(!pompier_x-1).pompier = 0
		|Right -> (!pompier_x)<m && terrain.(!pompier_y).(!pompier_x+1).element != Eau && terrain.(!pompier_y).(!pompier_x+1).pompier = 0 in
		
  if (terrain.(!pompier_y).(!pompier_x).pompier < 4 && possible) then
    begin
      let (i,j) =
	match dir with
	  | Up ->  (!pompier_y-1,!pompier_x)
	  | Down ->  (!pompier_y+1,!pompier_x)
	  | Left -> (!pompier_y,!pompier_x-1)
	  | Right -> (!pompier_y,!pompier_x+1) in
	terrain.(i).(j).pompier <- terrain.(!pompier_y).(!pompier_x).pompier + 1;
      	terrain.(!pompier_y).(!pompier_x).pompier <- 0;
      dessine_case (!pompier_y) (!pompier_x);
      pompier_x := j;
      pompier_y := i;
      dessine_case (i) (j);
    end;;

let unite_temps foudre =
  let n = Array.length terrain in
  let m = Array.length terrain.(0) in
  let new_terrain = Array.make_matrix n m (init_case(Foret)) in
  
  
  for i=0 to n-1 do
    for j=0 to m-1 do
      new_terrain.(i).(j) <- clone (terrain.(i).(j));
			if (new_terrain.(i).(j).pompier > 0) then new_terrain.(i).(j).pompier <- 1;
    done;
  done;
  
  for i=0 to n-1 do
    for j=0 to m-1 do
      let feu = ref false in
      let pompier_a_cote = ref false in
      if (i>0) then
	begin
	  if (j>0) then
	    if (terrain.(i-1).(j-1).pompier > 0) then pompier_a_cote := true else
	      if (terrain.(i-1).(j-1).intensite_feu > 0 && (not(advanced_simulation) || (calcul_proba (terrain.(i-1).(j-1)) 25. 9) > (Random.float 100.)) ) then feu := true;
	  
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
      else if (!feu) then maj_feu new_terrain.(i).(j) 1
    done;
  done;
  
  for i=0 to n-1 do
    for j=0 to m-1 do
      terrain.(i).(j) <- new_terrain.(i).(j);
    done;
  done;
  
  if (foudre) then
    if not(tout_en_feu()) then
      begin
	let i_foudre = ref (Random.int n) in
	let j_foudre = ref (Random.int m) in
	while (not(terrain.(!i_foudre).(!j_foudre).element <> Eau && terrain.(!i_foudre).(!j_foudre).intensite_feu = 0 && not(terrain.(!i_foudre).(!j_foudre).calcine))) do
	  i_foudre := Random.int n;
	  j_foudre := Random.int m;
	done;

	allumer_feu terrain.(!i_foudre).(!j_foudre);
	dessine_foudre (!i_foudre) (!j_foudre);
      end;;


