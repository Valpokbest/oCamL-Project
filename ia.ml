(*#load "types_et_donnees.cmo";;
#load "fenetre_graphique.cmo";;
#load "calculs.cmo";;
#load "generation_de_terrains.cmo";;*)
(* à décommenter si non compilé *)

open Graphics;;
open Types_et_donnees;;
open Fenetre_graphique;;
open Calculs;;
open Generation_de_terrains;;

(*gestion des collisions*)
let collision (i,j) =
  let retour = ref false in
  if i<0 || j<0 || i>n || j>m then retour := true;
  if not(!retour) && terrain.(i).(j).element = Eau then retour := true;
  if not(!retour) && terrain.(i).(j).pompier > 0 then retour := true;
  if not(!retour) && terrain.(i).(j).intensite_feu > 0 then retour := true;
  !retour;;

let rec poser_pompier () =
 (* print_string("poser_pompier()");
  print_newline();*)
  if !compteur_pompiers > 0 then
    begin

      if tout_eteint () then ()
      else begin

	let max = ref 0 and i = ref 0 and j = ref 0 in
	for i1=0 to n-1 do
	  for j1=0 to m-1 do
	    if (not(collision (i1,j1))) then (*si c'est une case potentielle pour accueillir le pompier*)
	      begin
		let homogene = ref 0 in
		if (i1>0) then
		  begin
		    if (j1>0) then
		      if (terrain.(i1-1).(j1-1).intensite_feu > 0) then
			incr(homogene);
		    if (terrain.(i1-1).(j1).intensite_feu > 0) then
		    incr(homogene);
		    if (j1<m-1) then
		      if (terrain.(i1-1).(j1+1).intensite_feu > 0) then
			incr(homogene);
		  end;
		if (j1<m-1) then
		  if (terrain.(i1).(j1+1).intensite_feu > 0) then
		    incr(homogene);
		if (i1<n-1) then
		  begin
		    if (j1<m-1) then
		      if (terrain.(i1+1).(j1+1).intensite_feu > 0) then
			incr(homogene);
		    if (terrain.(i1+1).(j1).intensite_feu > 0) then
		      incr(homogene);
		    if (j1>0) then
		      if (terrain.(i1+1).(j1-1).intensite_feu > 0) then
			incr(homogene);
		  end;
		if (i1>0) then
		  if (terrain.(i1-1).(j1).intensite_feu > 0) then
		    incr(homogene);
		if (!homogene > !max) then (*on a calculé le nb de cases en feu voisines*)
		  begin
		    max:=!homogene;
		    i:=i1;
		    j:=j1;
		  end (* le maximum est retenu*)
	      end;
	  done;
	done;
	liste_pompiers := (!j,!i)::(!liste_pompiers); (*on ajoute le pompier a la liste*)
	decr compteur_pompiers;
	actualiser_nombre_pompiers ();
	terrain.(!i).(!j).pompier <- 1; (*et au terrain*)
	dessine_case (!i) (!j);
	poser_pompier ();
      end;
      
    end
;;

(*fonction pour calculer la case en feu la plus proche du pompier en (x,y)*)
let objectif x y =
  (*print_string("objectif()");
  print_newline();*)
  let objectifx = ref (-1) and objectify = ref (-1) in
  let distance_min = ref (n*m) in
  for i=0 to n-1 do
    for j=0 to m-1 do
      if (terrain.(i).(j).intensite_feu > 0) then
        begin
          let d = (abs (j-x)+abs (i-y)) in (*calcul de la distance*)
          if d < !distance_min then
            begin
              objectifx := j;
              objectify := i;
              distance_min := d;
            end
        end;
    done;
  done;
  (!objectifx, !objectify);;

let objectif_score x y =
  let objectifx = ref (-1) and objectify = ref (-1) in
  let distance_min = ref (n*m) in
  for i=0 to n-1 do
    for j=0 to m-1 do
      if (terrain.(i).(j).intensite_feu > 0) then
        begin
			let score = (match terrain.(i).(j).element with
			|Plaine -> 10
			|Foret -> 5
			|Maison -> 2
			|_ -> 1) in
          let d = score*(abs (j-x)+abs (i-y)) in (*calcul de la distance scorée*)
          if d < !distance_min then
            begin
              objectifx := j;
              objectify := i;
              distance_min := d;
            end
        end;
    done;
  done;
  (!objectifx, !objectify);;

let objectif_feu x y =
  (*print_string("objectif()");
  print_newline();*)
  let objectifx = ref (-1) and objectify = ref (-1) in
  let distance_min = ref (n*m) in
  for i=0 to n-1 do
    for j=0 to m-1 do
      if (terrain.(i).(j).intensite_feu > 0) then
        begin
          let d = terrain.(i).(j).intensite_feu*(abs (j-x)+abs (i-y)) in (*calcul de la distance*)
          if d < !distance_min then
            begin
              objectifx := j;
              objectify := i;
              distance_min := d;
            end
        end;
    done;
  done;
  (!objectifx, !objectify);;


let action_ia_fonce () =
  (*print_string("action_ia_fonce()");
  print_newline();*)
  let liste  = !liste_pompiers in
  
  let deplace_pompier (x,y) =
    pompier_x := x;
    pompier_y := y;
    
    let (objectifx,objectify) = (match num_ia with
	|2 -> objectif_score x y
	|3 -> objectif_feu x y
	|_ -> objectif x y) in
   (* print_string("Objectif trouvé");
    print_newline();*)
	(*print_int(!objectifx);
	  print_newline();
	  print_int(!objectify);
	  print_newline();
	  print_int(!pompier_x);
	  print_newline();
	  print_int(!pompier_y);
	  print_newline();
	  print_newline();*)
    let coince = ref (objectifx = (-1) || objectify = (-1)) in (*pas de case en feu*)
    while (terrain.(!pompier_y).(!pompier_x).pompier < 4 && not(!coince)) do
      if abs(objectifx- !pompier_x) > abs(objectify - !pompier_y) then (*déplacement selon x*)
	begin
	  if objectifx - !pompier_x > 0 then
	    begin
	     (* print_string("Direction Droite");
	      print_newline();*)
	      if (not(collision(!pompier_y,!pompier_x+1))) then
		move_pompier Right
	      else
		begin
		  if objectify - !pompier_y > 0 then
		    if (not(collision(!pompier_y+1,!pompier_x))) then
		      move_pompier Down
		    else 
		      coince:=true
		  else
		    if (not(collision(!pompier_y-1,!pompier_x))) then
		      move_pompier Up
		    else
		      coince:=true
		end
	    end
	  else
	    begin
	     (* print_string("Direction Gauche");
	      print_newline();*)
	      if (not(collision(!pompier_y,!pompier_x-1))) then
		move_pompier Left
	      else
		begin
		  if objectify - !pompier_y > 0 then
		    if (not(collision(!pompier_y+1,!pompier_x))) then
		      move_pompier Down
		    else 
		      coince:=true
		  else
		    if (not(collision(!pompier_y-1,!pompier_x))) then
		      move_pompier Up
		    else
		      coince:=true
		end
	    end
	end
      else (*déplacement selon y*)
	begin
	  if objectify - !pompier_y > 0 then
	    begin
	     (* print_string("Direction Bas");
	      print_newline();*)
              if (not(collision(!pompier_y+1,!pompier_x))) then
		move_pompier Down
		else
		begin
		  if objectifx - !pompier_x > 0 then
		    if (not(collision(!pompier_y,!pompier_x+1))) then
		      move_pompier Right
		    else 
		      coince:=true
		  else
		    if (not(collision(!pompier_y,!pompier_x-1))) then
		      move_pompier Left
		    else
		      coince:=true
		end
	    end
	  else
	    begin
	      (*print_string("Direction Haut");
	      print_newline();*)
              if (not(collision(!pompier_y-1,!pompier_x))) then
		move_pompier Up
	      else
		begin
		  if objectifx - !pompier_x > 0 then
		    if (not(collision(!pompier_y,!pompier_x+1))) then
		      move_pompier Right
		    else 
		      coince:=true
		  else
		    if (not(collision(!pompier_y,!pompier_x-1))) then
		      move_pompier Left
		    else
		      coince:=true
		end
	    end
	end;
    done;
    (!pompier_x, !pompier_y)
  in
  
  let rec deplacer_pompiers = function (*on applique le déplacement à tous les pompiers*)
    | [] -> []
    | t::q -> 
    (*  print_string("deplacer_pompiers()");
      print_newline();*)
      (deplace_pompier t)::(deplacer_pompiers q)
  in
  
  liste_pompiers := deplacer_pompiers liste;;
