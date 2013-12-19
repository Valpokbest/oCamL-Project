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


let collision (i,j) =
  let retour = ref false in
  if terrain.(i).(j).element = Eau then retour := true;
  if terrain.(i).(j).pompier > 0 then retour := true;
  if terrain.(i).(j).intensite_feu > 0 then retour := true;
  !retour;;

let action_ia_fonce () =
 let liste  = !liste_pompiers in

  let deplace_pompier (x,y) =
    pompier_x := x;
    pompier_y := y;
    let objectifx = ref 0 and objectify = ref 0 in
    let distance_min = ref (n*m) in
    for i=0 to n-1 do
      for j=0 to m-1 do
        if (terrain.(i).(j).intensite_feu > 0) then
          begin
          let d = (abs (j-x)+abs (i-y)) in
          if d < !distance_min then
          begin
            objectifx := j;
            objectify := i;
            distance_min := d;
          end
          end;
      done;
    done;
	(*print_int(!pompier_x);
	print_newline();
	print_int(!pompier_y);
	print_newline();*)
	let coince = ref (!objectifx = 0 || !objectify = 0) in
    while (terrain.(!pompier_y).(!pompier_x).pompier < 4 && not(!coince)) do
    if abs(!objectifx- !pompier_x) > abs(!objectify - !pompier_y) then
      if !objectifx - !pompier_x > 0 then
		if (not(collision(!pompier_y,!pompier_x+1))) then
			move_pompier Right
		else
			if !objectify - !pompier_y > 0 then
				(if (not(collision(!pompier_y+1,!pompier_x))) then
					move_pompier Down)
			else
				if (not(collision(!pompier_y-1,!pompier_x))) then
					move_pompier Up
				else
					coince:=true
      else
		if (not(collision(!pompier_y,!pompier_x-1))) then
			move_pompier Left
		else
			if !objectify - !pompier_y > 0 then
				(if (not(collision(!pompier_y+1,!pompier_x))) then
					move_pompier Down)
			else
				if (not(collision(!pompier_y-1,!pompier_x))) then
					move_pompier Up
				else
					coince:=true
    else
      if !objectify - !pompier_y > 0 then
        if (not(collision(!pompier_y+1,!pompier_x))) then
			move_pompier Down
		else
			if !objectifx - !pompier_x > 0 then
				(if (not(collision(!pompier_y,!pompier_x+1))) then
					move_pompier Right)
			else
				if (not(collision(!pompier_y,!pompier_x-1))) then
					move_pompier Left
				else
					coince:=true
      else
        if (not(collision(!pompier_y-1,!pompier_x))) then
			move_pompier Up
		else
			if !objectifx - !pompier_x > 0 then
				(if (not(collision(!pompier_y,!pompier_x+1))) then
					move_pompier Right)
			else
				if (not(collision(!pompier_y,!pompier_x-1))) then
					move_pompier Left
				else
					coince:=true;
	done;
	(!pompier_x, !pompier_y)
  in

  let rec deplacer_pompiers = function
    | [] -> []
    | t::q -> (deplace_pompier t)::(deplacer_pompiers q)
  in

liste_pompiers := deplacer_pompiers liste;;

