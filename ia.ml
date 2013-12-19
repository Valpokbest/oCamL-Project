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
	print_int(300);
	print_newline();
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
	print_int(!pompier_x);
	print_newline();
	print_int(!pompier_y);
	print_newline();
    while (terrain.(!pompier_y).(!pompier_x).pompier < 4) do
    if abs(!objectifx- !pompier_x) > abs(!objectify - !pompier_y) then
	
      if !objectifx - !pompier_x > 0 then
	  
        (
	print_int(2000);
	print_newline();if (not(collision(!pompier_x+1,!pompier_y))) then
          begin
          move_pompier Right;
          end)
      else
        (
	print_int(3000);
	print_newline();if (not(collision(!pompier_x-1,!pompier_y))) then
            begin
              move_pompier Left;
            end)
    else
      if !objectify - !pompier_y > 0 then
        (if (not(collision(!pompier_x,!pompier_y+1))) then
          begin
	print_int(5000);
	print_newline();
          move_pompier Down;
          end)
      else
        (if (not(collision(!pompier_x,!pompier_y-1))) then
	print_int(6000);
	print_newline();
            begin
              move_pompier Up;
            end);
    done;
	(!pompier_x, !pompier_y)
  in

  let rec deplacer_pompiers = function
    | [] -> []
    | t::q -> (deplace_pompier t)::(deplacer_pompiers q)
  in

liste_pompiers := deplacer_pompiers liste;;
