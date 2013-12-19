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
  let retour = ref true in
  if terrain.(i).(j).element = Eau then retour := false;
  if terrain.(i).(j).pompier > 0 then retour := false;
  if terrain.(i).(j).intensite_feu > 0 then retour := false;
  !retour;;

let action_ia_fonce () =
 let liste  = !liste_pompiers in

  let deplace_pompier (x,y) =
    pompier_x := x;
    pompier_y := y;
    let objectifx = ref 0 and objectify = ref 0 in
    let distance_min = ref (n*m) in
    for i=0 to n do
      for j=0 to m do
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
    while (terrain.(!pompier_y).(!pompier_x).pompier < 4) do
    if abs(!objectifx- !pompier_x) > abs(!objectify - !pompier_y) then
      if !objectifx - !pompier_x > 0 then
        (if (not(collision(!pompier_x+1,!pompier_y))) then
          begin
          move_pompier Right;
          incr(pompier_x);
          end)
      else
        (if (not(collision(!pompier_x-1,!pompier_y))) then
            begin
              move_pompier Left;
              decr(pompier_x);
            end)
    else
      if !objectify - !pompier_y > 0 then
        (if (not(collision(!pompier_x,!pompier_y+1))) then
          begin
          move_pompier Down;
          incr(pompier_y);
          end)
      else
        (if (not(collision(!pompier_x,!pompier_y-1))) then
            begin
              move_pompier Up;
              decr(pompier_y);
            end);
    done;
  in

  let rec deplacer_pompiers = function
    | [] -> []
    | t::q -> (deplace_pompier t)::(deplacer_pompiers q)
  in

liste_pompiers := deplacer_pompiers liste;;

