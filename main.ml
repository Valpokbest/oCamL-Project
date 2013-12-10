(*#load "graphics.cma";;
#load "types_et_donnees.cmo";;
#load "generation_de_terrains.cmo";; 
#load "fenetre_graphique.cmo";;
#load "calculs.cmo";;
#load "files.cmo";;*)
(* à décommenter si non compilé *)

open Graphics;;
open Types_et_donnees;;
open Generation_de_terrains;;
open Fenetre_graphique;;
open Calculs;;
open Files;;

let main () =
  ouvrir();
  set_window_title "The one and only one Fire Project";
  dessine ();
  let fin = ref false in
  let compteur_tour = ref 1 in
  let foudre = ref false in
  
  while (not(!fin)) do
    if (thunder && !compteur_tour = 0 && (Random.float 1.) < prob_foudre) then foudre := true;
    let stat = wait_next_event [Button_down;Key_pressed] in
    let x = stat.mouse_x in
    let y = stat.mouse_y in
    
    if stat.keypressed then
      begin
	if stat.key = ' ' then 
	  begin
	    unite_temps (!foudre);
			incr(compteur_tour);
	    dessine ();
	  end
	else if stat.key = 'c' then
	  begin
	    fin:=true;
	    close_graph();
	  end
	else if stat.key = 'p' then
	  action_souris := Pompier
	else if stat.key = 'a' then
	  sauver terrain 1
	else if stat.key = 'z' then
	  move_pompier Up
	else if stat.key = 'q' then
	  move_pompier Left
	else if stat.key = 'd' then
	  move_pompier Right
	else if stat.key = 's' then
	  move_pompier Down
	else if stat.key = 'w' then
	  action_souris := Water
	else if stat.key = 'f' then
	  action_souris := Feu;
      end

    else
      begin
	let i,j = indices_souris x y in
	try (
	let case = terrain.(i).(j) in
	match (!action_souris) with
	  |Feu -> 
	    if case.intensite_feu = 0 then (allumer_feu case; dessine_case i j)
	  |Water ->
	    terrain.(i).(j) <- init_case(Eau); dessine_case i j
	  |Pompier ->
	  	begin
	  	if (!compteur_pompiers > 0 && case.pompier = 0 && case.element != Eau) then
	    	(case.pompier <- 1; decr(compteur_pompiers))
	    	dessine_case i j; pompier_x := j; pompier_y := i
	    	end;

	) with _ -> ()
      end;
    foudre := false;
    if (!compteur_tour = 10) then (compteur_tour := 0; incr(compteur_pompiers))
  done;
;;

main ();;
