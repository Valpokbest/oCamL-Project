(* #load "generation_de_terrains.cmo";; 
#load "fenetre_graphique.cmo";;
#load "calculs.cmo";;
#load "files.cmo";;
#load "ia.cmo";;*)
(* à décommenter si non compilé *)

open Graphics;;
open Types_et_donnees;;
open Generation_de_terrains;;
open Fenetre_graphique;;
open Calculs;;
open Files;;
open Ia;;

let main () =
  ouvrir();
  set_window_title "The one and only one Fire Project";
  dessine ();
  let fin = ref false in
  
  while (not(!fin)) do
    if objectif 0 0 = (-1,-1) then (affiche_score (score ()) (score_max ());
				    fin := recommencer_partie ());
    if (thunder && !compteur_tour = 0 && (Random.float 1.) < prob_foudre) then foudre := true;
    let stat = wait_next_event [Button_down; Key_pressed] in
    let x = stat.mouse_x in
    let y = stat.mouse_y in

    if stat.keypressed then
      match stat.key with
	| ' '  -> 
	  unite_temps (); incr(compteur_tour);
	  if !ia then begin
	    action_ia_fonce();
	    dessine();
	    if !compteur_pompiers > 0 then poser_pompier ();
	  end;
	  actualiser_tour();
    	  if (!compteur_tour mod fq_pomp = 0) then (incr compteur_pompiers; actualiser_nombre_pompiers());
	  dessine ()
	| 'c' -> fin:=true; close_graph()
	| 'k' -> sauver terrain 1
	| 'l' -> charger 1; dessine ()
	| 'z' -> move_pompier Up
	| 'q' -> move_pompier Left
	| 'd' -> move_pompier Right
	| 's' -> move_pompier Down
	| 'p' -> action_souris := Pompier
	| 'W' -> action_souris := Water
	| 'F' -> action_souris := Feu
	| 'i' -> ia := not(!ia)
	| _ -> () 

    else
      begin
	let i,j = indices_souris x y in
	try (
	let case = terrain.(i).(j) in
	match (!action_souris) with
	  | Feu -> 
	    if case.intensite_feu = 0 then (allumer_feu case; dessine_case i j)
	    else (eteindre_feu case; dessine_case i j)
	  | Water ->
	    terrain.(i).(j) <- init_case(Eau); dessine_case i j
	  | Pompier ->
	  	if (case.intensite_feu = 0 && case.element != Eau) then
	  	begin
	  	if (!compteur_pompiers > 0 && case.pompier = 0) then
		  begin
	    	    case.pompier <- 1; decr(compteur_pompiers); actualiser_nombre_pompiers();
		    liste_pompiers := (j,i)::(!liste_pompiers);
		  end;
	    	dessine_case i j; pompier_x := j; pompier_y := i
	    	end;

	) with _ -> ()
      end;
	  

	  
    foudre := false;
  done;
;;

main ();;
