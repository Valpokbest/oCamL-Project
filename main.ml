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
  let fin = ref false in (*arret du programme*)
  
  while (not(!fin)) do
    if objectif 0 0 = (-1,-1) then (affiche_score (score ()) (score_max ()); (*si l'objectif d'un pompier est (-1,-1) ça signifie qu'aucune case n'est enflammée donc la partie est finie*)
				    fin := recommencer_partie ());
    if (thunder && (!compteur_tour mod 10) = 0 && (Random.float 1.) < prob_foudre) then foudre := true;
    let stat = wait_next_event [Button_down; Key_pressed] in (*attente d'un évènement!*)
    let x = stat.mouse_x in
    let y = stat.mouse_y in

    if stat.keypressed then (*touche pressée*)
      match stat.key with
	| ' '  -> 
	  unite_temps (); incr(compteur_tour);
	  if !ia then begin (*actions à effectuer si l'ia est sélectionnée*)
	    action_ia_fonce(); (*déplacer les pompiers, voir ia.ml*)
	    dessine();
	    if !compteur_pompiers > 0 then poser_pompier (); (*poser un pompier quand disponible*)
	  end;
	  actualiser_tour(); (*maj graphique*)
    	  if (!compteur_tour mod fq_pomp = 0) then (incr compteur_pompiers; actualiser_nombre_pompiers()); (* tous les fq_pomp tours on ajoute un pompier*)
	  dessine ()
	| 'c' -> fin:=true; close_graph()
	| 'k' -> sauver terrain 1
	| 'l' -> charger 1; dessine ()
	| 'z' -> move_pompier Up
	| 'q' -> move_pompier Left
	| 'd' -> move_pompier Right
	| 's' -> move_pompier Down
	| 'i' -> ia := not(!ia)
	| 'P' -> dessine_cheat(); action_souris := Pompier
	| 'W' -> dessine_cheat(); action_souris := Water
	| 'F' -> dessine_cheat(); action_souris := Feu
	| 'I' -> dessine_cheat(); incr compteur_pompiers; actualiser_nombre_pompiers()
	| _ -> () 

    else (*si clic de souris*)
      begin
	let i,j = indices_souris x y in
	try (
	let case = terrain.(i).(j) in
	match (!action_souris) with
	  | Feu -> (*on allume on on éteint une case*)
	    if case.intensite_feu = 0 then (allumer_feu case; dessine_case i j)
	    else (eteindre_feu case; dessine_case i j)
	  | Water -> (*on met une case d'eau*)
	    terrain.(i).(j) <- init_case(Eau); dessine_case i j
	  | Pompier -> (*pour poser un pompier ou en sélectionner un*)
	  	if (case.intensite_feu = 0 && case.element != Eau) then (*sécurité*)
	  	begin
	  	if (!compteur_pompiers > 0 && case.pompier = 0) then
		  begin
	    	    case.pompier <- 1; decr(compteur_pompiers); actualiser_nombre_pompiers();
		    liste_pompiers := (j,i)::(!liste_pompiers); (*attention à bien inverser j et i*)
		  end;
	    	dessine_case i j; pompier_x := j; pompier_y := i (*sélection du pompier posé ou d'un déjà posé*)
	    end;

	) with _ -> ()
      end;
	  

	  
    foudre := false;
  done;
;;

main ();;
