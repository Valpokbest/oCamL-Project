(* #load "graphics.cma";;
#load "types_et_donnees.cmo";;
#load "generation_de_terrains.cmo";; *)
(* à décommenter si non compilé *)

open Graphics;;
open Types_et_donnees;;
open Generation_de_terrains;;

(* Résolution max de l'écran *)
let max_x = 1280;;
let max_y = 1024;;
let offset = 10;;

let taille_case = 20;; 
let largeur_raccourcis = 200;;
let hauteur_raccourcis = 220;;

let cote =
  let taille_x = min (max_x-3*offset-largeur_raccourcis-50) (m*taille_case) in
  let taille_y = min (max_y-2*offset-100) (n*taille_case) in

min (taille_y/n) (taille_x/m);;

let taille_x = m*cote;;
let taille_y = n*cote;;

let pos i j =
  (* Renvoie (x,y) la position du pixel en bas à gauche de la case correspondant à la ligne i et la colonne j *)
  (j*cote + offset, ((n-1)-i)*cote + offset)
;;

let divise_pos a b = if a >= 0 then a/b else -1;;

let indices_souris x y =
  let j = divise_pos (x - offset) cote in
  let i = (n-1) - divise_pos (y - offset) cote in
  
(i,j);;

let intensite_max ele = match ele with
  | Plaine -> 2
  | Foret -> 4
  | Maison -> 8
  | Eau -> -1
  | Centrale -> 1
;;

let bleu = rgb 72 118 255;;
let vert = rgb 0 100 0;;
let vert_clair = rgb 102 205 0;;
let vert_moche = rgb 151 188 56;;
let rouge case = match case.element with
  | Eau -> bleu
  | e -> rgb (50 + (200/(intensite_max(e))*case.intensite_feu)) 0 0
;;
let marron = rgb 139 79 19;;

let couleur case = match case.element with
  | Eau -> bleu
  | Foret -> vert
  | Plaine -> vert_clair
  | Maison -> marron
  | Centrale -> vert_moche
;;

let dessine_case i j =
  let case = terrain.(i).(j) in
  let x,y = pos i j in

  if case.calcine then
    (set_color black; 
     fill_rect x y (cote-1) (cote-1))
  else
    begin
      set_color (couleur case);
      fill_rect x y (cote-1) (cote-1);
      
      if case.intensite_feu > 0 then
	if case.estompe then
	  begin
	    set_color (rouge case);
	    for i = 0 to cote - 1 do
	      for j = 0 to cote - 1 do
	      if (i + j) mod 4 = 0 then plot (x+i) (y+j)
	      done;
	    done;
	  end
	else
	  begin
	    set_color (rouge case);
	    for i = 0 to cote - 1 do
	      for j = 0 to cote - 1 do
	      if (i - j) mod 4 = 0 then plot (x+i) (y+j)
	      done;
	    done;
	  end

    end;
  if case.pompier > 0 then
    begin
      set_color white;
      fill_rect (x+(cote-1)/3) (y+(cote-1)/8) ((cote-1)/3) (3*(cote-1)/4);
      fill_rect (x+(cote-1)/8) (y+(cote-1)/3) (3*(cote-1)/4) ((cote-1)/3);
    end;
;;

let dessine_raccourcis () =
  let debut_gauche = 2*offset + taille_x in
  let debut_haut = size_y()-2*offset in
  let ecart = 20 in

  set_color black;
  set_text_size 15;
  moveto (debut_gauche+20) debut_haut;
  draw_string "Commandes :";
  moveto debut_gauche (debut_haut-ecart);
  draw_string "'p' : selectionner pompier";
  moveto debut_gauche (debut_haut-2*ecart);
  draw_string "'f' : selectionner feu";
  moveto debut_gauche (debut_haut-3*ecart);
  draw_string "'l' : charger un terrain";
  moveto debut_gauche (debut_haut-4*ecart);
  draw_string "'c' : fermer la fenetre";
  moveto debut_gauche (debut_haut-5*ecart);
  draw_string "'SPACE' : passer au tour suivant";
  moveto (debut_gauche+20) (debut_haut-7*ecart);
  draw_string "Donnees :";
  moveto debut_gauche (debut_haut-8*ecart);
  draw_string "0 pompier disponible";
  moveto debut_gauche (debut_haut-9*ecart);
  draw_string "Tour 1";
;;

let actualiser_nombre_pompiers () =
  let debut_gauche = 2*offset + taille_x in
  let ecart = 20 in
  let debut_haut = size_y()-2*offset-8*ecart in

  set_color white;
  fill_rect debut_gauche debut_haut (offset+largeur_raccourcis) (ecart/2);
  set_color black;
  moveto debut_gauche debut_haut;

  let k = !compteur_pompiers in
  if k <= 1 then draw_string ((string_of_int (!compteur_pompiers))^" pompier disponible")
  else draw_string ((string_of_int (!compteur_pompiers))^" pompiers disponibles")
;;

let actualiser_tour () =
  let debut_gauche = 2*offset + taille_x in
  let ecart = 20 in
  let debut_haut = size_y()-2*offset-9*ecart in

  set_color white;
  fill_rect debut_gauche debut_haut (offset+largeur_raccourcis) (ecart/2);
  set_color black;
  moveto debut_gauche debut_haut;

  draw_string ("Tour "^(string_of_int (!compteur_tour)))
;;

let dessine () =
  for i = 0 to (n-1) do
    for j = 0 to (m-1) do
      dessine_case i j
    done;
  done;
;;

let dessine_foudre i j =
  let x,y = pos i j in
  set_color yellow;
  fill_rect x y (cote-1) (cote-1);;

let ouvrir () =
  open_graph (" "^string_of_int(taille_x+3*offset+largeur_raccourcis)^"x"^string_of_int(max hauteur_raccourcis (taille_y+2*offset)));
  dessine_raccourcis ()
;;
