(* #load "graphics.cma";;
#load "types_et_donnees.cmo";;
#load "generation_de_terrains.cmo";; *)
(* à décommenter si non compilé *)

open Graphics;;
open Types_et_donnees;;
open Generation_de_terrains;;

let taille_x = m*20;;
let taille_y = n*20;;
let offset = 20;;

let ouvrir () = open_graph (" "^string_of_int(taille_x+2*offset)^"x"^string_of_int(taille_y+2*offset));;

let cote = min (taille_y/n) (taille_x/m);;

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
;;

let bleu = rgb 72 118 255;;
let vert = rgb 0 100 0;;
let vert_clair = rgb 102 205 0;;
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
