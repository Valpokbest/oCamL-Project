type element = Eau | Foret | Plaine | Maison;;
type direction = Haut | Bas | Gauche | Droite | NO | NE | SO | SE;;
type mouvement = Up | Down | Left | Right;;
type action = Feu | Pompier | Water;;

type case = {
  element : element;
  mutable intensite_feu : int;
  mutable calcine : bool;
  mutable estompe : bool;
  mutable brule : bool;
  mutable pompier : int
};;

type terrain = case array array;;

(* Densités des terrains : la somme doit valoir 1 *)
let prob_foret = 0.43;;
let prob_plaine = 0.35;;
let prob_eau = 0.22;;

let prob_maison = 0.15;;
let prob_foudre = 0.5;;

(* Variables globales pour la simulation *)
let advanced_simulation = true;;
let wind = true;;
let wind_direction = SO;;
let rain = false;;
let thunder = true;;

let action_souris = ref Feu;;
let pompier_x = ref 0;;
let pompier_y = ref 0;;
let compteur_pompiers = ref 0;;

(* Taille du terrain *)
let n = 20;; (* lignes *)
let m = 30;; (* colonnes *)
