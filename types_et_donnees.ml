type element = Eau | Foret | Plaine | Maison | Centrale;;
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
let prob_plaine = 0.34;;
let prob_eau = 0.22;;
let prob_centrale = 0.01;;

let prob_maison = 0.15;;
let prob_foudre = 0.5;;

(* Variables globales pour la simulation *)
let advanced_simulation = true;;
let wind = true;;
let wind_direction = SO;;
let rain = false;;
let thunder = true;;

let compteur_tour = ref 1;;
let action_souris = ref Feu;;
let pompier_x = ref 0;;
let pompier_y = ref 0;;
let compteur_pompiers = ref 0;;
let liste_pompiers = ref [];;

(* Taille du terrain *)
let n = 30;; (* lignes *)
let m = 40;; (* colonnes *)
