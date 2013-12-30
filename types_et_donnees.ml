type element = Eau | Foret | Plaine | Maison | Centrale;;
type direction = Haut | Bas | Gauche | Droite | NO | NE | SO | SE;;
type mouvement = Up | Down | Left | Right;;
type action = Pompier | Water | Feu;;

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
let prob_foret = 0.434;;
let prob_plaine = 0.344;;
let prob_eau = 0.22;;
let prob_centrale = 0.002;;

let prob_maison = 0.15;;
let prob_foudre = 0.5;;

(* Variables globales pour la simulation *)
let wind = ref true;;
let wind_direction = ref SO;;
let foudre = ref false;;
let ia = ref false;;
let compteur_tour = ref 1;;
let action_souris = ref Pompier;;
let pompier_x = ref 0;;
let pompier_y = ref 0;;
let compteur_pompiers = ref 0;;
let liste_pompiers : ((int*int) list) ref = ref [];;
let fq_pomp = 10;;

(* Résolution max de l'écran *)
let max_x = 1280;;
let max_y = 1024;;

(*** Choix de l'utilisateurs ***)

(* Taille du terrain *)
print_endline "Caracteristiques du terrain :";;
let n = (* lignes *)
  print_endline "Nombre de lignes ?";
  read_int();;
let m = (* colonnes *)
  print_endline "Nombre de colonnes ?";
  read_int();;
print_endline "Effets du climat :";;
print_endline "Direction initiale du vent : Nord (n), Nord-Ouest (m), Ouest (o), Sud-Ouest (l), Sud (s), Sud-Est (k), Est (e), Nord-Est (j) ou pas de vent (p)";;
wind_direction := (match read_line() with
	| "n" -> Nord
	| "s" -> Sud
	| "o" -> Ouest
	| "e" -> Est
	| "m" -> NO
	| "l" -> SO
	| "k" -> SE
	| "j" -> NE
	| _ -> wind := false; Nord);;
print_endline "Pluie? (y/n)";;
let rain := ( match read_line() with
    | "y" -> true
    | _ -> false);;
print_endline "Foudre? (y/n)";;
let thunder := ( match read_line() with
    | "y" -> true
    | _ -> false);;
	

let advanced_simulation =
  print_endline "Voulez-vous lancer la simulation avancee (a) ou simple (s) ? (A/s)";
  match read_line() with
    | "s" -> print_endline "Simulation simple"; false
    | _ -> print_endline "Simulation avancee"; true
;;

let quantite_feu = int_of_float(ceil(float(n*m)/.(100.*.log(float(n*m)))));;
