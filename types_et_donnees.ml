(* Résolution max de l'écran *)
let max_x = 1280;;
let max_y = 1024;;

type element = Eau | Foret | Plaine | Maison | Centrale;; (*Pour le type d'une case*)
type direction = Haut | Bas | Gauche | Droite | NO | NE | SO | SE;; (*Pour le vent*)
type mouvement = Up | Down | Left | Right;; (*Pour les mouvements des pompiers*)
type action = Pompier | Water | Feu;; (*Action de la souris*)

type case = {
  element : element;				(*Type de la case*)
  mutable contamine : bool;			(*true si la case est contaminée par le nuage radioactif *)
  mutable intensite_feu : int;
  mutable calcine : bool;			(*true si la case a fini de bruler*)
  mutable estompe : bool;			(*true si intensite_feu = intensite_max*)
  mutable brule : bool;				(*true si intensite_feu >= intensite_max/2*)
  mutable pompier : int;			(*0 = pas de pompier sur la case ; sinon valeur-1 = nb de pas effectués durant le tour (cad 4 = pompier coincé)*)
  mutable pv : int					(*Nombre de Points de Vie du pompier sur la case*)
};;

type terrain = case array array;;	(*matrice pour le terrain*)

(* Densités des terrains : la somme doit valoir 1 *)
let prob_foret = 0.434;;
let prob_plaine = 0.344;;
let prob_eau = 0.22;;
let prob_centrale = 0.002;;

let prob_maison = 0.15;;

(*probabilité que la foudre tombe tous les 10 tours*)
let prob_foudre = 0.5;;

(* Variables globales pour la simulation *)
let wind_direction = ref SO;;
let foudre = ref false;; (*cette variable sert à dire si la foudre doit frapper lors d'un tour*)
let ia = ref false;;
let compteur_tour = ref 1;;
let action_souris = ref Pompier;;
let pompier_x = ref 0;;		(*coords du pompier sélectionné*)
let pompier_y = ref 0;;
let compteur_pompiers = ref 0;;
let liste_pompiers : ((int*int) list) ref = ref [];;	(*l'emplacement des pompiers est aussi géré sous forme de liste*)
let fq_pomp = 10;;	(*Nb de tours avant arrivée d'un pompier*)
let pv_pompier = 10;; (*Points de vie max d'un pompier*)
let nuage : ((int*int) list) ref = ref [];; (*pour stocker les cases où le nuage radioactif se trouve*)

let buffer = Array.create 10 'a';;
let super_pompier = ref false;;
let combinaison = ref false;;

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
print_endline "Direction initiale du vent : Nord (n), Nord-Ouest (m), Ouest (o), Sud-Ouest (l), Sud (s), Sud-Est (k), Est (e), Nord-Est (j) ou Pas de vent (p)";;
let wind =
  let k = ref true in
  wind_direction := (match read_line() with
    | "n" -> Haut
    | "s" -> Bas
    | "o" -> Gauche
    | "e" -> Droite
    | "m" -> NO
    | "l" -> SO
    | "k" -> SE
    | "j" -> NE
    | _ -> k := false; Haut);
!k;;
print_endline "Pluie ? (y/N)";;
let rain = ( match read_line() with
    | "y" -> true
    | _ -> false);;
print_endline "Foudre ? (y/N)";;
let thunder = ( match read_line() with
    | "y" -> true
    | _ -> false);;

print_endline "Choisissez une IA (1,2,3).";;
let num_ia = ( match read_line() with
|"2" -> 2
|"3" -> 3
|_ -> 1);;

let advanced_simulation =
  print_endline "Voulez-vous lancer la simulation avancee (a) ou simple (s) ? (A/s)";
  match read_line() with
    | "s" -> print_endline "Simulation simple"; false
    | _ -> print_endline "Simulation avancee"; true
;;

let quantite_feu = 1;;
