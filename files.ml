(*#load "types_et_donnees.cmo";;
#load "generation_de_terrains.cmo";;
#load "fenetre_graphique.cmo";;*)
(* à décommenter si non compilé *)

open Types_et_donnees;;
open Generation_de_terrains;;
open Fenetre_graphique;;

let sauver carte num = 
  let nb_rows=Array.length carte and nb_cols=Array.length carte.(0) in
  let fichier = open_out ("Saves/"^string_of_int(num)^".map") in
  (*on stocke diverses informations :
  les compteurs pour la simulation*)
  output_string fichier (string_of_int(!compteur_tour));
  output_string fichier "\n";
  output_string fichier (string_of_int(!compteur_pompiers));
  output_string fichier "\n";
  (*la taille du terrain*)
  output_string fichier (string_of_int(nb_rows));
  output_string fichier "\n";
  output_string fichier (string_of_int(nb_cols));
  output_string fichier "\n";
  for i=0 to nb_rows-1 do
    for j=0 to nb_cols-1 do
	(*une ligne par case pour écrire tout l'enregistrement*)
      let biome = match carte.(i).(j).element with
	| Eau -> "00"
	| Foret -> "01"
	| Plaine -> "02"
	| Maison -> "03"
	| Centrale -> "04" in
      output_string fichier biome;
      if (carte.(i).(j).contamine) then output_string fichier "1" else output_string fichier "0";
      output_string fichier (string_of_int(carte.(i).(j).intensite_feu));
      if (carte.(i).(j).calcine) then output_string fichier "1" else output_string fichier "0";
      if (carte.(i).(j).estompe) then output_string fichier "1" else output_string fichier "0";
      if (carte.(i).(j).brule) then output_string fichier "1" else output_string fichier "0";
      output_string fichier (string_of_int(carte.(i).(j).pompier));
      if (carte.(i).(j).pompier < 10) then
      output_string fichier ("0"^(string_of_int(carte.(i).(j).pompier)))
      else output_string fichier (string_of_int(carte.(i).(j).pompier));
      output_string fichier "\n";
    done;
  done;
  close_out fichier;;

let charger num = 
  try (
  let fichier = open_in ("Saves/"^string_of_int(num)^".map") in
  let ct = int_of_string(input_line fichier) in
  let cp = int_of_string(input_line fichier) in
  let nb_rows = int_of_string(input_line fichier) in
  let nb_cols = int_of_string(input_line fichier) in
  liste_pompiers := [];

  if (nb_rows = n && nb_cols = m) then (*on vérifie que le terrain est de la même taille que celui chargé*)
    begin
      compteur_tour := ct;
      actualiser_tour ();
      compteur_pompiers := cp;
      actualiser_nombre_pompiers ();
      for i=0 to nb_rows-1 do
	for j=0 to nb_cols-1 do
	  let premier_code = String.make 1 (input_char fichier) in
	  let deuxieme_code = String.make 1 (input_char fichier) in
	  let code_biome = premier_code^deuxieme_code in (*code un peu long mais String.make 2 (input_char fichier)^(input_char fichier) ne fonctionne pas!*)
	  let elt = (match code_biome with
	    | "00" -> Eau
	    | "01" -> Foret
	    | "02" -> Plaine
	    | "03" -> Maison
	    | "04" -> Centrale
	    | _ -> Eau) in
	  let case = init_case(elt) in (*on recrée la case selon les infos stockées*)
	  case.contamine <- (match (input_char fichier) with
	    | '0' -> false
	    | '1' -> true
	    | _ -> false);
	  case.intensite_feu <- int_of_string(String.make 1 (input_char fichier));
	  let c = input_char fichier in
	  case.calcine <- (match c with
	    | '0' -> false
	    | '1' -> true
	    | _ -> false);
	  case.estompe <- (match (input_char fichier) with
	    | '0' -> false
	    | '1' -> true
	    | _ -> false);
	  case.brule <- (match (input_char fichier) with
	    | '0' -> false
	    | '1' -> true
	    | _ -> false);
	  let pompier = int_of_string(String.make 1 (input_char fichier)) in
	  case.pompier <- pompier;
	  if (pompier > 0) then liste_pompiers := (j,i)::(!liste_pompiers); (*toujours gérer la double structure sur les pompiers*)
	  let premier_code = String.make 1 (input_char fichier) in
	  let deuxieme_code = String.make 1 (input_char fichier) in
	  case.pv <- int_of_string(premier_code^deuxieme_code);
	  let _ = input_line fichier in
	  terrain.(i).(j) <- case;
	done;
      done;
    end;
  ) with _ -> ();;
