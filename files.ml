(*#load "types_et_donnees.cmo";;
#load "generation_de_terrains.cmo";;*)
(* à décommenter si non compilé *)

open Types_et_donnees;;
open Generation_de_terrains;;

let sauver carte num = 
	let nb_rows=Array.length carte and nb_cols=Array.length carte.(0) in
	let fichier = open_out ("Saves/"^string_of_int(num)^".map") in
	output_string fichier (string_of_int(nb_rows));
	output_string fichier "\n";
	output_string fichier (string_of_int(nb_cols));
	output_string fichier "\n";
	for i=0 to nb_rows-1 do
		for j=0 to nb_cols-1 do
			let biome = match carte.(i).(j).element with
				|Eau -> "00"
				|Foret -> "01"
				|Plaine -> "02"
				|Maison -> "03" in
			output_string fichier biome;
			output_string fichier (string_of_int(carte.(i).(j).intensite_feu));
			if (carte.(i).(j).calcine) then output_string fichier "1" else output_string fichier "0";
			if (carte.(i).(j).estompe) then output_string fichier "1" else output_string fichier "0";
			if (carte.(i).(j).brule) then output_string fichier "1" else output_string fichier "0";
			output_string fichier (string_of_int(carte.(i).(j).pompier));
			output_string fichier "\n";
		done;
	done;
	close_out fichier;;
	
let charger num = 
	let fichier = open_in ("Saves/"^string_of_int(num)^".map") in
	let nb_rows = int_of_string(input_line fichier) in
	let nb_cols = int_of_string(input_line fichier) in
	for i=0 to nb_rows-1 do
		for j=0 to nb_cols-1 do
			let premier_code = Char.escaped (input_char fichier) in
			let code_biome = premier_code^(Char.escaped (input_char fichier)) in
			print_string code_biome;
			let elt = (match code_biome with
				|"00" -> Eau
				|"01" -> Foret
				|"02" -> Plaine
				|"03" -> Maison) in
			let case = init_case(elt) in
			case.intensite_feu <- int_of_char(input_char fichier);
			case.calcine <- (match (input_char fichier) with
				|'0' -> false
				|'1' -> true);
			case.estompe <- (match (input_char fichier) with
				|'0' -> false
				|'1' -> true);
			case.brule <- (match (input_char fichier) with
				|'0' -> false
				|'1' -> true);
			case.pompier <- int_of_char(input_char fichier);
			terrain.(i).(j) <- case;
		done;
	done;;
