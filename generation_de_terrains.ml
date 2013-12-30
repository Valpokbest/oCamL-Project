(* #load "types_et_donnees.cmo";; *)
(* à décommenter si non compilé *)
open Types_et_donnees;;


(* Initialisation de la graine *)
Random.self_init();;

let init_case (elt:element) =
  {element = elt; intensite_feu = 0; calcine = false; estompe = false; brule = false; pompier = 0;};;

let terrain_aleatoire hauteur largeur =
  let m = Array.make_matrix hauteur largeur (init_case(Foret)) in
  for i=0 to hauteur-1 do
    for j=0 to largeur-1 do
      let random = Random.int 1000 in
      if (random <= int_of_float(1000. *. prob_foret)) then
	m.(i).(j) <- init_case(Foret)
      else if (random <= int_of_float(1000. *. (prob_foret +. prob_plaine))) then
	m.(i).(j) <- init_case(Plaine)
      else if (random <= int_of_float(1000. *. (prob_foret +. prob_plaine +. prob_centrale))) then
	m.(i).(j) <- init_case(Centrale)
      else
	m.(i).(j) <- init_case(Eau);
    done;
  done;
  m;;

let ajouter_maisons m = 
  let hauteur = Array.length m in
  let largeur = Array.length m.(0) in
  for i=0 to hauteur-1 do
    for j=0 to largeur-1 do
      let random = Random.int 1000 in
      if (random <= int_of_float(1000. *. prob_maison)) then
	m.(i).(j) <- init_case(Maison);
    done;
  done;;

let homogeneite m i1 j1 case1 =
  let hauteur = Array.length m in
  let largeur = Array.length m.(0) in
  let homogene = ref 0 in
  if (i1>0) then
    begin
      if (j1>0) then
	if (m.(i1-1).(j1-1).element = case1) then
	  incr(homogene);
      if (m.(i1-1).(j1).element = case1) then
	incr(homogene);
      if (j1<largeur-1) then
	if (m.(i1-1).(j1+1).element = case1) then
	  incr(homogene);
    end;
  if (j1<largeur-1) then
    if (m.(i1).(j1+1).element = case1) then
      incr(homogene);
  if (i1<hauteur-1) then
    begin
      if (j1<largeur-1) then
	if (m.(i1+1).(j1+1).element = case1) then
	  incr(homogene);
      if (m.(i1+1).(j1).element = case1) then
	incr(homogene);
      if (j1>0) then
	if (m.(i1+1).(j1-1).element = case1) then
	  incr(homogene);
    end;
  if (i1>0) then
    if (m.(i1-1).(j1).element = case1) then
      incr(homogene);
  (!homogene);;

let etape m =
  let hauteur = Array.length m in
  let largeur = Array.length m.(0) in
  let i1 = Random.int hauteur in
  let j1 = Random.int largeur in
  let i2 = Random.int hauteur in
  let j2 = Random.int largeur in
  let case1 = m.(i1).(j1).element in
  let case2 = m.(i2).(j2).element in
  let homogene_avant = (homogeneite m i1 j1 case1) + (homogeneite m i2 j2 case2) in
  
  m.(i1).(j1) <- init_case(case2);
  m.(i2).(j2) <- init_case(case1);

  let homogene_apres = (homogeneite m i1 j1 case2) + (homogeneite m i2 j2 case1) in
  if (homogene_avant > homogene_apres) then (*si c'était mieux avant*)
    begin
      m.(i2).(j2) <- init_case(case2);
      m.(i1).(j1) <- init_case(case1);
    end;;


let terrain_intelligent m k =
  for i=1 to k do
    etape m;
  done;;

let initialiser_terrain n m =
  let t = terrain_aleatoire n m in
  ajouter_maisons t;
  terrain_intelligent t (n*m*100);

t;;

let terrain = initialiser_terrain n m;;

let allumer_terrain p =
(* Allume p cases du terrains *)
  let k = ref p in
  let compteur = ref 0 in (* vérif qu'on ne boucle pas à l'infini *)

  while (!k > 0 && !compteur < n*m*10) do
    let i = Random.int n in
    let j = Random.int m in
    let c = terrain.(i).(j) in

    if c.intensite_feu = 0 && c.element <> Eau then (c.intensite_feu <- 1; decr k);
    incr compteur;
  done;
;;

let reinitialiser_terrain () =
  for i = 0 to (n-1) do
    for j = 0 to (m-1) do
      let c = terrain.(i).(j) in
      c.intensite_feu <- 0;
      c.estompe <- false;
      c.brule <- false;
      c.calcine <- false;
      c.pompier <- 0;
    done;
  done;;

allumer_terrain quantite_feu;;

