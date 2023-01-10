(** Searching *)
open Etat

(*Renvoie true  quand les registres sont égaux et flse sinon*)
let egalite_de_registre (reg1:Card.card list) (reg2:Card.card list):bool =
  let rec existence_de_toutes_cartes_dans_registre (r1:Card.card list) (r2:Card.card list) (index_carte:int) (fin_inclu:int):bool =
    if index_carte > fin_inclu then true
    else
      begin
        (*Vérfication de l'appartenance de la carte d'index index_carte du registre 2 dans le registre 1*)
        if List.mem (List.nth r2 index_carte) r1 then
          existence_de_toutes_cartes_dans_registre r1 r2 (index_carte+1) fin_inclu
        else false
      end
    in
  if((List.length reg1) != (List.length reg2)) then false
  else existence_de_toutes_cartes_dans_registre reg1 reg2 0 ((List.length reg1)-1)


let compare_state (e1:etat) (e2:etat):int =
  let egalite_colones (c1:colone PArray.t) (c2:colone PArray.t):bool =
    if ((Stdlib.compare c1 c2) = 0) then true
    else false
  in
  let egalite_registres (r1:Card.card list) (r2:Card.card list):bool =
    if ((Stdlib.compare r1 r2) = 0) then true
    else false
  in
  if egalite_de_registre e1.registres e2.registres then
    begin
      if egalite_colones e1.colones e2.colones then 0
      else 1
    end
  else 1
module States = Set.Make (struct type t = etat let compare = compare_state end)

(*Renvoi le score de l'etat*)
let rec get_score (etat : etat) (i:int):int = 
  if (i < 4) then
    begin
      (PArray.get etat.depot i).nb_cartes_depose + get_score etat (i+1)
    end 
  else 
    begin 
      0
    end 

(*Fonction de comparaison de score de 2 etats*)
let comparaison_inferiorite_etat (e1 :etat) (e2 :etat):int =
  (get_score e1 0) - (get_score e2 0)

(*Fonction pour faire la copie d'etat*)
let copy_etat (etat:etat) :etat = etat
  


(*Choix de l'etat de plus hau score*)
let choix_etat_a_visiter (modul:States.t):etat = 
  let liste_des_etats = States.elements modul in
  let liste_des_etats_trie = List.sort comparaison_inferiorite_etat liste_des_etats in
  let retour = List.hd(List.rev liste_des_etats_trie) in
  copy_etat retour
  (* {retour with colones = PArray.to_array (retour.colones)}
    Faut faire la copy ici normalement  *)

(*Remplir le fichier solution*)
let remplir_fichier_solution nom_fichier liste_de_coups =
  let rec remplissement canal liste_de_coups =
  match liste_de_coups with
  | [] -> 1
  | h::r ->
    output_string canal h;
    output_char canal '\n';
    remplissement canal r
  in
  let c = open_out nom_fichier in
  (* let remplissement c liste_de_coups
  close_out c *)
  let res = remplissement c liste_de_coups in
  close_out c

(*Retourne la liste des cartes à deplacer c'est à dire celles en tête de colones*)
let cartes_en_tete_de_colone (etat:etat) :(int list) =
  let rec cartes_en_tete_de_colone_rec (etat:etat) (num_colone:int) (liste:int list) :(int list) =
  if num_colone < etat.nb_colones then
    begin
      print_string "Nombre d'element dans la liste ";
      (* print_int List.length( (PArray.get etat.colones num_colone).liste); *)
      print_string " * ";
      let carte_en_tete = List.hd( (PArray.get etat.colones num_colone).liste)  in
      cartes_en_tete_de_colone_rec etat (num_colone+1) ((Card.to_num carte_en_tete)::liste)
    end
  else
    begin
      liste
    end
  in
  cartes_en_tete_de_colone_rec etat 0 []
  
  (*Retourne la liste des String des destinations qui sont des coups valides par rapport
  a la carte a deplacer*)
let rec tout_coups_possibles (carte_depart : int) (etat : etat) (i : int) =
  if (i < etat.nb_colones) then 
    begin 
      let rank,suit = (List.hd((PArray.get etat.colones i).liste)) in
      if Etat.carte_inferieur carte_depart (Card.to_num (rank,suit)) then  
        begin 
          [string_of_int(Card.to_num (rank,suit))]@tout_coups_possibles carte_depart etat (i+1)
        end 
      else 
        begin 
          tout_coups_possibles carte_depart etat (i+1)
        end
    end
  else 
    begin
      if Etat.existence_place_dans_registre etat then
        begin 
          ["T"]
        end 
      else 
        begin
          []
        end
    end

(*Retourne la liste des etats atteignables apres tout les deplacements de toutes les cartes*)
let deplacement_des_cartes (cartes:int list) (etat_a_modifier:etat) = 

  (*Retourne la liste des etats atteignables avec une carte*)
  let rec tout_deplacements_de_une_carte num_carte les_deplacements etat liste_des_etats_atteints =
    match les_deplacements with
    | [] -> liste_des_etats_atteints
    | x::xs -> tout_deplacements_de_une_carte num_carte xs etat ((valider_coup (copy_etat etat) num_carte x)::liste_des_etats_atteints)
  in
  (*Retourne la liste des etats atteignables apres tout les deplacements de toutes les cartes*)
  let rec tout_deplacements_de_toute_cartes cartes etat (liste_des_etats_atteints: etat list) =
    match cartes with
    | [] -> liste_des_etats_atteints
    | x::l ->
      let coups_possibles = tout_coups_possibles x etat 0 in
      tout_deplacements_de_toute_cartes l etat ((tout_deplacements_de_une_carte x coups_possibles etat [])
      @liste_des_etats_atteints)
    in
  tout_deplacements_de_toute_cartes cartes etat_a_modifier []

(*Retourne le module où on a ajouté tous les états atteinds*)
let rec ajout_des_etats_atteints (etat_restant_a_visiter:States.t) liste_des_etats_atteins =
  match liste_des_etats_atteins with
  | [] -> etat_restant_a_visiter
  | x::l -> ajout_des_etats_atteints (States.add x etat_restant_a_visiter) l


exception Aucune_Solution_Trouve
(*Prends les etats déja traite et les restant puis lance le parcours de solution *)
let rec parcours_des_solutions (etat_restant_a_visiter:States.t) (etat_deja_traite:States.t) =
  (* Si il ne reste plus rien à visiter*)
  print_string "Parcours * ";

  if States.is_empty etat_restant_a_visiter then
    begin
      print_string "Pas de sol * ";
      raise (Aucune_Solution_Trouve)
    end
  else (*Quand on a des trucs à visiter*)
    begin
      print_string "Il ya sol * ";
      let etat_a_visite = choix_etat_a_visiter etat_restant_a_visiter in
      print_string "On a etat a visiter * ";
      (*Quand on a trouver l'etat solution*)
      if (get_score etat_a_visite 0 = 52) then
        begin
          print_string "Bon score * ";
          etat_a_visite
        end
      else (*Sinon on continue la recherche*)
        begin
          print_string "Mauvais score * ";
          let maj_etat_restant_a_visiter = States.remove etat_a_visite etat_restant_a_visiter in
          let maj_etat_deja_traite = States.add etat_a_visite etat_deja_traite in
          let cartes_a_deplacer = cartes_en_tete_de_colone (copy_etat etat_a_visite) in
          print_string "On a les cartes à deplacer * ";
          let liste_des_etats_atteins = deplacement_des_cartes cartes_a_deplacer etat_a_visite in
          let new_maj_etat_restant_a_visiter = ajout_des_etats_atteints maj_etat_restant_a_visiter liste_des_etats_atteins in
          parcours_des_solutions new_maj_etat_restant_a_visiter maj_etat_deja_traite
        end
    end
  



(*Prendre l'etat a visiter et trouver tous les coups memes ceux de registre, a chaque fois qu'on trouve,
ajouter le coups a nouvel etat, valider le coup dans l'etat ne pas oublier array.copy*)
let search (conf : Config.config) (fichier_solution : string) :unit=
  let etat_partie = normalisation (etatAPartirDeConfiguration conf) in
  let etat_restant_a_visiter = States.add etat_partie States.empty in
  let etat_deja_traite = States.empty in
  try
    print_string "Juste avant parcours * ";
    let etat_final = parcours_des_solutions etat_restant_a_visiter etat_deja_traite in
    let un = remplir_fichier_solution fichier_solution (etat_final.coups) in
    print_endline "SUCCES";
    exit 0
  with Aucune_Solution_Trouve ->
    print_endline "INSOLUBLE";
    exit 2
  


  




