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
        (PArray.get etat.depot i).nb_cartes_depose + get_score etat i+1
      end 
    else 
      begin 
        0
      end 

(*Fonction de comparaison de score de 2 etats*)
  let comparaison_inferiorite_etat (e1 :etat) (e2 :etat):int =
    (get_score e1 0) - (get_score e2 0)

let choix_etat_a_visiter (modul:States.t):etat = 
  let liste_des_etats = States.elements modul in
  let liste_des_etats_trie = List.sort comparaison_inferiorite_etat liste_des_etats in
  List.hd(List.rev liste_des_etats_trie)

  (*Remplir le fichier solution*)
let remplir_fichier_colone nom_fichier liste_de_coups =
  let rec remplissement canal liste_de_coups =
  match liste_de_coups with
  | [] -> 1
  | h::r ->
    output_string canal (string_of_int h);
    output_char canal '\n';
    remplissement canal r
  in
  let c = open_out nom_fichier in
  (* let remplissement c liste_de_coups
  close_out c *)
  let res = remplissement c liste_de_coups in
  close_out c

  (*Retourne la liste des cartes à deplacer c'est à dire celles en tête de colones*)
let rec cartes_en_tete_de_colone (etat:etat) (num_colone:int) (liste:int list) :(int list) =
  if num_colone < etat.nb_colones then
    begin
      let carte_en_tete = List.hd( List.rev((PArray.get etat.colones num_colone).liste) ) in
      cartes_en_tete_de_colone etat (num_colone+1) ((Card.to_num carte_en_tete)::liste)
    end
  else
    begin
      liste
    end
  
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

(*Prendre l'etat a visiter et trouver tous les coups memes ceux de registre, a chaque fois qu'on trouve,
ajouter le coups a nouvel etat, valider le coup dans l'etat ne pas oublier array.copy*)
let search (conf : Config.config) :unit=
  let etat_partie = normalisation (etatAPartirDeConfiguration conf) in
  let etat_restant_a_visiter = States.add etat_partie States.empty in
  let etat_deja_traite = States.empty in
  let etat_a_visite = choix_etat_a_visiter etat_restant_a_visiter in
  print_endline "a"
  




