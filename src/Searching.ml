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
      else Stdlib.compare e1.colones e2.colones
    end
  else Stdlib.compare e1.registres e2.registres
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


let afficher_etat e =
  print_string "e--";
  print_string "nb_colone est ";
  print_int (PArray.length e.colones);
  print_string " nb_registre est ";
  print_int (List.length e.registres);
  print_string "--e"



(*Fonction pour faire la copie d'etat*)
let copy_etat (etat:etat) :etat = 
  (* afficher_etat etat; *)
  let rec etatCopy etat iPArray =
    if (iPArray<etat.nb_colones) then
      begin 
        etatCopy ({etat with colones = PArray.set etat.colones iPArray (PArray.get etat.colones iPArray)}) (iPArray+1)
      end
    else
      begin
        (*Pour copier le depot*)
        let rec auxDepot etat i =
          if (i < 4) then 
            begin  
              auxDepot ({etat with depot = PArray.set etat.depot i (PArray.get etat.depot i)}) (i+1) 
            end 
          else 
            begin 
              etat 
            end
          in
        auxDepot etat 0
      end in 
  etatCopy etat 0


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
let cartes_en_tete_de_colone (etat:etat) :((int*int) list) =
  let rec cartes_en_tete_de_colone_rec (etat:etat) (num_colone:int) (liste:(int*int) list) :((int*int) list) =
  if num_colone < etat.nb_colones then
    begin
      (* print_string "Nombre d'element dans la liste ";
      print_int (List.length( (PArray.get etat.colones num_colone).liste));
      print_string " * "; *)
      if (List.length( (PArray.get etat.colones num_colone).liste) = 0) then
        begin
          cartes_en_tete_de_colone_rec etat (num_colone+1) (liste)
        end
      else
        begin
          let carte_en_tete = List.hd( (PArray.get etat.colones num_colone).liste)  in
            cartes_en_tete_de_colone_rec etat (num_colone+1) ( ((Card.to_num carte_en_tete),num_colone)::liste)
        end
    end
  else
    begin
      liste
    end
  in
  cartes_en_tete_de_colone_rec etat 0 []
  
  (*Retourne la liste des String des destinations qui sont des coups valides par rapport
  a la carte a deplacer*)
let rec tout_coups_possibles (carte_depart : int) (num_colone : int) (etat : etat) (i : int) =
  if (i < etat.nb_colones) then 
    begin 
      if (List.length((PArray.get etat.colones i).liste) = 0) then
        begin
          tout_coups_possibles carte_depart num_colone etat (i+1)
        end
      else
        begin
          let rank,suit = (List.hd((PArray.get etat.colones i).liste)) in
          if Etat.carte_inferieur carte_depart (Card.to_num (rank,suit)) then  
            begin 
              [string_of_int(Card.to_num (rank,suit))]@tout_coups_possibles carte_depart num_colone etat (i+1)
            end 
          else 
            begin 
              tout_coups_possibles carte_depart num_colone etat (i+1)
            end
        end
      
    end
  else 
    begin
      let coup_vers_colone_vide etat = 
        let (i,bool_colone_vide) = Etat.existence_colone_vide etat 0 in
        if (bool_colone_vide && Etat.carte_peut_aller_dans_colone_vide etat carte_depart && List.length((PArray.get etat.colones num_colone).liste) <=1) then
          begin 
            ["V"]
          end 
        else 
          begin 
            []
          end in
      let coup_vers_registre etat =
        if Etat.existence_place_dans_registre etat then
          begin 
            ["T"]@coup_vers_colone_vide etat
          end 
        else 
          begin
            coup_vers_colone_vide etat
          end in 
      coup_vers_registre etat 
    end
let rec afficher_string_list l i =
  if (i = 0) && ((List.length l) = 0 )then
    print_string "[]"
  else
    begin
      match l with
      |[] -> print_string "]"
      |x::xs ->
        if(i = 0) then
          begin
            print_string "["
          end;
          print_string x;
          print_string "-";
          afficher_string_list xs (i+1)
    end



(*Retourne la liste des etats atteignables apres tout les deplacements de toutes les cartes*)
let deplacement_des_cartes (cartes:(int*int) list) (etat_a_modifier:etat) (etat_deja_traite:States.t)= 
  (*Retourne la liste des etats atteignables avec une carte et ses deplacements*)
  let rec tout_deplacements_de_une_carte num_carte les_deplacements etat liste_des_etats_atteints =
    match les_deplacements with
    | [] -> liste_des_etats_atteints
    | x::xs ->
      if(x = "V") then
        begin
          (* print_string "Vers le vide" *)
        end;
      let coup = (string_of_int num_carte)^"-"^x in
      
      (* print_string coup;
      print_string "* "; *)
      try
        (*Si l'etat a deja été traité*)
        let etat_apres_validation_coup = (normalisation (valider_coup (copy_etat {etat with coups = etat.coups@[coup]}) num_carte x)) in
        (* let rankdebut,suitdebut = Card.of_num num_carte in
        let rankfin,suitfin = Card.of_num (int_of_string x) in *)
        (* print_string " *";
        print_string (Card.to_string (rankdebut,suitdebut));
        print_string " vers ";
        print_string (Card.to_string (rankfin,suitfin));
        print_string "* "; *)
        if(States.mem etat_apres_validation_coup etat_deja_traite) then
          begin
            (* print_string "Appartient * "; *)
            tout_deplacements_de_une_carte num_carte xs etat liste_des_etats_atteints
          end
        else (*Sinon*)
          begin
            (* print_string "Appartient pas *"; *)
            tout_deplacements_de_une_carte num_carte xs etat
              (etat_apres_validation_coup::liste_des_etats_atteints)
          end
      with Mauvais_coup s ->
        (*Quand on tombe sur un mauvais coup, on l'ignore et on passe à la suite*)
        tout_deplacements_de_une_carte num_carte xs etat liste_des_etats_atteints
  in
  (*Retourne la liste des etats atteignables apres tout les deplacements de toutes les cartes*)
  let rec tout_deplacements_de_toute_cartes cartes etat (liste_des_etats_atteints: etat list) =
    match cartes with
    | [] -> liste_des_etats_atteints
    | (x,num_colone)::l ->
      let coups_possibles = tout_coups_possibles x num_colone etat 0 in
      (* print_string "C";
      print_int x;
      print_string "*";
      afficher_string_list coups_possibles 0; *)
      tout_deplacements_de_toute_cartes l etat ((tout_deplacements_de_une_carte x coups_possibles etat [])
      @liste_des_etats_atteints)
    in
  tout_deplacements_de_toute_cartes cartes etat_a_modifier []

(*Retourne le module où on a ajouté tous les états atteind *)
let rec ajout_des_etats_atteints (etat_restant_a_visiter:States.t) (etat_deja_traite:States.t) liste_des_etats_atteins =
  match liste_des_etats_atteins with
  | [] -> etat_restant_a_visiter
  | x::l -> 
    if(States.mem x etat_deja_traite) then
      begin
        ajout_des_etats_atteints etat_restant_a_visiter etat_deja_traite l
      end
    else (*Sinon*)
      begin
        ajout_des_etats_atteints (States.add x etat_restant_a_visiter) etat_deja_traite l
      end


(*Supprime les états dont le score est de -20 par rapport au plus haut score*)
let suppression_etat_inutile (etat_restant_a_visiter:States.t) =
  let plus_haut_score = get_score (choix_etat_a_visiter etat_restant_a_visiter) 0 in
  States.filter (fun s -> (get_score s 0) >= (plus_haut_score - 19) ) etat_restant_a_visiter
  
let afficher_set s =
  print_int (List.length (States.elements s))


exception Aucune_Solution_Trouve
(*Prends les etats déja traite et les restant puis lance le parcours de solution *)
let rec parcours_des_solutions (etat_restant_a_visiter:States.t) (etat_deja_traite:States.t)  =
  (* Si il ne reste plus rien à visiter*)
      let new_etat_restant_a_visiter = suppression_etat_inutile etat_restant_a_visiter in
      if States.is_empty new_etat_restant_a_visiter then
        begin
          (* print_string "Pas de sol * "; *)
          raise (Aucune_Solution_Trouve)
        end
      else (*Quand on a des trucs à visiter*)
        begin
          (* print_string "Il ya des etats a visiter * "; *)
          let etat_a_visite = copy_etat( choix_etat_a_visiter new_etat_restant_a_visiter) in
          (* print_string "!!!!!! mon score est ";
          print_int (get_score etat_a_visite 0); *)
          (*Quand on a trouver l'etat solution*)
          if (get_score etat_a_visite 0 = 52) then
            begin
              (* print_string "Bon score * "; *)
              etat_a_visite
            end
          else (*Sinon on continue la recherche*)
            begin
              (* print_string "Mauvais score * "; *)
              let maj_etat_restant_a_visiter = States.remove (copy_etat etat_a_visite) new_etat_restant_a_visiter in
              let maj_etat_deja_traite = States.add (copy_etat etat_a_visite) etat_deja_traite in
              (* print_string "Taille a rester = ";
              print_int (List.length(Searching.States.elements ll_set)); *)
              let cartes_a_deplacer = cartes_en_tete_de_colone (copy_etat etat_a_visite) in
              (* print_string "On a les cartes à deplacer * "; *)
              let liste_des_etats_atteins = deplacement_des_cartes cartes_a_deplacer etat_a_visite maj_etat_deja_traite in
              
              let new_maj_etat_restant_a_visiter = ajout_des_etats_atteints maj_etat_restant_a_visiter maj_etat_deja_traite liste_des_etats_atteins in
              (* print_string " =============taille est ";
              print_int (List.length liste_des_etats_atteins);
              print_string " restant ";
              afficher_set new_maj_etat_restant_a_visiter;
              print_string " deja ";
              afficher_set maj_etat_deja_traite;
              print_string " *"; *)
              (* if(States.disjoint new_maj_etat_restant_a_visiter maj_etat_deja_traite) then
                print_string " Disjoints "
              else print_string " Conjoints"; *)
              parcours_des_solutions new_maj_etat_restant_a_visiter maj_etat_deja_traite
            end
      end
  
  



(*Prendre l'etat a visiter et trouver tous les coups memes ceux de registre, a chaque fois qu'on trouve,
ajouter le coups a nouvel etat, valider le coup dans l'etat ne pas oublier array.copy*)
let search (conf : Config.config) (fichier_solution : string) :unit=
  
  (* print_string "etat partie * "; *)
  let etat_partie = normalisation (etatAPartirDeConfiguration conf) in
  (* print_string "etatrestant * "; *)
  let etat_restant_a_visiter = States.add etat_partie States.empty in
  (* print_string "etat deja traite * "; *)
  let etat_deja_traite = States.empty in
  try
    (* print_string "Juste avant parcours * "; *)
    let etat_final = parcours_des_solutions etat_restant_a_visiter etat_deja_traite in
    let un = remplir_fichier_solution fichier_solution (etat_final.coups) in
    print_endline "SUCCES";
    exit 0
  with Aucune_Solution_Trouve ->
    print_endline "INSOLUBLE";
    exit 2
  


  




