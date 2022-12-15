(** Etat *)
open Regle
open Config




(*Une colone est un pile de cartes*)
type colone = {
  liste : Card.card list;

}

type coup = {
  vide : string;

}

type depot = {
  nb_cartes_depose : int;

}
type etat = {
  nb_colones : int;
  colones : colone PArray.t;
  nb_registres : int;
  registres : Card.card list;
  nb_registres_dispo : int;
  depot : depot PArray.t;
  coups : coup list;
  regle : regle
}

let rec normalisation (etat : etat) : etat = 
  let rec normaliser_colone (etat : etat) (i : int)=
    if(i<etat.nb_colones) then 
      begin
        let col = PArray.get etat.colones i in
        match col.liste with 
        |[] -> normaliser_colone etat (i+1)
        | x::l' -> let n = (Card.to_num x)/13 in 
          let depot = PArray.get etat.depot n in
          if((Card.to_num x mod 13)=(depot.nb_cartes_depose+1)) then  
            begin
              normalisation {nb_colones = etat.nb_colones;
              colones = PArray.set etat.colones i {liste = (List.tl col.liste);};
              nb_registres = etat.nb_registres;
              registres = etat.registres;
              nb_registres_dispo = etat.nb_registres_dispo;
              depot = PArray.set etat.depot n {nb_cartes_depose = (depot.nb_cartes_depose+1);};
              coups = etat.coups;
              regle = etat.regle;
              }
            end
          else
            normaliser_colone etat (i+1) 
      end
    else 
      etat
  in
  let rec normaliser_registres (etat : etat) (i : int) = 
    if(i<etat.nb_registres) then 
      begin 
        match etat.registres with 
        | [] -> normaliser_colone etat 0
        | x::l' -> let n = (Card.to_num x)/13 in 
          let depot = PArray.get etat.depot n in 
          if(Card.to_num x mod 13) = (depot.nb_cartes_depose+1) then
            begin 
              normalisation {nb_colones = etat.nb_colones;
              colones = etat.colones;
              nb_registres = etat.nb_registres;
              registres = l';
              nb_registres_dispo = (etat.nb_registres_dispo+1);
              depot = PArray.set etat.depot n {nb_cartes_depose = (depot.nb_cartes_depose+1);};
              coups = etat.coups;
              regle = etat.regle;}
            end
          else 
            normaliser_registres etat (i+1)
      end
    else 
      normaliser_colone etat 0 in 
  normaliser_registres etat 0



(* Retourne la somme des élement d'un tableau jusqu'a la position i exclus*)
let sum_to_i t i = 
  let sum = ref 0 in
  for j = 0 to i - 1 do
    sum := !sum + Array.get t j ;
  done;  
  !sum

(* Rempli une colone *)
let remplissage_colone (permutation : int list) (regle : Regle.regle) (num_colone: int):colone = 
  let l1 = [] in
  (* la variable debut est la où on doit ommencer à prendre les elements de permutation pour chaque colone*)
  let debut = sum_to_i regle.tab_nb_cartes_colone num_colone in
  let fin = debut + (Array.get regle.tab_nb_cartes_colone num_colone) - 1 in
  let rec aux (permutation : int list) (debut :int) (fin :int) (l : Card.card list):Card.card list =
    if debut > fin then l
      (*On rempli la liste de sorte que le dernier élement rentré soit en position 0*)
    else aux permutation (debut+1) fin  ([Card.of_num((List.nth permutation debut))] @l ) in

  {liste = aux permutation debut fin l1}

(*Crée une partie à partir d'une règle*)
let creer_partie (permutation : int list) (regle : Regle.regle): etat =

  let registres = ref [] in
  let depots = PArray.make 4 ({nb_cartes_depose = 0}) in
  let coups = [] in
  let colones = PArray.init (regle.nb_colones) (remplissage_colone permutation regle) in
  let taille_permutation = List.length permutation in
  let nb_registres_utilise_debut = regle.nb_registres_utilise in
  (*Dans le cas où il y'a des cartes dans le registre de debut on rempli les registres avec le reste des cartes*)
  if nb_registres_utilise_debut > 0 then
    for i = (taille_permutation - nb_registres_utilise_debut) to (taille_permutation - 1) do
      registres := [Card.of_num((List.nth permutation i))] @ !registres;
    done;  
  {
    nb_colones = regle.nb_colones;
    colones = colones;
    nb_registres = regle.nb_registres;
    registres = !registres;
    nb_registres_dispo = regle.nb_registres - regle.nb_registres_utilise;
    depot = depots;
    coups = coups;
    regle = regle;
    
  }

  let midnight_oil = {
    nb_colones = 18;
    nb_registres = 0;
    nb_registres_utilise = 0;
    tab_nb_cartes_colone = [|3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;1|];
    recoi_couleur_alternee = false;
    recoi_meme_couleur = true;
    colone_vide_remplissable = false;
    tab_cartes_colone_vide = [||];
  
  }
  let seahaven = {
    nb_colones = 10;
    nb_registres = 4;
    nb_registres_utilise = 2;
    tab_nb_cartes_colone = [|5;5;5;5;5;5;5;5;5;5|];
    recoi_couleur_alternee = false;
    recoi_meme_couleur = true;
    colone_vide_remplissable = true;
    (*les colone vide de seahaven ne prennent que des rois*)
    tab_cartes_colone_vide = [|12;25;38;51|];
  
  }

  let freecell = {
    nb_colones = 8;
    nb_registres = 4;
    nb_registres_utilise = 0;
    tab_nb_cartes_colone = [|7;6;7;6;7;6;7;6|];
    recoi_couleur_alternee = true;
    recoi_meme_couleur = false;
    colone_vide_remplissable = true;
    tab_cartes_colone_vide = Array.init 52 (fun x -> x);

  
  }

  let baker = {
    nb_colones = 13;
    nb_registres = 0;
    nb_registres_utilise = 0;
    tab_nb_cartes_colone = [|4;4;4;4;4;4;4;4;4;4;4;4;4|];
    recoi_couleur_alternee = true;
    recoi_meme_couleur = true;
    colone_vide_remplissable = false;
    tab_cartes_colone_vide = [||];
  
  }

  (*Cree un etat à partir d'une configuration*)
let etatAPartirDeConfiguration (conf : Config.config) :etat = 
  match conf.game with
  |Freecell -> creer_partie (XpatRandom.shuffle conf.seed) freecell
  |Seahaven -> creer_partie (XpatRandom.shuffle conf.seed) seahaven
  |Midnight -> creer_partie (XpatRandom.shuffle conf.seed) midnight_oil
  |Baker -> creer_partie (XpatRandom.shuffle conf.seed) baker


(*Retourne la liste de carte d'une colone*)
let liste_colone (etat : etat) (num_colone : int) : Card.card list = 
  (PArray.get etat.colones num_colone).liste
    
(* Verifie si une carte se trouve en tête de colone*)
let carte_en_tete_de_colone (etat : etat) (num_carte : int) : int*bool =
  let retour = ref false in
  let coloneN = ref (-1) in
  for i = 0 to etat.nb_colones -1 do
    (*Si on trouve la carte en début de colone*)
    if(  Card.to_num ( List.hd (liste_colone etat i))) = num_carte then
      begin 
        retour := true;
        coloneN := i;
      end
  done;
  !coloneN,!retour

  let carte_dans_registre (etat : etat) (num_carte : int) : bool =
    let retour = ref false in
    for i = 0 to ((List.length etat.registres) -1) do
      (*Si on trouve la carte en début de colone*)
      if(  (Card.to_num ( List.nth etat.registres i)) = num_carte )then
        retour := true
    done;
    !retour

  (*Verifie si il existe une colone vde*)
  let existence_colone_vide (etat : etat) :bool = 
    let retour = ref false in
    for i = 0 to etat.nb_colones - 1 do
      (*Si on trouve la carte en début de colone*)
      if List.length (liste_colone etat i) = 0 then
        retour := true
    done;
    !retour

    let carte_peut_aller_dans_colone_vide (etat : etat) (num_carte : int) : bool =
      let retour = ref false in
      for i = 0 to ((Array.length etat.regle.tab_cartes_colone_vide) - 1)  do
        (*Si on trouve la carte en début de colone*)
        if(  (Array.get (etat.regle.tab_cartes_colone_vide) i) = num_carte ) then
          retour := true
      done;
      !retour
  
    (*Supprime la carte trouvé en tête de colone*)
    let suppripmer_carte_dans_colone (etat : etat) (num_carte : int): colone PArray.t =
      let colone_a_renvoyer = ref {liste = []} in
      let trouve = ref false in
      let position_a_modifier = ref 0 in
      for i = 0 to etat.nb_colones -1 do
        (*Si on trouve la carte en début de colone*)
        if(  Card.to_num ( List.hd (liste_colone etat i))) = num_carte then
          trouve := true;
          position_a_modifier := i;
          colone_a_renvoyer := {liste = List.tl (liste_colone etat i)}
          
      done;
      (*Si on a trouvé alors on modifie la colone*)
      if !trouve then (PArray.set (etat.colones) !position_a_modifier !colone_a_renvoyer)
      else etat.colones

      (*Supprime la carte si dans registre*)
    let suppripmer_carte_dans_registre (etat : etat) (num_carte : int): Card.card list =
      List.filter (fun x -> (Card.to_num x) != num_carte) etat.registres

  (*Verifie si il ya de la place dans un registre*)
  let existence_place_dans_registre (etat : etat) :bool = 
    List.length etat.registres < etat.nb_registres

  (* Verifie si les 2 cartes sont de même couleur*)
  let meme_couleur (carte_a_deplacer : int) (destination : int): bool = 
    if (carte_a_deplacer/13 = 0 || carte_a_deplacer/13 = 1) && (destination/13 = 0 || destination/13 = 1) then
      true 
    else if (carte_a_deplacer/13 = 2 || carte_a_deplacer/13 = 3) && (destination/13 = 2 || destination/13 = 3) then 
      true
    else false


  (* Verifie si la carte deplacer est la carte inferieur direct de destination*)
  let carte_inferieur (carte_a_deplacer : int) (destination : int): bool =
    (carte_a_deplacer mod 13) = ((destination mod 13) - 1)
  
  (*envoi une carte vers une colone vide*)
  let envoi_vers_colone_vide (carte_a_deplacer : int) (etat :etat): colone PArray.t =
    let colone_a_renvoyer = ref {liste = []} in
    let position_a_modifier = ref 0 in
    let colones = ref etat.colones in
    for i = 0 to etat.nb_colones - 1 do
      (*Si on trouve la premiere colone vide*)
      if List.length (liste_colone etat i) = 0 then
        position_a_modifier := i;
        colone_a_renvoyer := {liste = [Card.of_num carte_a_deplacer]}
      
    done;
    (PArray.set (etat.colones) !position_a_modifier !colone_a_renvoyer)

  exception Mauvais_coup of string
  let envoi_vers_carte_dest (carte_a_deplacer : int) (col_carte_depla : int) (destination : int) (col_carte_dest :int)(etat:etat) = 
    let listeDepla= PArray.get etat.colones col_carte_depla in
    let cardDepla = Card.of_num carte_a_deplacer in
    let listeDest = PArray.get etat.colones col_carte_dest in
    {nb_colones = etat.nb_colones;
    colones = PArray.set (PArray.set etat.colones col_carte_depla {liste = List.tl listeDepla.liste}) col_carte_dest {liste = ([cardDepla]@listeDepla.liste)};
    nb_registres = etat.nb_registres;
    registres = etat.registres;
    nb_registres_dispo = etat.nb_registres_dispo;
    depot = etat.depot;
    coups = etat.coups;
    regle = etat.regle;
    }

  let valider_coup (etat : etat) (carte_a_deplacer : int) (destination : string):etat=
    (*Cas de l'envoi vers une colone vide*)
    if destination.[0] = 'V' then
      begin
        if((existence_colone_vide etat) && (etat.regle.colone_vide_remplissable) && (carte_peut_aller_dans_colone_vide etat carte_a_deplacer)) then
          begin
            let deplaCol,deplaBoolTeteCol = carte_en_tete_de_colone etat carte_a_deplacer in
            (*Si la carte est deplacable*)
            if (deplaBoolTeteCol || (carte_dans_registre etat carte_a_deplacer)) then
              begin
                (*Envoi vers premier colone vide et suppripmer carte partie*)
               {etat with 
                  registres = suppripmer_carte_dans_registre etat carte_a_deplacer;
                  colones = envoi_vers_colone_vide carte_a_deplacer {etat with colones = suppripmer_carte_dans_colone etat carte_a_deplacer}
                }
              end
            else 
              begin
                raise (Mauvais_coup "Mauvais coup")
              end
          end
        else begin
            (*print_string "Erreur Pas Envoi colone vide\n";*)
            raise (Mauvais_coup "Mauvais coup")
          end
      end
    
    (*Cas de l envoi vers un registre*)
    else if destination.[0] = 'T' then
      begin
        (*print_string "Registre 1\n";*)
        if ( (existence_place_dans_registre etat) && (etat.nb_registres > 0)) then
          begin
          (*Si la carte est deplacable*)
          let deplaCol,deplaBoolTeteCol = carte_en_tete_de_colone etat carte_a_deplacer in
          if deplaBoolTeteCol || (carte_dans_registre etat carte_a_deplacer) then
            print_string "Envoyer carte dans un registre\n";
            etat
          end
          else raise (Mauvais_coup "Mauvais coup")
      end
    (*Le vrai cas où va falloir vérifier si la carte peux aller*)
    else      
      begin
        let destination_finale = int_of_string destination in
        let deplaCol,deplaBoolTeteCol = carte_en_tete_de_colone etat carte_a_deplacer in 
        let destCol,destBoolTeteCol = carte_en_tete_de_colone etat destination_finale in 
        (*Si la carte à deplacer et la destination existe*)
        if( (deplaBoolTeteCol || (carte_dans_registre etat carte_a_deplacer))&& destBoolTeteCol) then
          begin
            (*Si les cartes sont de meme couleur*)
            if (meme_couleur carte_a_deplacer destination_finale) then
              begin
                (*Si le jeu recois les cartes sont de meme couleur*)
                if (etat.regle.recoi_meme_couleur) then
                  (*print_string "Deplacer\n";*)
                  (envoi_vers_carte_dest carte_a_deplacer deplaCol destination_finale destCol etat)
                else
                  (*print_string "Erreur\n";*)
                  raise (Mauvais_coup "Mauvais coup")
              end
            (*Si les cartes sont   de couleur differentes*)
            else 
              (*Si le je recois les cartes de couleurs alternee*)
              if (etat.regle.recoi_couleur_alternee) then
                (*print_string "Deplacer\n";*)
                etat
              else
                (*print_string "Erreur\n";*)
                raise (Mauvais_coup "Mauvais coup")
          end
          (*Le cas ou les cartes ne sont pas deplaçables*)
        else
          (*print_string "Erreur\n";*)
          raise (Mauvais_coup "Mauvais coup")
      end
     

(*Verifie si tous les depot sont à 13*)
let partie_gagne (etat : etat):bool =
  let retour = ref true in
  for i = 0 to 3 do
    if ((PArray.get etat.depot i).nb_cartes_depose < 13) then
      retour := false
  done;
  !retour


let check (conf : Config.config) :unit=
  (*print_endline "(-----------Putain arrivé ici------ le mode est\n";*)
  (*let etatPartie = ref (Some (normalisation (etatAPartirDeConfiguration conf))) in*)
  let etatPartie = normalisation (etatAPartirDeConfiguration conf) in
  let nb_coups = ref 1 in
  match conf.mode with
  | Check s -> 
    let rec lecture_ligne ci etat =
      try
        (*On lit une ligne*)
        let ligne = input_line ci in
        (*On spilt la ligne avec espace ' '*)
        let ligne_split = String.split_on_char ' ' ligne in
        let carte_a_deplacer = ( int_of_string(List.nth ligne_split 0)) in
        let destination = (List.nth ligne_split 1) in
        (*for i = 0 to 1 do
          print_string (List.nth ligne_split i);
          print_string "--";
        done;*)
        nb_coups := !nb_coups +1;
        (*let resultat = valider_coup etatPartie carte_a_deplacer destination (!nb_coups) in*)
        try
          let nouvel_etat = valider_coup etatPartie carte_a_deplacer destination in
          1 + lecture_ligne ci nouvel_etat
        with Mauvais_coup s->
          print_string "ECHEC ";
          print_int !nb_coups;
          print_newline ();
          exit 1;

        (*print_string ligne;
        print_string "--";*)
        
      with End_of_file ->
        if partie_gagne etat then
          begin
            print_string "SUCCES";
            print_newline ();
            exit 0
          end
        else
          begin
            print_string "ECHEC ";
            print_int !nb_coups;
            print_newline ();
            exit 1
          end

    in
    
    let canal = open_in s in
    let lu = lecture_ligne canal etatPartie in
    (*print_int taille;*)
    close_in canal;
  | Search s -> print_endline "Un Search"
  
