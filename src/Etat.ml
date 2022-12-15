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

let normalisation (etat : etat) : etat = 
  let rec normaliser (etat : etat) (i : int)=
    if(i<etat.nb_colones) then 
      begin
        let col = PArray.get etat.colones i in
        match col.liste with 
        |[] -> normaliser etat (i+1)
        | x::l' -> let n = (Card.to_num x)/13 in 
          let depot = PArray.get etat.depot n in
          if((Card.to_num x mod 13)=(depot.nb_cartes_depose+1)) then  
            begin
              normaliser {nb_colones = etat.nb_colones;
              colones = PArray.set etat.colones i {liste = (List.tl col.liste);};
              nb_registres = etat.nb_registres;
              registres = etat.registres;
              nb_registres_dispo = etat.nb_registres_dispo;
              depot = PArray.set etat.depot n {nb_cartes_depose = (depot.nb_cartes_depose+1);};
              coups = etat.coups;
              regle = etat.regle;
              } 0 
            end
          else
            normaliser etat (i+1) 
      end
    else 
      etat
  in
  normaliser etat 0



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
let carte_en_tete_de_colone (etat : etat) (num_carte : int) : bool =
  let retour = ref false in
  for i = 0 to etat.nb_colones -1 do
    (*Si on trouve la carte en début de colone*)
    if(  Card.to_num ( List.hd (liste_colone etat i))) = num_carte then
      retour := true
  done;
  !retour

let carte_dans_registre (etat : etat) (num_carte : int) : bool =
  let retour = ref false in
  for i = 0 to etat.nb_registres -1 do
    (*Si on trouve la carte en début de colone*)
    if(  Card.to_num ( List.nth etat.registres i)) = num_carte then
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
  
  
  (*let envoi_vers_colone_vide (carte_a_deplacer : int) (etat :etat):unit =
    let colones = ref etat.colones in
      for i = 0 to etat.nb_colones - 1 do
        (*Si on trouve la carte en début de colone*)
        if List.length (liste_colone etat i) = 0 then
          colones := PArray.set 

      done*)


  let valider_coup (etat : etat) (carte_a_deplacer : int) (destination : string) (nb_coups : int) =
    (*Cas de l'envoi vers une colone vide*)
    if destination.[0] = 'V' then
      begin
        if((existence_colone_vide etat) && (etat.regle.colone_vide_remplissable)) then
          begin
            (*Si la carte est deplacable*)
            if ((carte_en_tete_de_colone etat carte_a_deplacer) || (carte_dans_registre etat carte_a_deplacer)) then
              begin
                (*Envoi vers premier colone vide et suppripmer carte partie*)
                etat
              end
            else etat
          end
        else begin
            (*print_string "Erreur Pas Envoi colone vide\n";*)
            etat
          end
      end
    
    (*Cas de l envoi vers un registre*)
    else if destination.[0] = 'T' then
      begin
        (*print_string "Registre 1\n";*)
        if ( (existence_place_dans_registre etat) && (etat.nb_registres > 0)) then
          begin
          (*Si la carte est deplacable*)
          if (carte_en_tete_de_colone etat carte_a_deplacer) || (carte_dans_registre etat carte_a_deplacer) then
            print_string "Envoyer carte dans un registre\n";
            etat
          end
          else etat
      end
    (*Le vrai cas où va falloir vérifier si la carte peux aller*)
    else      
      begin
        let destination_finale = int_of_string destination in
        (*Si la carte à deplacer et la destination existe*)
        if( ((carte_en_tete_de_colone etat carte_a_deplacer) || (carte_dans_registre etat carte_a_deplacer))&& (carte_en_tete_de_colone etat destination_finale)) then
          begin
            (*Si les cartes sont de meme couleur*)
            if (meme_couleur carte_a_deplacer destination_finale) then
              begin
                (*Si le jeu recois les cartes sont de meme couleur*)
                if (etat.regle.recoi_meme_couleur) then
                  (*print_string "Deplacer\n";*)
                  etat
                else
                  (*print_string "Erreur\n";*)
                  etat
              end
            (*Si les cartes sont de couleur differentes*)
            else 
              (*Si le je recois les cartes de couleurs alternee*)
              if (etat.regle.recoi_couleur_alternee) then
                (*print_string "Deplacer\n";*)
                etat
              else
                (*print_string "Erreur\n";*)
                etat 
          end
          (*Le cas ou les cartes ne sont pas deplaçables*)
        else
          (*print_string "Erreur\n";*)
          etat
      end
     



let check (conf : Config.config) :unit=
  (*print_endline "(-----------Putain arrivé ici------ le mode est\n";*)
  let etatPartie = etatAPartirDeConfiguration conf in
  let nb_coups = ref 0 in
  match conf.mode with
  | Check s -> 
    let rec lecture_ligne ci =
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
        let resultat = valider_coup etatPartie carte_a_deplacer destination (!nb_coups) in
        (*print_string ligne;
        print_string "--";*)
        1 + lecture_ligne ci
      with End_of_file -> 0
    in
    
    let canal = open_in s in
    let lu = lecture_ligne canal in
    (*print_int taille;*)
    close_in canal;
  | Search s -> print_endline "Un Search"
  
