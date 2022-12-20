(** Etat *)
open Regle
open Config
open Card




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

   (*Supprime la carte si dans registre*)
   let supprimer_carte_dans_registre (etat : etat) (num_carte : int) : Card.card list =
    List.filter (fun x -> (Card.to_num x) != num_carte) etat.registres
let rec normalisation (etat : etat) : etat = 
  let rec normaliser_colone (etat : etat) (i : int)=
    if(i<etat.nb_colones) then 
      begin
        print_string "colone   ";
        let col = PArray.get etat.colones i in
        match col.liste with 
          |[] -> normaliser_colone etat (i+1)
          |x::l' ->
          let rank,suit = x in
          let num_depot = Card.num_of_suit suit in 
          let depot = PArray.get etat.depot num_depot in
          print_string "num carte a deplacer    ";
          print_string (Card.to_string x);
          print_string "   nb cartes du registre   ";
          print_int (List.length etat.registres);
          print_string "   nb cartes du depot   ";
          print_int num_depot;
          print_string "   ";
          print_int depot.nb_cartes_depose;  
          print_string "   ";
          if((rank)=(depot.nb_cartes_depose)+1) then  
            begin
              print_string "normaliser   ";
              print_string (Card.to_string x);
              print_string "   vers   ";
              print_int num_depot;
              print_string "   nb    ";
              print_int depot.nb_cartes_depose;
              normalisation {etat with
              colones = PArray.set etat.colones i {liste = (List.tl col.liste);};
              depot = PArray.set etat.depot num_depot {nb_cartes_depose = (depot.nb_cartes_depose+1);};
              }
            end
          else
            begin 
              print_string "normaliser colone +1   ";
              normaliser_colone etat (i+1) 
            end
      end
    else 
      begin 
        print_string "retour etat   ";
        etat
      end
  in
  let rec normaliser_registres (etat : etat) (i : int) = 
    if(i<List.length etat.registres) then 
      begin 
        print_string "registre   ";  
        let x = List.nth etat.registres i in 
        let rank,suit = x in
        let num_depot = Card.num_of_suit suit in 
        let depot = PArray.get etat.depot num_depot in
        print_string "num carte a deplacer    ";
        print_string (Card.to_string x);
        print_string "   nb cartes du registre   ";
        print_int (List.length etat.registres);
        print_string "   nb cartes du depot   ";
        print_int num_depot;
        print_string "   ";
        print_int depot.nb_cartes_depose;  
        print_string "   ";
        if(rank) = depot.nb_cartes_depose+1 then
          begin 
            print_string "normaliser   ";
            print_string (Card.to_string x);
            print_string "   vers   ";
            print_int num_depot;
            print_string "   nb    ";
            print_int depot.nb_cartes_depose;
            print_string "   ";
            normalisation {etat with
            registres = supprimer_carte_dans_registre etat (Card.to_num x);
            depot = PArray.set etat.depot num_depot {nb_cartes_depose = (depot.nb_cartes_depose+1);};
            }
          end
        else 
          begin
            print_string "normaliser_registres rec +1   ";
            normaliser_registres etat (i+1)
          end 
      end
    else 
      begin 
        print_string "normaliser colone 0   ";
        normaliser_colone etat 0 
      end in 
  normaliser_registres etat 0



(* Retourne la somme des élement d'un tableau jusqu'a la position i exclus*)
let sum_to_i t i = 
  let sum = ref 0 in
  for j = 0 to i - 1 do
    sum := !sum + Array.get t j ;
  done;  
  !sum

(*Envoi les rois en fin de colone pour Baker*)
let envoi_roi_en_fond_de_colone (liste: card list) :card list=
  let liste_des_rois = List.filter (fun (rank,suit) -> rank = 13) liste in
  let liste_des_autres = List.filter (fun (rank,suit) -> rank != 13) liste in
  liste_des_autres @ (List.rev liste_des_rois)

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
   (*On verifie si les rois doivent aller en fond de colone comme dans baker*) 
  if regle.roi_en_fond_de_colone then {liste =  envoi_roi_en_fond_de_colone(aux permutation debut fin l1)}
  else {liste = aux permutation debut fin l1}

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
    roi_en_fond_de_colone = false;
  
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
    tab_cartes_colone_vide = [|(13,Trefle);(13, Pique);(13,Carreau);(13,Coeur)|];
    roi_en_fond_de_colone = false;

  
  }

  let freecell = {
    nb_colones = 8;
    nb_registres = 4;
    nb_registres_utilise = 0;
    tab_nb_cartes_colone = [|7;6;7;6;7;6;7;6|];
    recoi_couleur_alternee = true;
    recoi_meme_couleur = false;
    colone_vide_remplissable = true;
    tab_cartes_colone_vide = Array.init 52 (fun x -> Card.of_num x);
    roi_en_fond_de_colone = false;

  
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
    roi_en_fond_de_colone = true;
  
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
(*let carte_en_tete_de_colone (etat : etat) (num_carte : int) : int*bool =
  let retour = ref false in
  let coloneN = ref (-1) in
  let card_test = Card.of_num num_carte in
  print_string "Card test = ";
  print_string (Card.to_string card_test);
  print_string "   ";
  for i = 0 to etat.nb_colones -1 do
    (*Si on trouve la carte en début de colone*)
    let card_tete_colone = (List.hd (liste_colone etat i)) in
    print_string "Tete colone ";
    print_int i;
    print_string "= ";
    print_string (Card.to_string card_tete_colone);
    print_string "   ";
    if card_test = card_tete_colone then
      begin 
        retour := true;
        coloneN := i;
      end
  done;
  !coloneN,!retour*)

  let rec carte_en_tete_de_colone (etat : etat) (num_carte : int) (i : int) : int*bool =
    let card_test = Card.of_num num_carte in
    print_string "Card test = ";
    print_string (Card.to_string card_test);
    print_string "   ";
    if (i<etat.nb_colones) then
      begin 
        (*Si on trouve la carte en début de colone*)
        match (PArray.get etat.colones i).liste with 
        |[] ->  carte_en_tete_de_colone etat num_carte (i+1)
        |x::l' -> 
        print_string "Tete colone ";
        print_int i;
        print_string "= ";
        print_string (Card.to_string x);
        print_string "   ";
        if card_test = x then
          i,true
        else 
          carte_en_tete_de_colone etat num_carte (i+1)
      end
    else   
      (-1),false

  (*let carte_dans_registre (etat : etat) (num_carte : int) : bool =
    let retour = ref false in
    let card_test = Card.of_num num_carte in
    for i = 0 to ((List.length etat.registres) -1) do
      (*Si on trouve la carte en début de colone*)
      let card_registre = (List.nth etat.registres i) in
      if card_test = card_registre then
        begin 
          retour := true;
        end
    done;
    !retour*)

  let rec carte_dans_registre (etat : etat) (num_carte : int) (i : int): bool =
    let card_test = Card.of_num num_carte in
    if (i<List.length etat.registres) then 
      (*Si on trouve la carte en début de colone*)
    let card_registre = (List.nth etat.registres i) in
      if card_test = card_registre then
        true
      else 
        carte_dans_registre etat num_carte (i+1)
    else 
      false

  (*Verifie si il existe une colone vde*)
  (*let existence_colone_vide (etat : etat) :int*bool = 
    let retour = ref false in
    let colVideN = ref (-1) in
    for i = 0 to etat.nb_colones - 1 do
      (*Si on trouve la carte en début de colone*)
      if List.length (liste_colone etat i) = 0 then
        begin 
          retour := true;
          colVideN := i
        end
    done;
    !colVideN,!retour*)

    let rec existence_colone_vide (etat : etat) (i:int): int*bool = 
      if (i<etat.nb_colones) then
        begin 
          match (PArray.get etat.colones i).liste with 
          |[] -> i,true
          |x::l' -> existence_colone_vide etat (i+1)
        end 
      else 
        begin 
          (-1),false
        end 

    
    (*Verifie si la carte peut aller dans une colone vide*)
    let carte_peut_aller_dans_colone_vide (etat : etat) (num_carte : int) : bool =
    Array.exists (fun x -> x = (Card.of_num num_carte)) etat.regle.tab_cartes_colone_vide
      (*let retour = ref false in
      for i = 0 to ((Array.length etat.regle.tab_cartes_colone_vide) - 1)  do
        (*Si on trouve la carte en début de colone*)
        if(  (Array.get (etat.regle.tab_cartes_colone_vide) i) = num_carte ) then
          retour := true
      done;
      !retour*)
  
    (*Supprime la carte trouvé en tête de colone*)
    let supprimer_carte_dans_colone (etat : etat) (num_carte : int) (col : int): colone PArray.t =
      let colone_a_renvoyer = {liste = List.tl (liste_colone etat col)} in
      (*Si on a trouvé alors on modifie la colone*)
      (PArray.set (etat.colones) col colone_a_renvoyer)

   

  (*Verifie si il ya de la place dans un registre*)
  let existence_place_dans_registre (etat : etat) :bool = 
    List.length etat.registres < etat.nb_registres

  (* Verifie si les 2 cartes sont de même couleur*)
  let meme_couleur (carte_a_deplacer : int) (destination : int): bool = 
    let rank_deplacer,suit_deplacer = Card.of_num carte_a_deplacer in 
    let rank_destination,suit_destination = Card.of_num destination in
    if (suit_deplacer = Trefle || suit_deplacer = Pique) && (suit_destination = Trefle || suit_destination = Pique) then
      true 
    else if (suit_deplacer = Coeur || suit_deplacer = Carreau) && (suit_destination = Coeur || suit_destination = Carreau) then 
      true
    else false


  (* Verifie si la carte deplacer est la carte inferieur direct de destination*)
  let carte_inferieur (carte_a_deplacer : int) (destination : int): bool =
    let rank_deplacer,suit_deplacer = Card.of_num carte_a_deplacer in 
    let rank_destination,suit_destination = Card.of_num destination in
    rank_deplacer = rank_destination-1
  
  (*envoi une carte vers une colone vide*)
  let envoi_vers_colone_vide (carte_a_deplacer : int) (etat :etat) (colVideN : int): colone PArray.t =
    let colone_a_renvoyer = {liste = [Card.of_num carte_a_deplacer]} in
    (PArray.set (etat.colones) colVideN colone_a_renvoyer)

  let envoi_vers_colone_registre (carte_a_deplacer : int) (etat :etat):Card.card list =
    [Card.of_num carte_a_deplacer]@ etat.registres


  exception Mauvais_coup of string
  let envoi_vers_carte_dest (carte_a_deplacer : int) (col_carte_depla : int) (col_carte_dest :int)(etat:etat) (boolReg : bool) = 
    let cardDepla = Card.of_num carte_a_deplacer in
    let listeDest = (PArray.get etat.colones col_carte_dest).liste in
    if (boolReg) then
      begin
        {etat with
        registres = supprimer_carte_dans_registre etat carte_a_deplacer;
        colones = PArray.set etat.colones col_carte_dest {liste = ([cardDepla]@listeDest)};
        }
      end 
    else 
      begin 
        {etat with
        colones = PArray.set (supprimer_carte_dans_colone etat carte_a_deplacer col_carte_depla) col_carte_dest {liste = ([cardDepla]@listeDest)};
        }
      end


  let valider_coup (etat : etat) (carte_a_deplacer : int) (destination : string):etat=
    (*Cas de l'envoi vers une colone vide*)
    let rank_deplacer,suit_deplacer = Card.of_num carte_a_deplacer in 
    print_string "valider coup   ";
    if destination = "V" then
      begin
        print_string "On est dans V- ";
        let colVideN,colVideExist = existence_colone_vide etat 0 in
        print_string (if(colVideExist) then "colvide true   " else "colvide false   ");
        print_string (if(etat.regle.colone_vide_remplissable) then "colvideremplissable true   " else "colvideremplissable false   ");
        print_string (if(carte_peut_aller_dans_colone_vide etat carte_a_deplacer) then "carte_dans_colvide true   " else "carte_dans_colvide false   ");
        if(colVideExist && (etat.regle.colone_vide_remplissable) && (carte_peut_aller_dans_colone_vide etat carte_a_deplacer)) then
          begin
            let deplaCol,deplaBoolTeteCol = carte_en_tete_de_colone etat carte_a_deplacer 0 in
            let regBool = carte_dans_registre etat carte_a_deplacer 0 in
            (*Si la carte est deplacable*)
            begin 
              if deplaBoolTeteCol then
                begin
                  (*Envoi vers premier colone vide et supprimer carte partie*)
                  {etat with 
                    colones = envoi_vers_colone_vide carte_a_deplacer {etat with colones = supprimer_carte_dans_colone etat carte_a_deplacer deplaCol} colVideN;
                  }
                end
              else if regBool then 
                begin
                  {etat with 
                    registres = supprimer_carte_dans_registre etat carte_a_deplacer ;
                    colones = envoi_vers_colone_vide carte_a_deplacer etat colVideN;
                  }
                end
              else 
                begin
                  raise (Mauvais_coup "Mauvais coup")
                end
            end
          end
        else begin
            (*print_string "Erreur Pas Envoi colone vide\n";*)
            raise (Mauvais_coup "Mauvais coup")
          end
      end
    
    (*Cas de l envoi vers un registre*)
    else if destination = "T" then
      begin
        print_string "On est dans T ";
        if ( (existence_place_dans_registre etat) && (etat.nb_registres > 0)) then
          begin
            (*Si la carte est deplacable*)
            let deplaCol,deplaBoolTeteCol = carte_en_tete_de_colone etat carte_a_deplacer 0 in
            if deplaBoolTeteCol then
              begin
                {etat with 
                registres = envoi_vers_colone_registre carte_a_deplacer etat;
                colones = supprimer_carte_dans_colone etat carte_a_deplacer deplaCol;
                }
              end
            else raise (Mauvais_coup "Mauvais coup")
          end
        else raise (Mauvais_coup "Mauvais coup")
      end
    (*Le vrai cas où va falloir vérifier si la carte peux aller*)
    else      
      begin
        print_string "On est dans ELSE   ";
        let destination_finale = int_of_string destination in
        let deplaCol,deplaBoolTeteCol = carte_en_tete_de_colone etat carte_a_deplacer 0 in 
        let destCol,destBoolTeteCol = carte_en_tete_de_colone etat destination_finale 0 in 
        let regBool = carte_dans_registre etat carte_a_deplacer 0 in
        print_string (if deplaBoolTeteCol then "deplabool true   " else "deplabool false   ");
        print_string (if destBoolTeteCol then "destBool true   " else "destBool false   ");
        print_string (if regBool then "regBool true   " else "regBool false   ");
        (*Si la carte à deplacer et la destination existe*)
        if( (deplaBoolTeteCol || regBool)&& destBoolTeteCol) then
          begin
            print_string "Expediteur bon et destinateur bon   ";
            (*Si les cartes sont de meme couleur*)
            if (meme_couleur carte_a_deplacer destination_finale) then
              begin
                (*Si le jeu recois les cartes sont de meme couleur*)
                print_string "meme_couleur   ";
                if etat.regle.recoi_meme_couleur then
                  (*print_string "Deplacer\n";*)
                  begin 
                    print_string "regle recois meme couleur   ";
                    if carte_inferieur carte_a_deplacer destination_finale then 
                      begin 
                        print_string "carte inferieur true   ";
                        if regBool then 
                          begin 
                            print_string "Envoi carte vient d'un registre   ";
                            (envoi_vers_carte_dest carte_a_deplacer (-1) destCol etat regBool);
                          end
                        else 
                          begin
                            print_string "Envoi carte vient d'une colone   ";
                            (envoi_vers_carte_dest carte_a_deplacer deplaCol destCol etat regBool);
                          end
                      end
                    else raise (Mauvais_coup "Mauvais coup")
                  end
                else
                  (*print_string "Erreur\n";*)
                  raise (Mauvais_coup "Mauvais coup")
              end
            (*Si les cartes sont   de couleur differentes*)
            else 
              begin 
                (*Si le je recois les cartes de couleurs alternee*)
                print_string "pas meme couleur   ";
                if etat.regle.recoi_couleur_alternee then
                  begin
                    print_string "regle recoit alternee   ";
                    if carte_inferieur carte_a_deplacer destination_finale then
                      begin
                        print_string "carte inferieur true   ";
                        if regBool then 
                          begin 
                            print_string "Envoi vient d'un registre   ";
                            (envoi_vers_carte_dest carte_a_deplacer (-1) destCol etat regBool);
                          end
                        else 
                          begin
                            print_string "Envoi vient d'une colone   ";
                            (envoi_vers_carte_dest carte_a_deplacer deplaCol destCol etat regBool);
                          end
                      end
                    else raise (Mauvais_coup "Mauvais coup")
                  end
                else
                  begin 
                  (*print_string "Erreur\n";*)
                    print_string "rate   ";
                    raise (Mauvais_coup "Mauvais coup")
                  end
              end
          end
          (*Le cas ou les cartes ne sont pas deplaçables*)
        else
          begin 
            (*print_string "Erreur\n";*)
            raise (Mauvais_coup "Mauvais coup")
          end
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
  match conf.mode with
  | Check s -> 
    let rec lecture_ligne ci etat i =
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
        (*let resultat = valider_coup etatPartie carte_a_deplacer destination (!nb_coups) in*)
        (*On fait un try qui valide de coup ou pas et continue dans le cas où on le coup est valide*)
        try
          print_string ligne;
          print_string "----";
          let nouvel_etat = valider_coup etat carte_a_deplacer destination in
          (*Si le coup est bon on normalise et on va à la prochiane ligne*)
          1 + lecture_ligne ci (normalisation nouvel_etat) (i+1)
          
        with Mauvais_coup s->
          (*Si le coup est mauvais on arrete*)
          print_string "ECHEC ";
          print_int i;
          print_newline ();
          exit 1;

        (*print_string ligne;
        print_string "--";*)
        
      with End_of_file ->
        (*Quand ya plus aucune ligne a lire on verfie si on a gagné ou pas*)
        if partie_gagne etat then
          begin
            print_string "SUCCES";
            print_newline ();
            exit 0
          end
        else
          begin
            print_string "ECHEC ";
            print_int i;
            print_newline ();
            exit 1
          end
    in
    
    let canal = open_in s in
    let lu = lecture_ligne canal etatPartie 1 in
    (*print_int taille;*)
    close_in canal;
  | Search s -> print_endline "Un Search"
  
