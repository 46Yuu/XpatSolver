(** Etat *)
open Regle;;


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
}

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
  let colones = PArray.init 10 (remplissage_colone permutation regle) in
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
              coups = etat.coups;} 0 
            end
          else
            normaliser etat (i+1) 
      end
    else 
      etat
  in
  normaliser etat 0

