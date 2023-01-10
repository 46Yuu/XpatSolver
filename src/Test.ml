(** Etat *)
open Regle;;


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

let b =  Etat.creer_partie (XpatRandom.shuffle 123) seahaven;;
let c =  Etat.creer_partie (XpatRandom.shuffle 123) seahaven;;
let d =  Etat.creer_partie (XpatRandom.shuffle 12345) seahaven;;

print_endline "Debut Test";;
print_int (Searching.compare_state b d);;
print_newline ();;

let le_set = Searching.States.empty;;
let l_set = Searching.States.add b le_set;;
print_string "Taille debut = ";;
print_int (List.length(Searching.States.elements l_set));;
print_newline ();;
let ll_set = Searching.States.add b l_set;;
print_string "Taille fin a près le re-add de b = ";;
print_int (List.length(Searching.States.elements ll_set));;
print_newline ();;

let ll_set = Searching.States.add d ll_set;;
print_string "Taille fin a près le add de d = ";;
print_int (List.length(Searching.States.elements ll_set));;
print_newline ();;

let ll_set = Searching.States.add c ll_set;;
print_string "Taille fin a près le add de c = ";;
print_int (List.length(Searching.States.elements ll_set));;
print_newline ();;

let ll_set = Searching.States.remove c ll_set;;
print_string "Taille fin a près le remove de c = ";;
print_int (List.length(Searching.States.elements ll_set));;
print_newline ();;
print_string "**************************************************************";;
print_int (Searching.compare_state b (Searching.copy_etat b));
print_string "**************************************************************";;
print_int (Searching.compare_state b (Searching.copy_etat d));
print_endline "**************************************************************";;
let ll_set = Searching.States.add c ll_set;;
let ll_set = Searching.States.add b ll_set;;
if (Searching.States.mem c ll_set) then print_endline "c dans ll" else print_endline "c pas dans ll";;
if (Searching.States.mem b ll_set) then print_endline "b dans ll" else print_endline "b pas dans ll";;
let ll_set = Searching.States.remove b ll_set;;
if (Searching.States.mem b ll_set) then print_endline "b dans ll" else print_endline "b pas dans ll";;

let liste = Searching.cartes_en_tete_de_colone b;;
let carte = fst(List.hd liste);;
let rank,suit = Card.of_num carte;;
(* print_int carte;; *)

print_string (Etat.print_shuffle 4);;




(* if (Search.egalite_colones b.colones d.colones )then print_endline "vrai"
else print_endline "Faux" *)
(* print_int (Card.to_num (List.nth ((PArray.get b.colones 7).liste) 0));;
print_endline "";;
print_int (Card.to_num (List.nth ((PArray.get b.colones 7).liste) 1));;
print_endline "";;
print_int (Card.to_num (List.nth ((PArray.get b.colones 7).liste) 2));;
print_endline "";;

print_endline "";;
print_endline "Registre 1";;
print_int (Card.to_num (List.nth (b.registres) 0));;
print_endline "Registre 2";;
print_int (Card.to_num (List.nth (b.registres) 1));;
print_endline "Registre 3";;
print_int (Card.to_num (List.nth (b.registres) 2));;
print_endline "Registre 4";;
print_int (Card.to_num (List.nth (b.registres) 3));; *)
