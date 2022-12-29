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
