(** Etat *)
open Regle;;


let midnight_oil = {
  nb_colones = 18;
  nb_registres = 0;
  nb_registres_utilise = 0;
  tab = [|3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;1|];

};;
let seahaven = {
  nb_colones = 10;
  nb_registres = 4;
  nb_registres_utilise = 2;
  tab = [|5;5;5;5;5;5;5;5;5;5|];

};;
let b =  Etat.creer_partie (XpatRandomExemple.permutation_graine_1) midnight_oil;;
print_int (Card.to_num (List.nth ((PArray.get b.colones 9).liste) 0));;
print_endline "";;
print_int (Card.to_num (List.nth ((PArray.get b.colones 9).liste) 1));;
print_endline "";;
print_int (Card.to_num (List.nth ((PArray.get b.colones 9).liste) 2));;
print_endline "";;

print_endline "";;
print_endline "Registre 1";;
print_int (Card.to_num (List.nth (b.registres) 0));;
print_endline "Registre 2";;
print_int (Card.to_num (List.nth (b.registres) 1));;
print_endline "Registre 3";;
print_int (Card.to_num (List.nth (b.registres) 2));;
print_endline "Registre 4";;
print_int (Card.to_num (List.nth (b.registres) 3));;
