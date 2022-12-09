(** Dechet Etat *)
open Regle;;

let maPile = Stack.create();;
Stack.push "Wesh" maPile;;
Stack.pop maPile;;


(*Une colone est un pile de cartes*)
type colone = {
  pile : Card.card Stack.t;

};;

(*Un régistre contient juste une carte*)
type registre = {
  carte : Card.card;

};;

type coup = {
  vide : string;

};;

type depot = {
  nb_cartes_depose : int;

};;
type etat = {
  nb_colones : int;
  colones : colone PArray.t;
  nb_registres : int;
  registres : registre PArray.t;
  nb_registres_dispo : int;
  depot : depot PArray.t;
  coups : coup list;
};;

(*Utiliser list tl*)
(*let remplissage_colone_free_cell (colones : colone PArray.t) (permutation : int list) (num_colone :int) (debut :int) (fin :int) :colone PArray.t = 

  (**if(debut > fin) then fin
  else
     Stack.push (Card.of_num (List.nth permutation debut)) (PArray.get colones num_colone).pile;
    remplissage_colone_free_cell colones permutation num_colone (debut+1) fin;;*)
    print_endline "avant la boucle et num_colone est ";
    print_int num_colone;
    print_endline "  apres";
    for k = debut to fin do
      Stack.push (Card.of_num (List.nth permutation k)) (PArray.get colones num_colone).pile;
      (*print_endline "le truc retiré est";
      print_int (Card.to_num (Stack.pop (PArray.get colones num_colone).pile));*)
    done;
    colones
  ;;
*)
  let remplissage_colone (permutation : int list) (debut :int) (fin :int) :colone = 

      let maPile = Stack.create() in

      for k = debut to fin do
        Stack.push (Card.of_num (List.nth permutation k)) maPile;
        (*print_endline "le truc retiré est";
        print_int (Card.to_num (Stack.pop (PArray.get colones num_colone).pile));*)
      done;
      {pile = maPile}
    ;;

    (* Rempli les colones de freeCell selon les indications*)
let remplir_colone_free_cell (permutation : int list) (num_colone: int) :colone = 
  if num_colone = 0 then remplissage_colone permutation 0 6
  else if num_colone = 1 then remplissage_colone permutation 7 12
  else if num_colone = 2 then remplissage_colone permutation 13 19
  else if num_colone = 3 then remplissage_colone permutation 20 25
  else if num_colone = 4 then remplissage_colone permutation 26 32
  else if num_colone = 5 then remplissage_colone permutation 33 38
  else if num_colone = 6 then remplissage_colone permutation 39 45
  else remplissage_colone permutation 46 51
  ;;
let free_cell (permutation : int list) : etat =

  (*let colones = PArray.make 8 ({pile = Stack.create()}) in*)
  let colones = PArray.init 8 (remplir_colone_free_cell permutation) in
  let registres = PArray.make 4 ({carte = Card.of_num 100}) in
  let depots = PArray.make 4 ({nb_cartes_depose = 0}) in
  let coups = [] in
  (*remplissage_colone_free_cell colones permutation 0 0 6;
  remplissage_colone_free_cell colones permutation 1 7 12 ;
  remplissage_colone_free_cell colones permutation 2 13 19 ;
  remplissage_colone_free_cell colones permutation 3 20 25 ;
  remplissage_colone_free_cell colones permutation 4 26 32 ;
  remplissage_colone_free_cell colones permutation 5 33 38 ;
  remplissage_colone_free_cell colones permutation 6 39 45 ;
  remplissage_colone_free_cell colones permutation 7 46 51 ;
  let c0 = PArray.set colones 0 (remplissage_colone permutation 0 6) in
  let c1 = PArray.set c0 1 (remplissage_colone permutation 7 12) in
  let c2 = PArray.set c1 2 (remplissage_colone permutation 13 19) in
  let c3 = PArray.set c2 3 (remplissage_colone permutation 20 25) in
  let c4 = PArray.set c3 4 (remplissage_colone permutation 26 32) in
  let c5 = PArray.set c4 5 (remplissage_colone permutation 33 38) in
  let c6 = PArray.set c5 6 (remplissage_colone permutation 39 45) in
  let c7 = PArray.set c6 7 (remplissage_colone permutation 46 51) in
  *)
  


  {
    nb_colones = 8;
    colones = colones;
      (*remplissage_colone_free_cell 
      (remplissage_colone_free_cell colones permutation 1 7 12)
      permutation 0 0 6;*)
    nb_registres = 4;
    nb_registres_dispo = 0;
    registres = registres;
    
    depot = depots;
    coups = coups;
  }






  (*Remplissage des colones
  let colones = PArray.make 8 ({pile = Stack.create()}) in
  let debut = remplissage_colone_free_cell colones permutation 0 0 7 in
  (*On a 8 colones du coup on les parcourent*)
  for i = 1 to 7 do
    (*Si le numero de colone est par, on push 7 cartes et 6 sinon*)
    if i mod 2 = 0 then
      (*for k = 0 to 6 do
        Stack.push (Card.of_num (List.nth permutation j)) (PArray.get colones i).pile;
      done;*)
      remplissage_colone_free_cell colones permutation i debut (debut + 6)
    else
      remplissage_colone_free_cell colones permutation i debut (debut + 5)
  done;*)

;;

  (* Rempli les colones de seaHaven selon les indications*)
let remplir_colone_seahaven (permutation : int list) (num_colone: int) :colone = 
  if num_colone = 0 then remplissage_colone permutation 0 4
  else if num_colone = 1 then remplissage_colone permutation 5 9
  else if num_colone = 2 then remplissage_colone permutation 10 14
  else if num_colone = 3 then remplissage_colone permutation 15 19
  else if num_colone = 4 then remplissage_colone permutation 20 24
  else if num_colone = 5 then remplissage_colone permutation 25 29
  else if num_colone = 6 then remplissage_colone permutation 30 34
  else if num_colone = 7 then remplissage_colone permutation 35 39
  else if num_colone = 8 then remplissage_colone permutation 40 44
  else  remplissage_colone permutation 45 49
  ;;
let seahaven (permutation : int list) : etat =

  (*let colones = PArray.make 8 ({pile = Stack.create()}) in*)
  let colones = PArray.init 10 (remplir_colone_seahaven permutation) in
  let registres = PArray.make 4 ({carte = Card.of_num 100}) in
  let depots = PArray.make 4 ({nb_cartes_depose = 0}) in
  let coups = [] in
  let r1 = PArray.set registres 0 ({carte = Card.of_num (List.nth permutation 50)})  in
  let r2 = PArray.set r1 1 ({carte = Card.of_num (List.nth permutation 51)})  in

  
  {
    nb_colones = 10;
    colones = colones;
      (*remplissage_colone_free_cell 
      (remplissage_colone_free_cell colones permutation 1 7 12)
      permutation 0 0 6;*)
    nb_registres = 4;
    registres = r2;
    nb_registres_dispo = 2;
    depot = depots;
    coups = coups;
  }


;;



let midnight_oil = {
  nb_colones = 18;
  nb_registres = 0;
  nb_registres_utilise = 0;
  tab = [||];
};;
  (*if num_colone = 0 then remplissage_colone permutation 0 4
  else if num_colone = 1 then remplissage_colone permutation 5 9
  else if num_colone = 2 then remplissage_colone permutation 10 14
  else if num_colone = 3 then remplissage_colone permutation 15 19
  else if num_colone = 4 then remplissage_colone permutation 20 24
  else if num_colone = 5 then remplissage_colone permutation 25 29
  else if num_colone = 6 then remplissage_colone permutation 30 34
  else if num_colone = 7 then remplissage_colone permutation 35 39
  else if num_colone = 8 then remplissage_colone permutation 40 44
  else  remplissage_colone permutation 45 49
  ;;*)

(*let remplir_partie (permutation : int list) (regle : Regle.regle) (num_colone: int) (num_colone: int): colone = 
  let maPile = Stack.create() in
  let debut = fst( regle.tab_valeurs.(num_colone)) in
  let fin = snd(regle.tab_valeurs.(num_colone) ) in
    for i = debut to fin do
      Stack.push (Card.of_num (List.nth permutation i)) maPile;
      (*print_endline "le truc retiré est";
      print_int (Card.to_num (Stack.pop (PArray.get colones num_colone).pile));*)
    done;
    {pile = maPile}
  ;;
let creer_partie (permutation : int list) (regle : Regle.regle): etat =

  let registres = PArray.make 4 ({carte = Card.of_num 100}) in
  let colones = PArray.init 10 (remplir_partie permutation regle) in
  for i = 0 to 1 do
    print_endline "registres";
  done;

  

  
  {
    nb_colones = regle.nb_colones;
    colones = colones;
      (*remplissage_colone_free_cell 
      (remplissage_colone_free_cell colones permutation 1 7 12)
      permutation 0 0 6;*)
    nb_registres = 4;
    registres = r2;
    nb_registres_dispo = 2;
    depot = depots;
    coups = coups;
  }


;;
*)
 
    


let a = (1,Card.Trefle);;
print_endline "Hello world";;
let tab = [|1;2|];;


let b = free_cell XpatRandomExemple.permutation_graine_1;;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 1).pile));; 
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 1).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 1).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 1).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 1).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 1).pile));;


print_endline "Pour zero";;

print_int (Card.to_num (Stack.pop (PArray.get b.colones 0).pile));; 
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 0).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 0).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 0).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 0).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 0).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 0).pile));;
print_endline "-";;


print_endline "Pour 7";;

print_int (Card.to_num (Stack.pop (PArray.get b.colones 7).pile));; 
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 7).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 7).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 7).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 7).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get b.colones 7).pile));;
print_endline "-";;

print_endline "\nseahaven now";;
let seahaven = seahaven XpatRandomExemple.permutation_graine_1;;
print_endline "Pour zero";;

print_int (Card.to_num (Stack.pop (PArray.get seahaven.colones 0).pile));; 
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get seahaven.colones 0).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get seahaven.colones 0).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get seahaven.colones 0).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get seahaven.colones 0).pile));;
print_endline "Pour 9";;


print_int (Card.to_num (Stack.pop (PArray.get seahaven.colones 9).pile));; 
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get seahaven.colones 9).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get seahaven.colones 9).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get seahaven.colones 9).pile));;
print_endline "-";;
print_int (Card.to_num (Stack.pop (PArray.get seahaven.colones 9).pile));;
print_endline "";;
print_endline "Les 2 derniers";;


print_int (Card.to_num (PArray.get seahaven.registres 0).carte);;
print_endline "-";;
print_int (Card.to_num (PArray.get seahaven.registres 1).carte);;






let maPile = Stack.create();;
Stack.push "Wesh" maPile;;
Stack.pop maPile;;


(*let roiCoeur = Card.card  (rank : 13,Card.suit Coeur) *)

(*TEST-------------------------------



(** Etat *)
open Regle;;

let maPile = Stack.create();;
Stack.push "Wesh" maPile;;
Stack.pop maPile;;


(*Une colone est un pile de cartes*)
type colone = {
  liste : Card.card list;

};;

(*Un régistre contient juste une carte*)
type registre = {
  carte : Card.card;

};;

type coup = {
  vide : string;

};;

type depot = {
  nb_cartes_depose : int;

};;
type etat = {
  nb_colones : int;
  colones : colone PArray.t;
  nb_registres : int;
  registres : Card.card list;
  nb_registres_dispo : int;
  depot : depot PArray.t;
  coups : coup list;
};;

(*Utiliser list tl*)

(*let remplissage_colone (permutation : int list) (regle : Regle.regle) (num_colone: int):colone = 

  let l1 = [] in
  let debut = fst( regle.tab_valeurs.(num_colone)) in
  let fin = snd(regle.tab_valeurs.(num_colone) ) in
  let rec aux (permutation : int list) (debut :int) (fin :int) (l : Card.card list):Card.card list =
    if debut > fin then l
    else aux permutation (debut+1) fin  (l @ [Card.of_num((List.nth permutation debut))] ) in

  {liste = aux permutation debut fin l1}
;;*)

(* Retourne la somme des élement d'un tableau jusqu'a la position i exclus*)
let sum_to_i t i = 
  let sum = ref 0 in
  for j = 0 to i - 1 do
    sum := !sum + Array.get t j ;
  done;  
  !sum;;

let remplissage_colone_t (permutation : int list) (regle : Regle.regle) (num_colone: int):colone = 
  let l1 = [] in
  let debut = sum_to_i regle.tab num_colone in
  let fin = debut + (Array.get regle.tab num_colone) - 1 in
  let rec aux (permutation : int list) (debut :int) (fin :int) (l : Card.card list):Card.card list =
    if debut > fin then l
    else aux permutation (debut+1) fin  (l @ [Card.of_num((List.nth permutation debut))] ) in

  {liste = aux permutation debut fin l1}
;;




let midnight_oil = {
  nb_colones = 18;
  (*tab_valeurs = [|(0,2);(3,5);(6,8);(9,11);(12,14);(15,17);(18,20);(21,23);(24,26);(27,29);(30,32);(33,35);(36,38);(39,41);(42,44);(45,47);(48,50);(51,51)|];*)
  nb_registres = 0;
  nb_registres_utilise = 0;
  tab = [||];
};;


let creer_partie (permutation : int list) (regle : Regle.regle): etat =

  let registres = ref [] in
  let depots = PArray.make 4 ({nb_cartes_depose = 0}) in
  let coups = [] in
  let colones = PArray.init 10 (remplissage_colone_t permutation regle) in
  let taille_permutation = List.length permutation in
  let nb_registres_utilise_debut = regle.nb_registres_utilise in
  if nb_registres_utilise_debut > 0 then
    for i = (taille_permutation - nb_registres_utilise_debut) to (taille_permutation - 1) do
      registres := [Card.of_num((List.nth permutation i))] @ !registres;
      print_int i;
      print_endline "-";
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
;;

 
    


let a = (1,Card.Trefle);;
print_endline "Hello world";;
let tab = [|1;2|];;


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
let b = creer_partie (XpatRandomExemple.permutation_graine_1) midnight_oil;;
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
let permutation_graine_1 =
  [ 13;32;33;35;30;46;7;29;9;48;38;36;51;41;26;20;23;43;27;
    42;4;21;37;39;2;15;34;28;25;17;16;18;31;3;0;10;50;49;
    14;6;24;1;22;5;40;44;11;8;45;19;12;47 ];;





(*let roiCoeur = Card.card  (rank : 13,Card.suit Coeur) *)
   
*)