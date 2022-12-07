(** Etat *)

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


      
    


let a = (1,Card.Trefle);;
print_endline "Hello world";;

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






print_endline "\nsuite";;



let maPile = Stack.create();;
Stack.push "Wesh" maPile;;
Stack.pop maPile;;


(*let roiCoeur = Card.card  (rank : 13,Card.suit Coeur) *)