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
      else 1
    end
  else 1
  
module States = Set.Make (struct type t = etat let compare = compare_state end)

let search (conf : Config.config) :unit=
  let etatPartie = normalisation (etatAPartirDeConfiguration conf) in
  let etat_restant_a_visiter = States.add etatPartie States.empty in
  let etat_deja_traite = States.empty in
  print_endline "Bref le search arrive la"