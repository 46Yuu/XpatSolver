(** A FIFO structure (First-In First-Out),
    implemented in functional style
    (NB: the Queue module of OCaml stdlib is imperative)

    NB: l'implémentation fournie initialement ci-dessous est inefficace,
    l'améliorer (tout en restant fonctionnel). Par exemple on peut utiliser
    une paire de listes pour implémenter ['a t].

*)

type 'a t = {tail :'a list ;head : 'a list}  (* head of list = first out *)
let empty = {tail = [];head = []}
let push x q = {tail = x::q.tail;head = q.head}
let pop q = 
  match q.tail,q.head with 
  | tail',x::head' -> x,{tail =tail'; head =head'}
  | tail',[] -> match List.rev tail' with
    | [] -> raise Not_found
    | x::head' -> x , {tail = [];head = head'}
let of_list l = {tail = [];head = List.rev l}
let to_list l = l.tail@l.head
