type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode }