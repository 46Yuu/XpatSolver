
type regle = {
  nb_colones : int;
  nb_registres : int;
  nb_registres_utilise : int;
  (* ce tableau contient le nombre de cartes qui doit aller dans chaque colone*)
  tab_nb_cartes_colone : int array;
  
};;