
type regle = {
  nb_colones : int;
  nb_registres : int;
  nb_registres_utilise : int;
  (* ce tableau contient le nombre de cartes qui doit aller dans chaque colone*)
  tab_nb_cartes_colone : int array;
  recoi_couleur_alternee : bool;
  recoi_meme_couleur : bool;
  colone_vide_remplissable: bool;
  (*Tableau des cartes qui peuevnt aller dans ue colone vide*)
  tab_cartes_colone_vide : Card.card array;
  
};;