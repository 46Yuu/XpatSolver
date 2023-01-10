# MEMBRES DE L'EQUIPE
| | | | |
|---|---|---|---|
| Nom | Prénoms | Numéro Etudiant | Pseudo gitlab |
| TRAN | Henri Bergson | 22007962 | tranh |
| KOUE-HEMAZRO | Kankoé Ange Herman | 22032729 | kouehema |
| MANSOURI | Manel | 22114942 | mansouri |

# Fonctionalités
On a implémenté quasiment tout le jalon 1 mais notre problème c'est qu'on pensait que les entiers correspondait aux cartes du genre 0 .. 13 correspondait à As de trèfle ... Roi de trèfle jusqu'à 51. Mais on s'est rendu compte trop tard que c'etait pas le cas du coup on est en train de changer toute nos fonctions la veille et si on y arrive pas à temps les tests ne marcheront probablement pas tous.

Pour le jalon 2, on a tout d'abord terminé le jalon 1 puis pour la recherche on a terminé toute nos fonctions et on les a testés une à une et elles marchaient toutes sauf bien sur le parcours qui nous menait dans une boucle infini.



# Compilation et exécution
dune build
make test

# Découpage modulaire
Etat.ml contient toutes les fonctions pour le check et Searching.ml toutes celles pour le search. 
Il y a d'autres fichiers comme Regle.ml , Config.ml qui sont des types pour Etat.ml. 

# Organisation du travail
Henri a fait normalisation et Shuffle
Ange s'est occupé de la creation des Etats et de l'option check qu'il a commencé seul mais au final nous travaillons tous deux sur la validation des coups avec check.
Pour la partie de search Ange s'est occupé d'implémenter la recherche et des fonctions principales , Henri s'est occupé d'implémenter des fonctions secondaires 
comme tout_coups_possibles ou cartes_en_tete_de_colone. Puis on s'est mis à deux pour essayer de régler tout les problèmes restants.

# Misc
On s'est rendu compte trop tard de notre problème de carte et aussi de la manière d'ajouter de nouveau module pendant le jalon 1 et on n'a pas réussi a trouver 
comment régler le probleme sur la recherche qui étaient infinie a un moment puis qui ne trouve pas de solution après avoir régler le premier problème. 