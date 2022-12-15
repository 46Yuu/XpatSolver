# MEMBRES DE L'EQUIPE
| | | | |
|---|---|---|---|
| Nom | Prénoms | Numéro Etudiant | Pseudo gitlab |
| TRAN | Henri Bergson | 22007962 | tranh |
| KOUE-HEMAZRO | Kankoé Ange Herman | 22032729 | kouehema |
| MANSOURI | Manel | 22114942 | mansouri |

# Fonctionalités
On a implémenté quasiment tout le jalon 1 mais notre problème c'est qu'on pensait que les entiers correspondait aux cartes du genre 0 .. 13 correspondait à As de trèfle ... Roi de trèfle jusqu'à 51. Mais on s'est rendu compte trop tard que c'etait pas le cas du coup on est en train de changer toute nos fonctions la veille et si on y arrive pas à temps les tests ne marcheront probablement pas tous.

# Compilation et exécution
dune build
make test

# Découpage modulaire
On va pas mentir on a presque tout mis dans le fichier Etat.ml, on a creer d'autres fichiers comme Regle.ml et Config.ml mais Etat.ml contient plusieurs types et toute les fonctions de check

# Organisation du travail
Henri a fait normalisation et Shuffle
Ange s'est occupé de la creation des Etats et de l'otion check qu'il a commencé seul mais au final nous travaillonstous deux sur la validation des coups avec check

# Misc
On s'est rendu compte trop tard de notre problème de carte et aussi de la manière d'ajouter de nouveau module.