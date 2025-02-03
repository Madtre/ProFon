# Projet Fonctionnel, rendu 0

Ce répertoire contient un programme de départ à partir duquel vous pouvez programmer votre fouine.

## Compilation et execution

Les commandes ci-dessous sont à exécuter dans le répertoire principal, qui est au-dessus de bin/ et lib/.

pour (re)compiler, lancer
```
dune build
```

pour executer le programme, lancer
```
dune exec bin/fouine.exe
```

entrez ensuite une expression arithmetique, avec juste `+ -` et `*`, comme par exemple `4+3*5`



On vous conseille d'exécuter une fois la commande
```
ln -s _build/default/bin/fouine.exe fouine
```
pour ne pas avoir à saisir le `_build/default/main.exe`

Après quoi vous pouvez
- taper `./fouine` pour lancer l'exécutable et saisir une expression au clavier
- taper `./fouine test/basic.ml` pour lancer fouine sur le fichier basic.ml

## Ce qui est attendu

Lisez les fichiers dans cette archive, compilez le code (cf. ci-dessous)

Puis étendez fouine. Voici une succession d'étapes, traitez celles que vous pouvez, dans la mesure de vos possibilités.  

1. expressions arithmétiques. 
Contemplez l’évaluation des expressions arithmétiques, améliorez les choses du point de vue de la factorisation du code si vous voyez un moyen de le faire ; sinon, laissez en l’état pour le moment.
Implémentation des casts qui permettent de forcer le type d'une valeur avec message d'erreur si le type n'est pas bon
2. opérateurs booléens et `if.. then.. else`
Cast en booléen et priorité mise sur le else
Généralisation de l'affichage
3. fonction `prInt` (cf. les notes de cours)
Implémenté et exploité ; mise en place d'une valeur booléenne permettant de cacher les affichages de test
4. ajout des déclarations let.. in
Le contexte est géré par une Hashmap
Implémentation du let _ = ..

5. ajout des fonctions
(on utilise initialement la règle pour notre grammaire var expr)
Les fonctions sont vues commes un type de valeur qui tient sa propre hashmap. L'environnement de la fonction remplace le reste lorsqu'elle est executée
Formes acceptées :
fun x -> ..
fun x -> fun y ->
fun x y ->
let f x y = ..
6. fonctions récursives
Les déclarations des rec lorsqu'elle sont evalués forcent l'ajout de la fonction à son propre contexte
Autant de liberté pour créer ces fonctions qu'à la 5.
Ajout de messages d'erreurs liés à des utilisations inadaptés d'un rec

Pour chaque étape, référez-vous aux notes de cours (partie 1.2). 
Écrivez à chaque fois de petits fichiers de tests dans le répertoire `test/`

-> mettre des tests dans un fichier autre que basic.ml fait planter la lecture des fichiers définitivement. J'ai donc écrasé les tests à chaque fois

## Travailler sur fouine, modifier le code

`bin/fouine.ml` : fichier principal

`lib/expr.ml` : définition des expressions, de l'affichage et de l'évaluation

`lib/lexer.mll` : lexèmes, analyse lexicale

`lib/parser.mly` : règles de grammaire, analyse syntaxique

`tests/` : sous-répertoire de tests

fichiers `dune` et `dune-project` : pour la compilation, à ne pas modifier a priori

En principe vous n'avez à modifier que les fichiers
  `expr.ml`    `lexer.mll`    `parser.mly`
dans le répertoire `lib/`

## Erreurs à la compilation en lien avec le lexer et le parser :
   référez-vous au fichier **README-parser.md*, dans cette archive

## Date limite pour téléverser ce rendu sur la page du portail des études :

   **lundi 3 février 2025 à 23h59**

     . indiquez brièvement dans le fichier Readme ce que vous avez pu ajouter à fouine
     . envoyez une archive qui compile
