# Projet Fonctionnel, rendu 1.1
Le projet compile sans problème. Voici des bugs qui ont été trouvés :
. Un begin peut être fermé par )
Fix

. L’exemple suivant ne passe pas :
let a = 3 ;;
let b = 4 ;;
let c = 5 in
prInt (a+b+c)
Fix

. la ligne     | v=VAR ASSIGN e=expression             { Assign(Var v, e) }
dans le parser est trop restrictive : on peut avoir une expression à gauche de :=
Fix, on peut mettre une applic, + amélioration de la gestion des ref
. il faut pouvoir faire try .. with (E i) -> ...    (avec les parenthèses)
Fix
. le code    let u = -3,4 in prInt 3    est refusé par fouine.
même chose pour let u = 3, ref () in ...
. le fichier minus-list.ml, qui faisait partie de l'archive de tests fournie, est refusé par fouine. meme chose pour plus-list.ml.
.  l'ordre d'évaluation des éléments d'une liste n'est pas le bon
. le programme    let l = 3::4+5::[] in prInt 2     est refusé par fouine
. le programme    let f x = match x with | (h1 :: l1, l2) -> 3 in prInt 3   est refusé par fouine
Inverser smotif et motif ?

# Projet Fonctionnel, rendu 1

Je suis conscient les motifs où _ doit "jeter astucieusement" ne passent pas :
let f l = match l with |_::p::[] -> p (*devrait renvoyer le dernier élément d'une liste*)

L'implémentation des for et while en continuation a été laissé de côté par manque de temps

On porte une attention particulière au liste ne se terminant pas par un ::[], afin de pouvoir implémenter concat par exemple
Pour cela tout List(l) se doit de respecter l de la forme : _::...::_::List([])::[] implicitement
On peut avoir l1::l2 mais l2 doit respecter la condition

Les failwith qui restent dans le code sont des erreurs très "spécifiques", où l'exception qu'ils provoquent ne serait pas généralisable

L'implémentation actuelle des exceptions permettrait totalement de créer une exception qui prend comme paramètre un valeur au lieu d'un entier, mais le type entier est forcé pour se conformer au sujet

Les variables anonymes (lié à l'implémentation de function) intéragissent actuellement assez mal avec le paramètre d'éxécution -show-src ; même si les tests fonctionneront probablement, l'implémentation aurait dû être mieux travaillée

On ne parse toujours pas 2+3::[] mais des améliorations ont été apportées pour améliorer la clarté du parseur et reconnaître plus de formulation (potentiellement pathologiques). Par exemple on peut écrire f x:: []


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


