# elp-elm

## Dev

`git clone https://github.com/Gui-Yom/elp-elm`

Il est recommandé d'utiliser [elm-live](https://www.elm-live.com/) :

`elm-live src/Main.elm -o -d src -- --output src/index.js`
ou
`dev.bat`

## Lancement

`$ elm make src/Main.elm --output src/main.js`

Puis ouvrir index.html dans un navigateur. Ou utiliser
la [version publiée sur Github Pages](https://gui-yom.github.io/elp-elm).

![screenshot](screenshot.png)

Les différents composants de l'interface sont synchronisés. Ils se mettent à jour automatiquement à la modification du
programme. Cependant, il est important de noter qu'il existe des "points de sauvegarde". Le panel sur la droite et le
détail du programme afficheront toujours la dernière version fonctionnelle du programme.

Il est aussi important de noter que les instructions modifiant le curseur comme Color ou Width ont pour champ d'effet le
reste du programme. Il aurait été intéressant d'avoir un concept de stack frame, une couleur définie dans une procédure
ne déborde pas dans la procédure parente.

## Code

Le code est séparé en fichiers contenants chacun un composant de l'interface.

- `Canvas.elm`: Zone de dessin svg (panel à droite)
- `ErrorList.elm`: Espace décrivant les erreurs dans le programme
- `ProgramList.elm`: Espace détaillant le programme en cours
- `Manual.elm`: Zone d'aide
- `Main.elm`: Page liant tous les composants

Le programme est sauvegardé dans le stockage local du navigateur et persiste en les sessions.

- `LocalStorage.elm`: Bindings vers l'API LocalStorage du navigateur
- `Program.elm`: Parser et checker du programme

## Fonctionnalités du langage

- Instructions de base : Forward, Left, Right, Repeat
- Paramètres de pinceau : Color, Width
- Définition de procédures : Call
- Détection d'erreurs de syntaxe
- Détection de boucles infinies lors des appels
- Détection des appels invalides
- Stack pour les paramètres Color et Width (la modification dans un scope n'affecte pas la scope parent)
