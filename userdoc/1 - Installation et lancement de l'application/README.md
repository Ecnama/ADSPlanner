# 1 - Installation et lancement de l'application

## Installation de R

R est disponible sur :
- **Windows :** https://cran.r-project.org/bin/windows/base/
- **MacOS :** https://cran.r-project.org/bin/macosx/
- **Linux :** Package disponible pour toutes les distributions majeures, chercher dans les repos.

Si ce n'est pas déjà fait, il faut aussi installer `git`.

## Récupération des fichiers

Pour récupérer les fichiers de ce dépôt, exécuter la commande suivante :

```sh
git clone https://github.com/Ecnama/ADSPlanner.git
```

## Installation du package

On commence par entrer dans l'environnement R (depuis le dossier `ADSPlanner`) :

```sh
R
```

Malheureusement, R ne permet pas nativement d'installer un package local sans une manipulation peu pratique. Pour simplifer ce processus, il faudra donc installer le package `devtools` :

```R
install.packages("devtools")
```

Une fois que l'installation est terminée (elle peut prendre un certain temps), on peut simplement installer notre package :

```R
devtools::install_local(".")
```

## Lancement de l'application

Une fois le package installé, il suffit d'exécuter `ADSPlanner::run()` depuis une session R pour le lancer. Si un navigateur ne s'ouvre pas automatiquement, il faudra ctrl-cliquer le lien affiché dans le terminal.