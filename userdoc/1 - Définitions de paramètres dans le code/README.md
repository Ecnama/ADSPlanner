# 1 - Définitions de paramètres dans le code

Il existe quelques paramètres de l'application qui n'ont pas été rendus configurables depuis l'interface. Cependant, leur modification reste facile.

## Sessions par filière

Le paramètre du nombre de sessions par filières est disponible dans le fichier [`config.R`](../../R/config.R), dans le vecteur `NB_SESSIONS`.

Il est également possible de "décaler" les sessions d'une filière, pour que les FISP aient les deux dernières sessions au lieu des deux premières, par exemple (ce qui est le cas par défaut). Pour cela, il suffit de mettre le numéro de la première session de la filière dans `SESSION_DEBUT`.

Actuellement, il n'est pas possible d'avoir la première et la troisième session. Pour cela, il faudra simplement échanger des colonnes manuellement dans le fichier Excel de sortie.

## Ajout de département

Si un département était ajouté (ou enlevé) à l'INSA, il suffirait de mettre à jour le vecteur `DEPARTS` dans [`config.R`](../../R/config.R).

## Ajout de filière

Au cas où une filière viendrait à être ajoutée à l'INSA, il y aurait des modifications plus importantes à apporter au code. 

Il faudrait tout d'abord ajouter cette filière dans [`config.R`](../../R/config.R).

Ensuite, le format du fichier d'entrée aura forcément changé, il faudra donc apporter les modifications appropriées à [`input.R`](../../R/input.R).