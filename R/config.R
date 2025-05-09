NB_SESSIONS <- c(
    "FC_FIRE" = 3,
    "EMIR" = 2,
    "MICA" = 2
)

# Numero de la premiere session pour chaque filiere (utile pour celle qui ont moins du maximum de sessions)
# Si c'est la session du milieu que les FISP ne font pas, ce cas n'est pas supporte
# Une solution simple dans ce cas est de simplement echanger deux sessions pour tout le monde, apres l'affectation
SESSION_DEBUT <- c(
    "FC_FIRE" = 1,
    "EMIR" = 2,
    "MICA" = 2
)
