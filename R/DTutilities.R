# French localization for DataTables
dt_translation <- list(
    emptyTable = "Aucune donnée disponible",
    info = "Éléments _START_ à _END_ affichés sur _TOTAL_",
    paginate = list(previous = "Précédent", `next` = "Suivant", first = "Premier", last = "Dernier"),
    decimal = "",
    infoEmpty = "Aucune entrée à afficher",
    infoFiltered = "(filtré de _MAX_ entrées totales)",
    infoPostFix = "",
    thousands = "",
    lengthMenu = "Afficher _MENU_ entrées",
    loadingRecords = "Chargement...",
    processing = "",
    search = "Rechercher:",
    zeroRecords = "Aucun enregistrement correspondant trouvé",
    aria = list(
        orderable = "Trier par cette colonne",
        orderableReverse = "Ordre inverse par cette colonne"
    )
)

# Buttons that allow to select or deselect all the rows displayed
dt_select_deselect_buttons <- list(list(
    extend = "selectAll",
    text = "Sélectionner affichés",
    action = DT::JS("function (e, dt, node, config) {
        dt.rows({ search: 'applied'}).deselect();
        dt.rows({ search: 'applied'}).select();
    }")
), list(
    extend = "selectNone",
    text = "Désélectionner affichés",
    action = DT::JS("function (e, dt, node, config) {
        dt.rows({ search: 'applied'}).select();
        dt.rows({ search: 'applied'}).deselect();
    }")
))

# Global options for all DataTables
options(DT.options = list(
    language = dt_translation,
    scrollY = 320,
    scroller = TRUE
))
