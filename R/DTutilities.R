# French localization for DataTables
dt_translation <- list(
    emptyTable = "Aucune donn\u00E9e disponible",
    info = "\u00C9l\u00E9ments _START_ \u00E0 _END_ affich\u00E9s sur _TOTAL_",
    paginate = list(previous = "Pr\u00E9c\u00E9dent", `next` = "Suivant", first = "Premier", last = "Dernier"),
    decimal = "",
    infoEmpty = "Aucune entr\u00E9e \u00E0 afficher",
    infoFiltered = "(filtr\u00E9 sur _MAX_ entr\u00E9es totales)",
    infoPostFix = "",
    thousands = "",
    lengthMenu = "Afficher _MENU_ entr\u00E9es",
    loadingRecords = "Chargement...",
    processing = "",
    search = "Rechercher:",
    zeroRecords = "Aucun enregistrement correspondant trouv\u00E9",
    aria = list(
        orderable = "Trier par cette colonne",
        orderableReverse = "Ordre inverse par cette colonne"
    )
)

# Buttons that allow to select or deselect all the rows displayed
dt_select_deselect_buttons <- list(list(
    extend = "selectAll",
    text = "S\u00E9lectionner affich\u00E9s",
    action = DT::JS("function (e, dt, node, config) {
        dt.rows({ search: 'applied'}).deselect();
        dt.rows({ search: 'applied'}).select();
    }")
), list(
    extend = "selectNone",
    text = "D\u00E9s\u00E9lectionner affich\u00E9s",
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
