# Rename Morphotypes --------------------
# Cambia los nombres de los morfotipos a un formato humanamente legible
# Funciona en un tibble largo donde los morfotipos están en una variable
# `data` es un tibble largo
# `variable_names`es el nombre de la variable/columna donde están los morfotipos
# Cambia el nombre de la variable ingresada por `pollen`.

rename_morphotypes <- function(data, variable_name) {
  variable_name <- rlang::enquo(variable_name)
  
  data <- 
    data |> 
    dplyr::rename(
      pollen = !!variable_name
    ) |> 
    dplyr::mutate(
      pollen = dplyr::case_match(
        pollen,
        "acacia" ~ "Acacia",
        "alnus" ~ "Alnus",
        "alternanthera" ~ "Alternanthera",
        "amaranthaceae" ~ "Amaranthaceae",
        "ambrosia" ~ "Ambrosia",
        "apiaceae" ~ "Apiaceae",
        "arecaceae" ~ "Arecaceae",
        "asteraceae_anthemideae" ~ "Asteraceae: Anthemideae",
        "asteraceae_helianthus" ~ "Asteraceae: Helianthus",
        "asteraceae_lactuceae" ~ "Asteraceae: Lactuceae",
        "asteraceae_senecio" ~ "Asteraceae: Senecio",
        "casuarina" ~ "Casuarina",
        "cecropia" ~ "Cecropia",
        "cupressaceae" ~ "Cupressaceae",
        "cyperaceae" ~ "Cyperaceae",
        "fraxinus" ~ "Fraxinus",
        "juglans" ~ "Juglans",
        "loranthaceae" ~ "Loranthaceae",
        "moraceae" ~ "Moraceae",
        "myrtaceae" ~ "Myrtaceae",
        "pinus" ~ "Pinus",
        "plantago" ~ "Plantago",
        "poaceae" ~ "Poaceae",
        "ricinus" ~ "Ricinus",
        "rumex" ~ "Rumex",
        "urticaceae" ~ "Urticaceae",
        "no_identificado" ~ "No identificado",
        "total_pollen" ~ "Total Pollen"
      )
    )
  
  return(data)
}

# Ejemplo de como funciona
# concentracion_1d_long <- rename_morphotypes(concentracion_1d_long, "type")
