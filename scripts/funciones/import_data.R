# import raw data aerobiological count --------------------

import_data <- function(route){
  
  import_data <- 
    base::list.files(
      path = here::here(route),
      pattern = "*csv",
      full.names = TRUE
    ) |>
    purrr::map_dfr(
      readr::read_csv,
      col_types = readr::cols(.default = "?")
    )
  
  base::return(
    import_data
  )
}

# Ejemplo de como funciona
# raw_data <- import_data("raw_data/conteo_aerobiologico/")