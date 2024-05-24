# clear raw data aerobiological count --------------------

clear_data_2 <- function(data, col_date){
  
  col_date <- 
    rlang::enquo(col_date) # Depende del nombre de la variable fecha
  
  clean_data <- 
    data |>
    janitor::clean_names() |> # Facilita el posterior tratamiento de las variables
    purrr::discard(
      ~base::all(is.na(.) | . < 1)
    ) |> # descarta las columnas que sólo contienen NA y 0
    dplyr::rename(
      datetime = !!col_date
    )# El nombre de la variable es homogénea con el resto del código
  
  base::return(
    clean_data
  )
}

# Ejemplo de como funciona
# clean_data <- clear_data(raw_data, "fecha")
