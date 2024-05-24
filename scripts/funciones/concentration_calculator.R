# concentration calculator aerobiological data --------------------

concentration_calculator <- function(data, correction_factor = 0.54){
  
  # correction_factor <- function(x, na.rm = TRUE) {x * correction_factor}
  
  temporal_data <- 
    data |> 
    stats::na.omit(
    ) |> 
    dplyr::mutate(
      date = lubridate::date(datetime),
      .after = datetime
    ) |> 
    dplyr::group_by(
      date
    ) |> 
    dplyr::mutate(
      hour_count = forcats::as_factor(dplyr::n()),
      .after = date
    ) |> 
    dplyr::ungroup()
  
  concentration_data <- 
    temporal_data |> 
    dplyr::mutate(
      dplyr::across(
        dplyr::where(base::is.numeric), 
        ~ base::round(
          .x * correction_factor * base::as.numeric(base::levels(hour_count))[hour_count]
        )
      )
    ) |> 
    dplyr::select(
      -c(
        date,
        hour_count
      )
    ) |> 
    purrr::discard(~base::all(is.na(.) | . < 1)
    ) # descarta las columnas que sólo contienen NA y 0

  
  return(concentration_data)
}

# Ejemplo de como funciona
# concentration_data <- 
#   clean_data |> 
#   concentration_calculator(
#     correction_factor = 0.54
#   )
