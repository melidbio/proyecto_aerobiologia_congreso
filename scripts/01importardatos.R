##importacion y preparacion de los datos----

here::here()
source(here::here("scripts", "00setup.R"))

##importar los datos----

raw_data<-
  import_data(
    here::here(
      "datos",
      "raw-data",
      "muestreo-2011")
  
  )


# Limpiar los datos ----


clean_data <- 
  clear_data_2(
    raw_data,
    col_date = fecha
  ) |> 
  stats::na.omit()

# seleccionar morfotipos----

seleccion_morfotipos <- 
      c(
        "alnus",
        "alternanthera",
        "amaranthaceae",
        "anacardiaceae_mangifera_indica",
        "arecaceae",
        "asteraceae_helianthus",
        "begonia",
        "cannabaceae_cannabis",
        "cannabaceae_celtis",
        "citrus",
        "cupressaceae",
        "cyperaceae",
        "euphorbiaceae",
        "euphorbiaceae_ricinus",
        "fraxinus",
        "moraceae",
        "myrtaceae",
        "pinus",
        "plantago",
        "poaceae",
        "rumex",
        "urticaceae",
        "urticaceae_cecropia",
        "no_identificado"
      )


clean_data_selection<-
  clean_data |> 
  dplyr::select(
    datetime,
    dplyr::any_of(seleccion_morfotipos)
  ) |> 
  dplyr::rename(
    mangifera_indica = anacardiaceae_mangifera_indica ,
    cecropia = urticaceae_cecropia 
  ) |> 
dplyr::mutate(
  cannabaceae = cannabaceae_cannabis + cannabaceae_celtis,
  ricinus = euphorbiaceae_ricinus + euphorbiaceae
  ) |> 
  dplyr::select(
    -cannabaceae_cannabis,
    -cannabaceae_celtis,
    -euphorbiaceae,
    -euphorbiaceae_ricinus
  )

da


from<-"2010-02-01 12:00:00"


## Fechas corregidas----
dplyr::rename(
  fecha_original = datetime
) |> 
  dplyr::mutate(
    datetime = base::seq.POSIXt(
      from = lubridate::as_datetime("2010-02-01 12:00:00"),
      to = 
    )
  )
