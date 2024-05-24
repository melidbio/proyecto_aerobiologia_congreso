# Script datos aerobiológicos ----

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("here")) install.packages("here")
if (!require("janitor")) install.packages("janitor")
if (!require("AeRobiology")) install.packages("AeRobiology")
if (!require("ragg")) install.packages("ragg")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("ggtext")) install.packages("ggtext")
if (!require("gt")) install.packages("gt")

here::here()

source(here::here("aerobiologia-medellin", "scripts", "import_data.R"))
source(here::here("aerobiologia-medellin", "scripts", "clear_data_2.R"))
source(here::here("aerobiologia-medellin", "scripts", "concentration_calculator.R"))
source(here::here("aerobiologia-medellin", "scripts", "complete_datetime.R"))
source(here::here("aerobiologia-medellin", "scripts", "rename_morphotypes.R"))
source(here::here("aerobiologia-medellin", "scripts", "group_by_time.R"))
source(here::here("aerobiologia-medellin", "scripts", "gt_italic_pollen_types.R"))
source(here::here("aerobiologia-medellin", "scripts", "gt_theme_apa.R"))


# Importación y preparación de los datos ----  


# raw-conteo
raw_conteo <- 
  import_data(
    here::here(
      "aerobiologia-medellin",
      "raw-data",
      "conteo"
    )
  )

dplyr::glimpse(raw_conteo)


# clean-conteo
clean_conteo <- 
  clear_data_2(
    raw_conteo, 
    col_date = fecha
  )

dplyr::glimpse(clean_conteo)


# clean-conteo-selection
clean_conteo_selection <- 
  clean_conteo |> 
  dplyr::select(
    datetime,
    acacia,
    alnus,
    alternanthera,
    amaranthaceae,
    asteraceae_ambrosia,
    apiaceae,
    arecaceae,
    asteraceae_anthemideae,
    asteraceae_helianthus,
    asteraceae_lactuceae,
    asteraceae_senecio,
    cannabaceae_cannabis,
    casuarina,
    urticaceae_cecropia,
    cupressaceae,
    cyperaceae,
    fraxinus,
    juglans,
    loranthaceae,
    moraceae,
    myrtaceae,
    pinus,
    plantago,
    poaceae,
    euphorbiaceae_ricinus,
    rumex,
    urticaceae,
    no_identificado,
  ) |>  
  dplyr::rename(
    cecropia = urticaceae_cecropia,
    ricinus = euphorbiaceae_ricinus,
    ambrosia = asteraceae_ambrosia
  ) |> 
  dplyr::mutate(
    urticaceae = urticaceae + cannabaceae_cannabis
  ) |> 
  dplyr::select(
    -cannabaceae_cannabis
  )


# clean-conteo-total
clean_conteo_total <- 
  clean_conteo_selection |> 
  dplyr::mutate(
    total_pollen = base::rowSums(dplyr::across(base::is.numeric))
  )


# Creación de vectores ----
# Estos vectores son usados en diferentes apartados


# morfotipos-labels
labels_morfotipos <-
  c(
    "Acacia" = "*Acacia*",
    "Alnus" = "*Alnus*",
    "Alternanthera" = "*Alternanthera*",
    "Amaranthaceae" = "Amaranthaceae",
    "Ambrosia" = "*Ambrosia*",
    "Apiaceae" = "Apiaceae",
    "Arecaceae" = "Arecaceae",
    "Asteraceae: Anthemideae" = "Asteraceae: Anthemideae",
    "Asteraceae: Helianthus" = "Asteraceae: Helianthus",
    "Asteraceae: Lactuceae" = "Asteraceae: Lactuceae",
    "Asteraceae: Senecio" = "Asteraceae: Senecio",
    "Casuarina" = "*Casuarina*",
    "Cecropia" = "*Cecropia*",
    "Cupressaceae" = "Cupressaceae",
    "Cyperaceae" = "Cyperaceae",
    "Fraxinus" = "*Fraxinus*",
    "Juglans" = "*Juglans*",
    "Loranthaceae" = "Loranthaceae",
    "Moraceae" = "Moraceae",
    "Myrtaceae" = "Myrtaceae",
    "Pinus" = "*Pinus*",
    "Plantago" = "*Plantago*",
    "Poaceae" = "Poaceae",
    "Ricinus" = "*Ricinus*",
    "Rumex" = "*Rumex*",
    "Urticaceae" = "Urticaceae",
    "No identificado" = "Unidentified",
    "Total Pollen" = "Total Pollen"
  )


# top-ten-morfotipos
top_ten_morfotipos <- 
  c(
    "Arecaceae",
    "Cecropia",
    "Cupressaceae",
    "Fraxinus",
    "Moraceae",
    "Myrtaceae",
    "Pinus",
    "Poaceae",
    "Urticaceae",
    "Total Pollen"
  )


# top-ten-labels
labels_top_ten_morfotipos <- 
  c(
    "Arecaceae" = "Arecaceae",
    "Cecropia" = "*Cecropia*",
    "Cupressaceae" = "Cupressaceae",
    "Fraxinus" = "*Fraxinus*",
    "Moraceae" = "Moraceae",
    "Myrtaceae" = "Myrtaceae",
    "Pinus" = "*Pinus*",
    "Poaceae" = "Poaceae",
    "Urticaceae" = "Urticaceae",
    "Total Pollen" = "Total Pollen"
  )


# top-ten-order
order_top_ten_morfotipos <- 
  c(
    "Total Pollen",
    "Cecropia",
    "Urticaceae",
    "Fraxinus",
    "Cupressaceae",
    "Moraceae",
    "Poaceae",
    "Myrtaceae",
    "Pinus",
    "Arecaceae"
  )


# labels-months-letters
labels_month_letter <- 
  c("J",
    "F",
    "M",
    "A",
    "M",
    "J",
    "J",
    "A",
    "S",
    "O",
    "N",
    "D",
    "J")


# morfotipos
morfotipos <- c(
  "Acacia",
  "Alnus",
  "Alternanthera",
  "Amaranthaceae",
  "Ambrosia",
  "Apiaceae",
  "Arecaceae",
  "Asteraceae: Anthemideae",
  "Asteraceae: Helianthus",
  "Asteraceae: Lactuceae",
  "Asteraceae: Senecio",
  "Casuarina",
  "Cecropia",
  "Cupressaceae",
  "Cyperaceae",
  "Fraxinus",
  "Juglans",
  "Loranthaceae",
  "Moraceae",
  "Myrtaceae",
  "Pinus",
  "Plantago",
  "Poaceae",
  "Ricinus",
  "Rumex",
  "Urticaceae",
  "No identificado",
  "Total Pollen"
)


# Concentración horaria ----


# concentracion-data
concentracion_data <- 
  clean_conteo_total |> 
  concentration_calculator(
    correction_factor = 0.54
  )

dplyr::glimpse(concentracion_data)


# concentracion-1h
concentracion_1h <- 
  concentracion_data |> 
  tidyr::complete(
    datetime = base::seq.POSIXt(
      base::min(datetime),
      base::max(datetime),
      by = "hour"
    )
  )

dplyr::glimpse(concentracion_1h)


# guardar-concentracion-1h
# concentracion_1h |> 
#   readr::write_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "concentracion_1h.csv"
#     )
#   )


# cargar-concentracion-1h
# concentracion_1h <- 
#   readr::read_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "concentracion_1h.csv"
#     )
#   )


# concentracion-1h-long
concentracion_1h_long <-
  concentracion_1h |>
  dplyr::rename(
    from = datetime
  ) |>
  tidyr::pivot_longer(
    -from,
    names_to = "pollen",
    values_to = "value"
  ) |>
  dplyr::mutate(
    to = from + 3600,
    location = "Medellin",
    pollen = forcats::as_factor(pollen),
    location = forcats::as_factor(location)
  ) |>
  dplyr::select(
    pollen,
    location,
    from,
    to,
    value
  ) |> 
  rename_morphotypes(
    "pollen"
  ) #|> 
#stats::na.omit()

dplyr::glimpse(concentracion_1h_long)


# guardar-concentracion-1h-long
# concentracion_1h_long |> 
#   readr::write_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "concentracion_1h_long.csv"
#     )
#   )


# cargar-concentracion-1h-long
# concentracion_1h_long <- 
#   readr::read_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "concentracion_1h_long.csv"
#     )
#   )


# Concentración diaria ----


# concentracion-1d
concentracion_1d <- 
  concentracion_1h |> 
  stats::na.omit(
  ) |> 
  dplyr::group_by(
    date = lubridate::floor_date(datetime, "day")
  ) |> 
  dplyr::summarise(
    dplyr::across(
      dplyr::where(base::is.numeric), 
      ~ base::round(base::mean(.x, na.rm = TRUE))
    )
  ) |> 
  tidyr::complete(
    date = base::seq.POSIXt(
      base::min(date),
      base::max(date),
      by = "day"
    )
  ) |> 
  dplyr::mutate(
    date = lubridate::as_date(date)
  ) |> 
  dplyr::ungroup()

dplyr::glimpse(concentracion_1d)


# guardar-concentracion-1d
# concentracion_1d |> 
#   readr::write_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "concentracion_1d.csv"
#     )
#   )


# cargar-concentracion-1d
# concentracion_1d <- 
#   readr::read_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "concentracion_1d.csv"
#     )
#   )


# concentracion-1d-long
concentracion_1d_long <-
  concentracion_1d |>
  dplyr::rename(
    from = date
  ) |>
  tidyr::pivot_longer(
    -from,
    names_to = "pollen",
    values_to = "value"
  ) |>
  dplyr::mutate(
    to = from + 1,
    location = "Medellin",
    pollen = forcats::as_factor(pollen),
    location = forcats::as_factor(location)
  ) |>
  dplyr::select(
    pollen,
    location,
    from,
    to,
    value
  ) |> 
  rename_morphotypes(
    "pollen"
  ) #|> 
#stats::na.omit()


# guardar-concentracion-1d-long
# concentracion_1d_long |> 
#   readr::write_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "concentracion_1d_long.csv"
#     )
#   )


# cargar-concentracion-1d-long
# concentracion_1d_long <- 
#   readr::read_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "concentracion_1d_long.csv"
#     )
#   )


# Abundancia relativa ----


# relative-abundance
relative_abundance <- 
  concentracion_1d_long |> 
  dplyr::filter(
    pollen != "Total Pollen"
  ) |> 
  dplyr::group_by(
    pollen
  ) |>
  dplyr::summarise(
    value = base::sum(value, na.rm = TRUE)
  ) |> 
  dplyr::ungroup(
  ) |>
  dplyr::mutate(
    percent = scales::percent(value / base::sum(value), accuracy = 0.01),
    value = (value / base::sum(value)) * 100,
    pollen = forcats::fct_reorder(pollen, value)
  )


# fig-relative-abundance
fig_relative_abundance <- 
  relative_abundance |> 
  ggplot2::ggplot(
    ggplot2::aes(
      x = pollen,
      y = value,
      label = percent
    )
  ) +
  ggplot2::geom_bar(
    stat = "identity",
    fill = "#0072B2"
  ) +
  ggplot2::geom_text(
    hjust = -0.1
  ) +
  ggplot2::scale_x_discrete(
    labels = labels_morfotipos
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 50)
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    #title = "Relative Abundance in the Air",
    x = NULL, y = "Relative abundance (%)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 11),
    axis.title.y = ggtext::element_markdown(size = 11),
    axis.text.x = ggtext::element_markdown(size = 11),
    axis.text.y = ggtext::element_markdown(size = 11),
  )


# Comportamiento horario ----


## Preparación de los datos ----


# percent-1h
percent_morfotipos <- 
  concentracion_1h_long |>
  dplyr::filter(
    pollen != "Total Pollen"
  ) |> 
  dplyr::mutate(
    date = lubridate::as_date(from),
    hour = lubridate::hour(from)
  ) |>
  dplyr::group_by(
    date,
    pollen,
    location
  ) |>
  dplyr::mutate(
    percent = (value * 100) / base::mean(value)
  ) |>
  stats::na.omit() |>
  dplyr::ungroup() |>
  dplyr::mutate(
    percent = percent / base::length(base::unique(hour))
  ) |> 
  dplyr::select(
    pollen,
    from,
    date,
    hour,
    value,
    percent
  )


# percent-total-pollen
percent_total_pollen <- 
  percent_morfotipos |> 
  dplyr::group_by(
    from
  ) |> 
  dplyr::summarise(
    value = base::sum(value),
    percent = base::mean(percent)
  ) |> 
  dplyr::mutate(
    pollen = "Total Pollen",
    date = lubridate::as_date(from),
    hour = lubridate::hour(from)
  ) |> 
  dplyr::select(
    pollen,
    from,
    date,
    hour,
    value,
    percent
  )


# percent-todos
percent_todos <- 
  percent_morfotipos |> 
  dplyr::full_join(
    percent_total_pollen
  ) |> 
  dplyr::arrange(
    from
  )


## Sin barras de error ----


fig_hourly_pollen_concentration_top_ten_1 <- 
  percent_todos |> 
  dplyr::filter(
    pollen %in% top_ten_morfotipos
  ) |> 
  ggplot2::ggplot(
    ggplot2::aes(
      x = hour,
      y = percent,
      # fill = pollen
    )
  ) + 
  ggplot2::geom_bar(
    stat = "summary",
    fun = "mean",
    position = "dodge",
    fill = "#0072B2"
  ) + 
  ggplot2::scale_x_continuous(
    breaks = c(0, 4, 8, 12, 16, 20)
  ) +
  ggplot2::facet_wrap(
    ~ base::factor(pollen, order_top_ten_morfotipos),
    ncol = 2, 
    scales = "free_y",
    labeller = ggplot2::as_labeller(labels_top_ten_morfotipos)
  ) +
  ggplot2::labs(
    #title = "Hourly pollen concentration",
    x = "Hour of day",
    y = "Daily pollen (%)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 11),
    axis.title.y = ggtext::element_markdown(size = 11),
    axis.text = ggtext::element_markdown(size = 11),
    strip.text = ggtext::element_markdown(size = 11),
    legend.position = "right"
  )


## Con barras de error ----


fig_hourly_pollen_concentration_top_ten_2 <- 
  percent_todos |> 
  dplyr::filter(
    pollen %in% top_ten_morfotipos
  ) |> 
  ggplot2::ggplot(
    ggplot2::aes(
      x = hour,
      y = percent,
      # fill = pollen
    )
  ) + 
  ggplot2::geom_bar(
    stat = "summary",
    fun = "mean",
    position = "dodge",
    fill = "#0072B2"
  ) + 
  ggplot2::stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = ggplot2::position_dodge(width = 0.9),
    width = 0.3
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 4, 8, 12, 16, 20)
  ) +
  ggplot2::facet_wrap(
    ~ base::factor(pollen, order_top_ten_morfotipos),
    ncol = 2, 
    scales = "free_y",
    labeller = ggplot2::as_labeller(labels_top_ten_morfotipos)
  ) +
  ggplot2::labs(
    #title = "Hourly pollen concentration",
    x = "Hour of day",
    y = "Daily pollen (%)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 11),
    axis.title.y = ggtext::element_markdown(size = 11),
    axis.text = ggtext::element_markdown(size = 11),
    strip.text = ggtext::element_markdown(size = 11),
    legend.position = "right"
  )


# Comportamiento diario ----


## Con `ggplot2::geom_line` ----


# fig-daily-pollen-concentration-top-ten-1
fig_daily_pollen_concentration_top_ten_1 <- 
  concentracion_1d_long |>
  dplyr::filter(
    pollen %in% top_ten_morfotipos
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = lubridate::yday(from),
      y = value,
      color = as_factor(lubridate::year(from))
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(
    ~ base::factor(pollen, order_top_ten_morfotipos),
    ncol = 2, 
    scales = "free_y",
    labeller = ggplot2::as_labeller(labels_top_ten_morfotipos)
  ) + 
  ggplot2::scale_x_continuous(
    breaks = c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 367),
    labels = labels_month_letter
  ) + 
  ggplot2::labs(
    #title = "Daily pollen concentration",
    x = "Month of the year",
    y = "Pollen/m^3^",
    color = "Year"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 11),
    axis.title.y = ggtext::element_markdown(size = 11),
    axis.text = ggtext::element_markdown(size = 11),
    strip.text = ggtext::element_markdown(size = 11),
    legend.position = "bottom"
  ) +
  ggthemes::scale_color_colorblind()


## Con `ggplot2::geom_bar` ----


# fig-daily-pollen-concentration-top-ten-2
fig_daily_pollen_concentration_top_ten_2 <- 
  concentracion_1d_long |>
  dplyr::filter(
    pollen %in% top_ten_morfotipos
  ) |> 
  ggplot2::ggplot(
    ggplot2::aes(
      x = from,
      y = value,
      fill = as_factor(lubridate::year(from))
    )
  ) + 
  ggplot2::geom_bar(
    stat = "identity"
  ) + 
  ggplot2::scale_x_date(
    date_breaks = "3 month",
    date_minor_breaks = "1 month",
    date_labels = "%b %Y",
    # limits = c(
    #   lubridate::as_datetime("2019-06-20"), 
    #   lubridate::as_datetime("2022-09-11")
    # )
  ) +
  ggplot2::facet_wrap(
    ~ base::factor(pollen, order_top_ten_morfotipos),
    ncol = 2, 
    scales = "free_y",
    labeller = ggplot2::as_labeller(labels_top_ten_morfotipos)
  ) + 
  ggplot2::labs(
    #title = "Daily pollen concentration",
    x = "Month of the year",
    y = "Pollen/m^3^",
    fill = "Year"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 11),
    axis.title.y = ggtext::element_markdown(size = 11),
    axis.text = ggtext::element_markdown(size = 11),
    axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 8),
    strip.text = ggtext::element_markdown(size = 11),
    legend.position = "bottom"
  ) +
  ggthemes::scale_fill_colorblind()


## Con `ggplot2::geom_area` ----

# fig-daily-pollen-concentration-top-ten-3
fig_daily_pollen_concentration_top_ten_3 <- 
  concentracion_1d_long |>
  dplyr::mutate(
    from = lubridate::as_date(from)
  ) |> 
  tidyr::complete(
    from = seq.Date(min(from), max(from), by = "day")
  ) |> 
  dplyr::mutate(
    from = lubridate::as_date(from),
    yday = lubridate::yday(from),
    year = lubridate::year(from)
  ) |> 
  dplyr::group_by(
    pollen,
    yday
  ) |> 
  dplyr::mutate(
    mean = base::mean(value, na.rm = TRUE)
  ) |> 
  stats::na.omit() |> 
  dplyr::ungroup() |> 
  dplyr::filter(
    pollen %in% top_ten_morfotipos
  ) |>
  ggplot2::ggplot(
  ) + 
  ggplot2::geom_area(
    position = "identity",
    alpha = 0.3, ggplot2::aes(
      x = yday,
      y = value,
      fill = as_factor(year)
    )
  ) +
  ggplot2::geom_line(ggplot2::aes(x = yday, y = mean)) +
  ggplot2::facet_wrap(
    ~ base::factor(pollen, order_top_ten_morfotipos),
    ncol = 2, 
    scales = "free_y",
    labeller = ggplot2::as_labeller(labels_top_ten_morfotipos)
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 367),
    labels = labels_month_letter
  ) + 
  ggplot2::labs(
    #title = "Daily pollen concentration",
    x = "Month of the year",
    y = "Pollen/m^3^",
    fill = "Year"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 11),
    axis.title.y = ggtext::element_markdown(size = 11),
    axis.text = ggtext::element_markdown(size = 11),
    strip.text = ggtext::element_markdown(size = 11),
    legend.position = "bottom"
  ) +
  ggthemes::scale_color_colorblind()


## Media con `ggplot2::geom_line` ----


# fig-daily-pollen-concentration-top-ten-4
fig_daily_pollen_concentration_top_ten_4 <- 
  concentracion_1d_long |>
  dplyr::mutate(
    from = lubridate::as_date(from)
  ) |> 
  tidyr::complete(
    from = seq.Date(min(from), max(from), by = "day"),
    pollen
  ) |> 
  dplyr::mutate(
    from = lubridate::as_date(from),
    yday = lubridate::yday(from),
    year = lubridate::year(from)
  ) |> 
  dplyr::group_by(
    pollen,
    yday
  ) |> 
  dplyr::mutate(
    mean = base::mean(value, na.rm = TRUE)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(
    pollen %in% top_ten_morfotipos
  ) |>
  ggplot2::ggplot(
  ) +
  ggplot2::geom_line(ggplot2::aes(x = yday, y = mean)) +
  ggplot2::facet_wrap(
    ~ base::factor(pollen, order_top_ten_morfotipos),
    ncol = 2, 
    scales = "free_y",
    labeller = ggplot2::as_labeller(labels_top_ten_morfotipos)
  ) + 
  ggplot2::scale_x_continuous(
    breaks = c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 367),
    labels = labels_month_letter
  ) + 
  ggplot2::labs(
    #title = "Daily pollen concentration",
    x = "Month of the year",
    y = "Pollen/m^3^",
    fill = "Year"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 11),
    axis.title.y = ggtext::element_markdown(size = 11),
    axis.text = ggtext::element_markdown(size = 11),
    strip.text = ggtext::element_markdown(size = 11),
    legend.position = "bottom"
  ) +
  ggthemes::scale_color_colorblind()


# Main Pollen Season (MPS) ----


## MPS 2020-2022 ----


# mps-dates-2020-2022
mps_start_date_2020_2022 <- "2020-09-11"
mps_end_date_2020_2022 <- "2022-09-10"

# concentracion-mps-2020-2022
concentracion_mps_2020_2022 <-
  concentracion_1d |>
  dplyr::filter(
    between(
      date,
      lubridate::as_date(mps_start_date_2020_2022),
      lubridate::as_date(mps_end_date_2020_2022)
    )
  ) |> 
  tidyr::complete(
    date = base::seq(
      lubridate::as_date(mps_start_date_2020_2022),
      lubridate::as_date(mps_end_date_2020_2022),
      by = "day"
    )
  ) |>
  dplyr::rename(
    original_date = date # conserva la fecha original
  ) |>
  dplyr::mutate(
    date = base::seq.Date(
      from = lubridate::as_date("2021-01-01"),
      to = lubridate::as_date("2022-12-31"),
      by = "day"
    ),
    .before = acacia
  )


# pollen-season-2020-2022-calculate
pollen_season_2020_2022 <-
  concentracion_mps_2020_2022 |>
  dplyr::select(
    -original_date
  ) |>
  AeRobiology::calculate_ps(
    method = "percentage",
    perc = 90,
    def.season = "natural",
    plot = FALSE
  )


# pollen-season-2020-2022-modificate-date
pollen_season_2020_2022 <-
  pollen_season_2020_2022 |>
  dplyr::mutate(
    st.dt = dplyr::case_when(
      seasons == 2021 ~ lubridate::as_date(mps_start_date_2020_2022) + st.jd,
      seasons == 2022 ~ lubridate::as_date(mps_start_date_2020_2022) + 365 + st.jd,
    ),
    en.dt = dplyr::case_when(
      seasons == 2021 ~ lubridate::as_date(mps_start_date_2020_2022) + en.jd,
      seasons == 2022 ~ lubridate::as_date(mps_start_date_2020_2022) + 365 + en.jd,
    ),
    pk.dt = dplyr::case_when(
      seasons == 2021 ~ lubridate::as_date(mps_start_date_2020_2022) + pk.jd,
      seasons == 2022 ~ lubridate::as_date(mps_start_date_2020_2022) + 365 + pk.jd,
    ),
    seasons = dplyr::case_when(
      seasons == 2021 ~ "2020-2021",
      seasons == 2022 ~ "2021-2022"
    )
  )


# pollen-season-2020-2022-modificate-order
pollen_season_2020_2022 <-
  pollen_season_2020_2022 |>
  rename_morphotypes(
    type
  ) |> 
  dplyr::mutate(
    pollen = forcats::fct_relevel(
      pollen, morfotipos
    )
  ) |> 
  dplyr::arrange(
    pollen
  )


# guardar-pollen-season-2020-2022
# pollen_season_2020_2022 |> 
#   readr::write_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "pollen_season_2020_2022.csv"
#     )
#   )


# cargar-pollen-season-2020-2022
# pollen_season_2020_2022 <- 
#   readr::read_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "pollen_season_2020_2022.csv"
#     )
#   )


# tbl-pollen-season-2020-2022-top-ten
tbl_pollen_season_2020_2022_top_ten <- 
  pollen_season_2020_2022 |> 
  dplyr::filter(
    pollen %in% top_ten_morfotipos
  ) |> 
  dplyr::select(
    pollen,
    seasons,
    st.dt,
    en.dt,
    ln.ps,
    sm.ps,
    pk.val,
    pk.dt
  ) |> 
  dplyr::mutate(
    seasons = base::paste("Season", seasons)
  ) |> 
  gt::gt(
    # rowname_col = "pollen",
    groupname_col = "seasons",
  ) |> 
  gt::tab_stubhead(
    label = ""
  ) |> 
  gt::cols_label(
    pollen = "",
    st.dt = "Start date",
    en.dt = "End date",
    ln.ps = md("Season <br>length <br>(days)"),
    sm.ps = md("Season <br>Pollen <br>Integral <br>(Pollen * day/m^3^)"),
    pk.val = md("Peak <br>concentration <br>(Pollen/m^3^)"),
    pk.dt = "Peak date",
    .fun = md
  ) |> 
  gt_italic_pollen_types() |> 
  gt_theme_apa()


## MPS 2019-2020 ----

# mps-dates-2019
mps_start_date_2019 <- "2019-06-20"
mps_end_date_2019 <- "2020-06-19"


# concentracion-mps-2019
concentracion_mps_2019 <-
  concentracion_1d |>
  dplyr::filter(
    between(
      date,
      lubridate::as_date(mps_start_date_2019),
      lubridate::as_date(mps_end_date_2019)
    )
  ) |> 
  tidyr::complete(
    date = base::seq(
      lubridate::as_date(mps_start_date_2019),
      lubridate::as_date(mps_end_date_2019),
      by = "day"
    )
  ) |>
  dplyr::rename(
    original_date = date # conserva la fecha original
  ) |>
  dplyr::mutate(
    date = base::seq.Date(
      from = lubridate::as_date("2020-01-01"),
      to = lubridate::as_date("2020-12-31"),
      by = "day"
    ),
    .before = acacia
  )


# pollen-season-2019-calculate
pollen_season_2019 <-
  concentracion_mps_2019 |>
  dplyr::select(
    -original_date
  ) |>
  AeRobiology::calculate_ps(
    method = "percentage",
    perc = 90,
    def.season = "natural",
    plot = FALSE
  )


# pollen-season-2019-modificate-date
pollen_season_2019 <-
  pollen_season_2019 |>
  dplyr::mutate(
    st.dt = dplyr::case_when(
      seasons == 2020 ~ lubridate::as_date(mps_start_date_2019) + st.jd
    ),
    en.dt = dplyr::case_when(
      seasons == 2020 ~ lubridate::as_date(mps_start_date_2019) + en.jd
    ),
    pk.dt = dplyr::case_when(
      seasons == 2020 ~ lubridate::as_date(mps_start_date_2019) + pk.jd
    ),
    seasons = dplyr::case_when(
      seasons == 2020 ~ "2019-2020"
    )
  )


# pollen-season-2019-modificate-order
pollen_season_2019 <-
  pollen_season_2019 |>
  rename_morphotypes(
    type
  ) |> 
  dplyr::mutate(
    pollen = forcats::fct_relevel(
      pollen, morfotipos
    )
  ) |> 
  dplyr::arrange(
    pollen
  )

# guardar-pollen-2019-season
# pollen_season_2019 |> 
#   readr::write_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "pollen_season_2019.csv"
#     )
#   )


# cargar-pollen-season-2019
# pollen_season_2019 <- 
#   readr::read_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "pollen_season_2019.csv"
#     )
#   )


# tbl-pollen-season-top-ten-2020
tbl_pollen_season_2019_top_ten <- 
  pollen_season_2019 |> 
  dplyr::filter(
    pollen %in% top_ten_morfotipos
  ) |> 
  dplyr::select(
    pollen,
    seasons,
    st.dt,
    en.dt,
    ln.ps,
    sm.ps,
    pk.val,
    pk.dt
  ) |> 
  dplyr::mutate(
    seasons = base::paste("Season", seasons)
  ) |> 
  gt::gt(
    # rowname_col = "pollen",
    groupname_col = "seasons",
  ) |> 
  gt::tab_stubhead(
    label = ""
  ) |> 
  gt::cols_label(
    pollen = "",
    st.dt = "Start date",
    en.dt = "End date",
    ln.ps = md("Season <br>length <br>(days)"),
    sm.ps = md("Season <br>Pollen <br>Integral <br>(Pollen * day/m^3^)"),
    pk.val = md("Peak <br>concentration <br>(Pollen/m^3^)"),
    pk.dt = "Peak date",
    .fun = md
  ) |> 
  gt_italic_pollen_types() |> 
  gt_theme_apa()


## MPS 2019-2022 ----


# pollen-season-2019-2022
pollen_season_2019_2022 <- 
  pollen_season_2019 |> 
  dplyr::full_join(
    pollen_season_2020_2022
  ) |> 
  dplyr::mutate(
    pollen = forcats::fct_relevel(
      pollen, morfotipos
    )
  ) |> 
  dplyr::arrange(
    pollen
  )


# guardar-pollen-season-2019-2022
# pollen_season_2019_2022 |> 
#   readr::write_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "pollen_season_2019_2022.csv"
#     )
#   )


# cargar-pollen-season-2019-2022
# pollen_season_2019_2022 <- 
#   readr::read_csv(
#     here::here(
#       "aerobiologia-medellin",
#       "output-data",
#       "pollen_season_2019_2022.csv"
#     )
#   )


# tbl-pollen-season-2019-2022-top-ten
tbl_pollen_season_2019_2022_top_ten <- 
  pollen_season_2019_2022 |> 
  dplyr::filter(
    pollen %in% top_ten_morfotipos
  ) |> 
  dplyr::select(
    pollen,
    seasons,
    st.dt,
    en.dt,
    ln.ps,
    sm.ps,
    pk.val,
    pk.dt
  ) |> 
  dplyr::mutate(
    seasons = base::paste("Season", seasons)
  ) |> 
  gt::gt(
    # rowname_col = "pollen",
    groupname_col = "seasons",
  ) |> 
  gt::tab_stubhead(
    label = ""
  ) |> 
  gt::cols_label(
    pollen = "",
    st.dt = "Start date",
    en.dt = "End date",
    ln.ps = md("Season <br>length <br>(days)"),
    sm.ps = md("Season <br>Pollen <br>Integral <br>(Pollen * day/m^3^)"),
    pk.val = md("Peak <br>concentration <br>(Pollen/m^3^)"),
    pk.dt = "Peak date",
    .fun = md
  ) |> 
  gt_italic_pollen_types() |> 
  gt_theme_apa()


# Annual Pollen Integral (APIn) ----


# annual-pollen-integral
annual_pollen_integral <- 
  concentracion_1d_long |> 
  dplyr::mutate(
    from = lubridate::as_date(from),
    season = dplyr::case_when(
      dplyr::between(
        from, lubridate::as_date("2019-06-20"), lubridate::as_date("2020-03-19")
      ) ~ "2019-2020",
      dplyr::between(
        from, lubridate::as_date("2020-09-10"), lubridate::as_date("2021-09-10")
      ) ~ "2020-2021",
      dplyr::between(
        from, lubridate::as_date("2021-09-11"), lubridate::as_date("2022-09-11")
      ) ~ "2021-2022"
    )
  ) |> 
  dplyr::group_by(
    season,
    pollen
  ) |>
  dplyr::summarise(
    value = base::sum(value, na.rm = TRUE)
  ) |> 
  stats::na.omit() |> 
  dplyr::ungroup()


# fig-annual-pollen-integral-top-ten
fig_annual_pollen_integral_top_ten <- 
  annual_pollen_integral |> 
  dplyr::filter(
    pollen %in% top_ten_morfotipos
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = season,
      y = value,
      #fill = forcats::as_factor(season)
    )
  ) + 
  ggplot2::geom_bar(
    stat = "identity",
    fill = "#0072B2"
  ) + 
  ggplot2::facet_wrap(
    ~ base::factor(pollen, order_top_ten_morfotipos),
    ncol = 2, 
    scales = "free_y",
    labeller = ggplot2::as_labeller(labels_top_ten_morfotipos)
  ) + 
  ggplot2::labs(
    #title = "Annual Pollen Integral",
    x = "Season",
    y = "Pollen * day/m^3^",
    fill = "Year"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggtext::element_markdown(size = 11),
    axis.title.y = ggtext::element_markdown(size = 11),
    axis.text = ggtext::element_markdown(size = 11),
    strip.text = ggtext::element_markdown(size = 11),
    legend.position = "right"
  ) +
  ggthemes::scale_color_colorblind()
