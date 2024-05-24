# Proyecto aerobiologia congreso ----
## Paquetes y funciones ----

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("here")) install.packages("here")
if (!require("janitor")) install.packages("janitor")

here::here()

## Cargar funciones propias---- 

source(here::here("scripts","funciones", "import_data.R"))
source(here::here("scripts","funciones", "clear_data_2.R"))
source(here::here("scripts","funciones", "concentration_calculator.R"))
source(here::here("scripts","funciones", "rename_morphotypes.R"))

