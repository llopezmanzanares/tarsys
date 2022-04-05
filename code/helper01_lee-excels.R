# Lectura de los archivos donde se almacena la información descargada de TarSys
# Nota: la información se almacena en libros de EXCEL


# Librerias y Variables ---------------------------------------------------

library(tidyverse)
library(readxl)

ruta_tarsys <- "\\\\bender\\departamento\\Tarsys"
patron_tarsys <- "Mensuales"


# Acceso a los archivos ---------------------------------------------------

# lista de archivos a procesar
archivos_tarsys <-
  list.files(
    path = ruta_tarsys,
    pattern = patron_tarsys,
    full.names = T
  ) %>% 
  file.info() %>% 
  as_tibble(
    rownames = "archivo"
  ) %>%
  # solo me interesa la fecha de creación para poder quedarme con el último de cada anualidad
  select(archivo, ctime)
