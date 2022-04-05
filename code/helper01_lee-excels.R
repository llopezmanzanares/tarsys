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
  as_tibble() %>%
  # el nombre del archivo contiene información de la anualidad y mensualidad
  separate(
    col = value,
    into = c("aaaa", "periodicidad", "mes", "version", "tipo_archivo"),
    sep = "\\.",
    remove = F
  ) %>% 
  mutate(
    mes = str_extract(mes, pattern = "\\d+") %>% as.integer()
  ) %>% 
  group_by(aaaa) %>% 
  # filtro para el máximo mes de cada anualidad
  filter(
    mes == max(mes)
  ) %>% 
  # y posteriormente, para la última versión
  filter(
    version == max(version)
  )

