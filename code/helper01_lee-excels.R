# Lectura de los archivos donde se almacena la información descargada de TarSys
# Nota: la información se almacena en libros de EXCEL


# Librerías y Variables ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(readxl)

tarsys <- list(
  ruta   = "\\\\bender\\departamento\\Tarsys",
  patron = "Mensuales"
)


# funciones propias de este proceso
source(file = here("code", "helper01_funs.R"))


# Acceso a los datos ------------------------------------------------------

# lista de archivos a procesar
archivos_tarsys <-
  filtrar_archivos(
    ruta = tarsys$ruta,
    patron = tarsys$patron
  )

# datos
ds_tarsys <- 
  extrae_datos(archivos_tarsys) %>% 
  select(fecha, planta, ends_with("kw")) %>% 
  # limpieza de datos
  # los números aparecen como character, debido a que no ha funcionado skip=2 en read_xls
  # y pilla la fila de cabecera como si fueran datos
  # 1o quito las cabeceras de los datos originales
  filter(
    planta != "Planta",
    !is.na(planta)
  ) %>% 
  mutate(
    across(ends_with("kw"), as.double)
  )


# Guardo los datos --------------------------------------------------------

save(ds_tarsys, file = here("data", "tarsys.RData"))
