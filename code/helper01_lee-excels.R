# Lectura de los archivos donde se almacena la información descargada de TarSys
# Nota: la información se almacena en libros de EXCEL


# Librerias y Variables ---------------------------------------------------

library(tidyverse)
library(readxl)

ruta_tarsys <- "\\\\bender\\departamento\\Tarsys"
patron_tarsys <- "Mensuales"


# Funciones ---------------------------------------------------------------

# Filtro los archivos almacenados en el directorio remoto Tarsys y me quedo
# con los que contienen información anual de las instalaciones.
# Estos son archivos EXCEL con una hoja por mes con los datos de los contadores
#
# Inputs:
#   - ruta hasta los archivos
#   - patrón para descargar sólo los de los datos de los contadores
#
# Outputs:
#   - lista de archivos a procesar, son los últimos de cada anualidad

filtrar_archivos <- function(ruta, patron){
  archivos <-
    list.files(
      path = ruta,
      pattern = patron,
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
    ) %>% 
    ungroup() %>% 
    select(value)
  
  return(archivos)
}

# Acceso a los archivos ---------------------------------------------------

# lista de archivos a procesar
archivos_tarsys <-
  filtrar_archivos(
    ruta = ruta_tarsys,
    patron = patron_tarsys
  )

