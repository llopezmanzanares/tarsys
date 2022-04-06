# Lectura de los archivos donde se almacena la información descargada de TarSys
# Nota: la información se almacena en libros de EXCEL


# Librerias y Variables ---------------------------------------------------

library(tidyverse)
library(lubridate)
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
    group_by(aaaa, mes) %>% 
    # filtro la última versión de cada mes
    filter(
      version == max(version)
    ) %>% 
    ungroup() %>% 
    select(value)
  
  return(archivos)
}

extrae_datos <- function(archivos_lst) {
  # contenedor de los datos
  ds_all <- tibble()
  
  for (i in seq_along(archivos_lst[[1]])) {
    ds <-
      read_xls(
        path = archivos_lst[[i,1]],
        sheet = as.numeric(archivos_lst[[i,3]]),
        range = cell_cols(c(2:8)),
        col_names = c("planta", "dummy1", "dummy2", "reac_gen_kw", "reac_cons_kw", "consumo_kw", "generacion_kw"),
        skip = 2
      ) %>% 
      mutate(
        anualidad = archivos_lst[[i,2]],
        mes = archivos_lst[[i,3]],
        fecha = ym(str_c(anualidad, mes, sep = "-")) %>% 
          rollforward()
      )
    ds_all <- bind_rows(ds_all, ds)
    rm(ds)
    
  }
  
  return(ds_all)
}

# Acceso a los datos ------------------------------------------------------

# lista de archivos a procesar
archivos_tarsys <-
  filtrar_archivos(
    ruta = ruta_tarsys,
    patron = patron_tarsys
  ) %>% 
  # añado mes y año
  mutate(
    archivo = value,
    anualidad = str_extract(archivo, pattern = "\\d+"),
    mes = str_sub(archivo, start = 46, end = 47),
    .keep = "none"
  )

ds_tarsys <- tibble()
for (i in seq_along(archivos_tarsys$archivo)) {
 ds <-
  read_xls(
    path = archivos_tarsys$archivo[i],
    sheet = as.numeric(archivos_tarsys$mes[i]),
    range = cell_cols(c(2:8)),
    col_names = c("planta", "dummy1", "dummy2", "reac_gen_kw", "reac_cons_kw", "consumo_kw", "generacion_kw"),
    skip = 2
  ) %>% 
  mutate(
    anualidad = archivos_tarsys$anualidad[i],
    mes = archivos_tarsys$mes[i],
    fecha = lubridate::ym(str_c(anualidad, mes, sep = "-")) %>% 
      lubridate::rollforward()
    
  )
 ds_tarsys <- bind_rows(ds_tarsys, ds)
 rm(ds)
}
# filtro algunos datos que se han colado, no funciona skip=2
ds_tarsys <-
  ds_tarsys %>% 
  filter(
    !is.na(planta),
    planta != "Planta"
  ) %>% 
  select(fecha, planta, ends_with("kw")) %>% 
  mutate(
    across(ends_with("kw"), as.double)
  ) %>% 
  # al transformar en número hay una fila con valor "---------"
  filter(
    !is.na(generacion_kw)
  )
  
cosa <- ds_tarsys %>%   
  # mutate(
  #   fecha = lubridate::ym(str_c(anualidad, mes, sep = "-")) %>% 
  #     lubridate::rollforward()
  # ) %>% 
  select(fecha, planta, ends_with("kw")) %>% 
  mutate(
    across(ends_with("kw"), as.double)
  ) %>% 
  filter(
    !is.na(generacion_kw)
  )




# leo todas las hojas, pero no puedo añadir información sobre cada una de las hojas para
# conocer el año-mes del dato
ruta <- archivos_tarsys$value[1]
ds_tarsys_2018 <-
  excel_sheets(ruta) %>% 
#  set_names() %>% 
  map_dfr(read_xls, path = ruta)
