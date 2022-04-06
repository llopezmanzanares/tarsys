# Funciones empleadas en el script helper01_lee-excels

# Filtro los archivos almacenados en el directorio remoto Tarsys y me quedo
# con los que contienen información anual de las instalaciones.
# Estos son archivos EXCEL con una hoja por mes con los datos de los contadores
#
# Inputs:
#   - ruta hasta los archivos
#   - patrón para descargar sólo los de los datos de los contadores
#
# Outputs:
#   - lista de archivos a procesar, son los últimos de cada mensualidad

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
      anualidad = str_extract(aaaa, pattern = "\\d+$") %>% as.integer(),
      mes = str_extract(mes, pattern = "\\d+") %>% as.integer()
    ) %>% 
    group_by(anualidad, mes) %>% 
    # filtro la última versión de cada mes
    filter(
      version == max(version)
    ) %>% 
    ungroup() %>% 
    select(archivo = value, anualidad, mes)
  
  return(archivos)
}

# Extraigo los datos de la lista de archivos obtenida con la función filtrar_archivos
#
# Inputs:
#   - dataset con la lista de archivos, año y mes de los datos
#       * el 1er elemento es la ruta de los archivos
#       * el 2o  elemento es la anualidad del dato
#       * el 3er elemento es la mensualidad del dato
#
# Outputs:
#   - dataset con los valores de cada uno de los excel leídos, incluyendo fecha del dato

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