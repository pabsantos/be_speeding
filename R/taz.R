load_taz <- function() {
  taz <- st_read("data/taz.gpkg") %>% 
    rename(id_taz = cod_taz, neigh = BAIRRO) %>% 
    mutate(id_taz = as.character(id_taz))
}
