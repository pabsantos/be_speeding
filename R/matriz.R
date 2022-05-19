filter_matriz <- function(taz) {
  reg_matriz <- c(
    "AHU", "CABRAL", "JUVEVE", "HUGO LANGE", "JARDIM SOCIAL", "ALTO DA RUA XV",
    "ALTO DA GLORIA", "CENTRO CIVICO", "BOM RETIRO", "SAO FRANCISCO", "MERCES",
    "BIGORILHO", "BATEL", "REBOUCAS", "PRADO VELHO", "JARDIM BOTANICO", 
    "CRISTO REI", "CENTRO"
  )
  taz %>% 
    filter(neigh %in% reg_matriz) %>% 
    rename(SP = SPEEDING)
}
