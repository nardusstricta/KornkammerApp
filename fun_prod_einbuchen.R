# Funktionen für das Einbuchen der Produckte:

## Funktion, die den neu eingebuchten Produkten die Preis_ID gibt:
fun_produkt_count <- function(ProduckteX, NameY){
  erg <- ProduckteX %>% 
    filter(Name == NameY) %>% 
    summarise(max_ID = max(Preis_ID))
  return(erg + 1)
} 