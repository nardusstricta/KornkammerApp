# Funktionen für den Warenbestand

## Funktion, die den komlpetten Warenbestand eines Produktes filtert.
fun_war <- function(BilanzX, NameY, ProduckteZ){
  erg_war <- BilanzX %>% 
    filter(Name == NameY) %>% 
    left_join(
      select(ProduckteZ, Preis_ID, Name, Preis, Einheit), 
      by = c("Name", "Preis_ID")
    ) %>% # hier wird der Preis angefügt
    mutate(Soll = Soll / Preis, Haben = Haben /Preis) %>% 
    arrange(Datum) %>% 
    mutate(cumsoll =  cumsum(Soll) - cumsum(Haben)) %>% 
    select(Datum, cumsoll, Einheit)
  return(erg_war)
}

fun_war_all <- function(BilanzX, ProduckteZ){
  erg_war <- BilanzX %>% 
    filter(Name %in% ProduckteZ$Name) %>% 
    left_join(
      select(ProduckteZ, Preis_ID, Name, Preis, Einheit), 
      by = c("Name", "Preis_ID")
    ) %>% # hier wird der Preis angefügt
    mutate(Soll = Soll / Preis, Haben = Haben /Preis) %>% 
    group_by(Name) %>% 
    summarise(
      MengeProEinheit = round(sum(Soll)-sum(Haben),2),
      maxSoll = max(Soll),
      Einheit = unique(Einheit)
    ) %>% 
    mutate(relativeDa = 100* maxSoll / MengeProEinheit) %>% 
    mutate(color = ifelse(relativeDa < 3, "red", ifelse(relativeDa > 20, "lightgreen", "orange")))
  return(erg_war)
}
