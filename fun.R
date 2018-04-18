

## Funktion, die die aktuelle Preis_Id berechnet, mit der man das Produkt einkauft.
get_preis_ID2 <- function(BilanzX, NameY){ 
  preis_ID <- BilanzX %>%
    filter(Name == NameY & Verwendung == "Verkauf") %>% 
    summarise(z = max(Preis_ID))## aktuelle Preis_ID
  #Wenn noch kein Verkauf stattgefunden hat dann ist preis_ID = -inf dann soll die minimale Preis_ID genommen werden:
  if(preis_ID == -Inf){
      preis_ID <- BilanzX %>%
      filter(Name == NameY) %>% 
      summarise(z = min(Preis_ID))
  }
  
  ## hier noch eine Warnmeldung einbauen, für den Fall, dass unter Reis andere Verwendungen gebucht wurden als Verkauf und Wareneinkauf.
  if(unique(filter(BilanzX, Name == NameY)$Verwendung) %in% c("Verkauf", "Wareneinkauf")){
    warning("Achtung ")
  }
    
  AktuelleID <- BilanzX %>% 
    filter(Name == NameY & Preis_ID == preis_ID$z) %>% 
    summarise(Haben = sum(Haben)-sum(Soll))

  if(AktuelleID >= 0){ ## wenn T, dann muss neue ID vergeben werden
    aktuellePreis_ID <- preis_ID$z + 1
  } else {
    aktuellePreis_ID <- preis_ID$z
  }
  return(aktuellePreis_ID)
}


## Reingewinn geht aktuell nur mit get_cur_price2 ID
get_cur_price2 <- function(ProduckteX, NameY1, BilanzX1, ID = F){
  preis_IDZ <- get_preis_ID2(BilanzX = BilanzX1, NameY = NameY1)
  cur_price <- ProduckteX %>%
    filter(Name == NameY1 & Preis_ID == preis_IDZ)
  
  if(length(cur_price$Preis_ID)==0){
    cur_price <- ProduckteX %>%
      filter(Name == NameY1 & Preis_ID == preis_IDZ - 1)
    ifelse(ID == T, return(cur_price$Preis_ID), return(cur_price$Preis))
    
  }else{
    ifelse(ID == T, return(cur_price$Preis_ID), return(cur_price$Preis))
  }
}


get_cur_Lieferant <- function(ProduckteX, NameY1, BilanzX1){
  preis_IDZ <- get_preis_ID2(BilanzX = BilanzX1, NameY = NameY1)
  cur_price <- ProduckteX %>%
  filter(Name == NameY1 & Preis_ID == preis_IDZ)
  return(cur_price$Lieferant)
}

#Funktion eigener Kontostand: ## muss noch erweitert werden (-Einlage - Mitgliederbeiträge)
fun_kont <- function(BilanzX, NameY){
    BilanzX %>% 
    filter(Name == NameY & Verwendung %in% c("Einkauf", "Einzahlung")) %>% 
    arrange(Datum) %>% 
    mutate(cumsoll =  cumsum(Haben) - cumsum(Soll)) %>% 
    summarise(cumsoll = last(cumsoll))
}

#Funktion für die Einheiten:
fun_einh <- function(ProduckteX, NameY){
  ProduckteX %>% 
  distinct(Name, Einheit) %>% 
  filter(Name==NameY) %>% 
  select(Einheit)
}




