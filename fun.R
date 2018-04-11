library(shiny)
library(DT)
library(tidyverse)
bilanz <<- read_csv("buchhaltung.csv")
mitglieder <<- read_csv("mitglieder.csv")
produckte <<- read_csv("produckt_info.csv")
#str(bilanz)
#erstellung der Preis_ID immer wenn die Daten neu geladen werden.

get_preis_ID <- function(BilanzX, NameY){
  preis_ID <- BilanzX %>%
    arrange(Datum) %>% 
    #group_by(Name == NameY) %>% 
    filter(Name == NameY) %>% 
    mutate(cumsoll = cumsum(Haben-Soll)) %>% 
    mutate(Preis_ID = 1) %>% 
    mutate(
      Preis_ID = ifelse(lag(cumsoll) < 0 & cumsoll >= 0, lag(Preis_ID) + 1, 1)
    ) %>% ## hier habe ich für die else-bedingung statt der 1 das lag(Preis_ID) hingeschrieben!
    replace_na(list(Preis_ID = 1)) %>% 
    mutate(Preis_ID = cumprod(Preis_I(D))  %>% 
    summarise(Preis_ID = last(Preis_ID))
  return(preis_ID)
}

## Funktion, die die aktuelle Preis_Id berechnet, mit der man das Produkt einkauft.
get_preis_ID2 <- function(BilanzX, NameY){ 
  preis_ID <- BilanzX %>%
    filter(Name == NameY & Verwendung == "Verkauf") %>% 
    summarise(z = max(Preis_ID))## aktuelle Preis_ID
  
  ## hier noch eine Warnmeldung einbauen, für den Fall, das unter Reis andere Verwendungen gebucht wurden als Verkauf und Wareneinkauf.
  if(unique(filter(BilanzX, Name == NameY)$Verwendung) %in% c("Verkauf", "Wareneinkauf")){
    warning("Achtung ")
  }
    
  AktuelleID <- BilanzX %>% 
    filter(Name == NameY & Preis_ID == preis_ID$z) %>% 
    #group_by(Verwendung) %>% 
    summarise(Haben = sum(Haben)-sum(Soll))
    #summarise(HabenAktuelleID = sum(Haben), SollAktuelleID = sum(Soll))
  
  if(AktuelleID > 0){ ## wenn T, dann muss neue ID vergeben werden
    aktuellePreis_ID <- z + 1
  } else {
    aktuellePreis_ID <- z
  }
  return(aktuellePreis_ID)
}

preis_IDZ <- get_preis_ID2(BilanzX = bilanz, NameY = "Reis")

get_cur_price2 <- function(ProduckteX, NameY, preis_IDZ){
  cur_price <- ProduckteX %>%
    filter(Name == NameY & Preis_ID == preis_IDZ)
  return(cur_price$Preis)
}


get_cur_price <- function(ProduckteX, NameY, preis_IDZ){
  cur_price <- ProduckteX %>%
    filter(Name == NameY & Preis_ID == preis_IDZ)
  return(cur_price$Preis)
}

get_cur_price(ProduckteX = produckte, NameY = "Reis", unlist(get_preis_ID(BilanzX = bilanz, NameY = "Reis")))
#Funktion eigener Kontostand:

fun_kont <- function(BilanzX, NameY){
    BilanzX %>% 
    filter(Name == NameY) %>% 
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

