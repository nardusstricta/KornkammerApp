library(shiny)
library(DT)
library(tidyverse)
bilanz <<- read_csv("buchhaltung.csv")
mitglieder <<- read_csv("mitglieder.csv")
produckte <<- read_csv("produckt_info.csv")

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

