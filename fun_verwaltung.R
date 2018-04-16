library(shiny)
library(DT)
library(tidyverse)
library(lubridate)

##
###Funktionen Für die Mitgliederverwaltung:

#Funktion welche aus einem Jahreseintrag 12 Jahreseinträge macht:
#
expand_mitglieder <- function(MitgliederX){
  last_entry_exp <- MitgliederX %>% 
    group_by(Name) %>% # gruppierung nach den Namen
    filter(Datum == max(Datum) & Anzahl_Personen > 0) %>% ## letztes Datum auswählen (Dez) um die letzten Aktuellen Daten zu übernehmen + Kündigungen nicht übernehmen.
    expand( # für jeden Account werden neue Zeilen angfügt, bis zum Dezember des laufenden Jahres.
      Anzahl_Personen, Mitgliedsnummer, E_Mail, Forderung_mtl, 
      Datum = seq(
        from = date(max(Datum)), 
        to = lubridate::ymd(paste0( year(Sys.Date()), "-", "12-","01")), 
        by = "month"
      )
    )
  MitgliederNeu <- dplyr::union(MitgliederX, last_entry_exp)
  return(MitgliederNeu)
}

get_mitglieder <- function(MitgliederX, NameY, DateZ){
  mit2 <- MitgliederX %>% 
    filter(Name == NameY) %>% 
    filter(year(Datum) == DateZ) %>% 
    arrange(Datum) %>% 
    mutate(
      Monat = paste0(lubridate::month(Datum, label = T), " ", lubridate::year(Datum))
    ) %>% 
    select(Monat, Anzahl_Personen, Mitgliedsnummer, Name, E_Mail, Forderung_mtl, Datum)
  return(mit2)
}

