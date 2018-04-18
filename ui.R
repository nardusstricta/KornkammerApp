library(rhandsontable)
library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
library(rdrop2)

token <<- readRDS("token.rds")
drop_acc(dtoken = token)

bilanz_imp <- drop_read_csv("buchhaltung.csv",colClasses = c("Date",  "character", "numeric", "numeric", "character","integer","integer"))

mitglieder_imp <- drop_read_csv("mitglieder.csv", colClasses = c("integer", "integer", "character", 
                                                                  "character", "integer", 
                                                                  "Date"
                                                                  ))
produckte_imp <<- drop_read_csv("produckt.csv",colClasses = c("integer", "character", "character", "numeric", "character", "Date"), sep = ",")

write.table(bilanz_imp, "buchhaltung.csv", sep = ",", col.names = T, append = F, row.names = F)
write.table(mitglieder_imp , "mitglieder.csv", sep = ",", col.names = T, append = F, row.names = F)
write.table(produckte_imp, "produckt.csv", sep = ",", col.names = T, append = F, row.names = F)

bilanz <<- read_csv("buchhaltung.csv")
mitglieder <<- read_csv("mitglieder.csv", 
  col_types = cols(Datum = col_date(format = "%Y-%m-%d"))
)
produckte <<- read_csv("produckt.csv")
## Bei Jahreswechsel werden automatisch für jeden Account neue Zeilen erstellt, für jeden Monat.
## !!! Achtung! wenn schon händisch eine Zeile des neuen Jahres eingetragen wurde, bevor diese Funktion abgeschickt wurde, wird das nicht mehr passieren!
if(lubridate::year(max(mitglieder$Datum)) != year(Sys.Date())){
  mitglieder_neu <- expand_mitglieder(mitglieder)
  write_csv(mitglieder_neu, "mitglieder.csv")
  mitglieder <<- read_csv("mitglieder.csv", 
                          col_types = cols(Datum = col_date(format = "%Y-%m-%d")))
}




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
  
  if(AktuelleID > 0){ ## wenn T, dann muss neue ID vergeben werden
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

## Funktion, die den neu eingebuchten Produkten die Preis_ID gibt:
fun_produkt_count <- function(ProduckteX, NameY){
  erg <- ProduckteX %>% 
    filter(Name == NameY) %>% 
    summarise(max_ID = max(Preis_ID))
  return(erg + 1)
} 

## Funktion, die den komlpetten Warenbestand eines Produktes filtert.
fun_war <- function(BilanzX, NameY, ProduckteZ){
  erg_war <- BilanzX %>% 
    filter(Name == NameY) %>% 
    arrange(Datum) %>% 
    mutate(cumsoll =  cumsum(Soll) - cumsum(Haben)) %>% 
    select(Datum, cumsoll)
  return(erg_war)
}

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

#
#Persönliche Rechnung:
#

#Funktion welche den aktuellen zubezahlenden Mitglederbeitrag in die Bilanz Tabelle schreibt:

fun_pers_replace <- function(BilanzX, sum_sollY, DatumZ, NameA){
  erg_mit2 <- BilanzX %>%
    mutate(Soll = replace(Soll, Name == NameA & year(Datum) == DatumZ & Verwendung == "Mitgliedsbeitrag" & Soll > 0, sum_sollY)) 
  return(erg_mit2)
}

per_bilanz_fun <- function(BilanzX, NameY, DatumZ){
  erg <- BilanzX %>% 
    filter(Name == NameY & year(Datum) == DatumZ) %>%
    arrange(Datum) %>% 
    group_by(Datum, Verwendung) %>% 
    summarize(Soll = sum(Soll), Haben = sum(Haben))
  return(erg)
}

kk_bilanz_fun <- function(BilanzX, DatumZ){
  erg2 <- BilanzX %>% 
    filter(year(Datum) == DatumZ) %>%
    arrange(Datum) %>% 
    group_by(Datum, Verwendung) %>% 
    summarize(Soll = sum(Soll), Haben = sum(Haben))
  return(erg2)
}
########################################

shinyUI(fluidPage(
  tabsetPanel(
  tabPanel("Einkaufen",
  sidebarLayout(
      wellPanel(splitLayout(selectInput(inputId = 'Name', label = 'Vorname', 
                                        choices = c("Konto",  unique(mitglieder$Name))), 
                            dateInput("date1", "Bestelldatum:", value = Sys.Date())),
                splitLayout(selectInput(inputId = "produckt", label = "Produckt", 
                                        choices = unique(c("Auswahl",  produckte$Name)), selectize=F),
                  numericInput("menge", "Menge", min = 1, max = 1000, value = 0),
                            textOutput("einheit"), textOutput("preis_pro_gewicht")),
splitLayout(
  actionButton("best", "In den Warenkorb"),
  actionButton("deleteRows", "Aus den Warenkorb"),
  textOutput("Kontostand"),
  textOutput("Lieferant")
    )),
    mainPanel(
      actionButton("kaufen", 'Kaufen', style = "color: black;background-color: yellow"),
      strong("Dein lieber Warenkorb:"),
      dataTableOutput("table1")
    )
  )
),
tabPanel("Produckte Importiern",
         sidebarLayout(
           wellPanel(
             splitLayout(
             selectInput("inp_name", "Produckt", choices = c("Auswahl", unique(produckte$Name)),  selectize=F),
             selectInput("inp_lieferant", "Lieferant", choices = c("Auswahl", unique(produckte$Lieferant)),  selectize=F),
             numericInput("inp_preis", "Gesamtpreis", min = 1, max = 1000, value = 0)),
             splitLayout(
             selectInput("inp_einheit", "Einheit", choices = c("kg", "g", "l"),  selectize=F),
             numericInput("inp_menge", "Gesamtmenge", value = 0),
             numericInput("inp_rech_ID", "Rechnungs_ID", min = 1, max = 1000, value = 0),
             dateInput("inp_date", "Datum Wareneingang", value = Sys.Date())
             ),
             splitLayout(
               actionButton("inp_merk", "Auf die Liste"),
               actionButton("inp_delrows", "löschen von der Liste"),
               textOutput("inp_wert"),
               textOutput("inp_preis_pro_einheit")
             )
             ),
           
           mainPanel(
             actionButton("inp_save", 'speichern', style = "color: black;background-color: yellow"),
             dataTableOutput("table2")
           )
         )
),
tabPanel("Konotauszug importiern",
         sidebarLayout(
           wellPanel(
         fileInput("import", "Load Model Data",
                   accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"),
                   buttonLabel = "Suchen"),
         actionButton("save_gls", 'speichern', style = "color: white;background-color: red")
         ),
         mainPanel(
           rHandsontableOutput("hot")
         ))),
tabPanel("Verwaltung",
         sidebarPanel(
           wellPanel(strong("Hier kannst du deine Kontoauszüge einsehen und als pdf speichern, Informationen über deine Mitgliedschaft ändern (z.B. wenn sich die Anzahl der Mitglieder in deiner Mitgliedschaft ändert) und die Jahresbilanz der Kornkammer einsehen."),
             splitLayout(
           selectInput("ver_name", "Name", choices = c("Auswahl", unique(mitglieder$Name))),
           selectInput("ver_date", "Jahr", choices = 2015:2025)),
           actionButton("ver_bearbeiten", 'bearbeiten', style = "color: white;background-color: red"),
           actionButton("ver_save", 'speichern', style = "color: black;background-color: yellow"), 
           downloadButton('pers_rechn', label="Eingene Rechnung"),
           downloadButton('kk_rechn', label="KK Jahresbilanz")
         )),mainPanel(
           DTOutput('x1')
         )
),
tabPanel("aktueller Warenstand",
         titlePanel(title=h4("aktueller Warenstand", align="center")),
         sidebarPanel( 
           selectInput("war_name", "Produckt", choices = c("Auswahl", unique(produckte$Name)),  selectize=F),
           uiOutput("war_slider")),
         mainPanel(plotOutput("war_plot"))
)
)))
