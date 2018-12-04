## install all packages that are not installed yet:
list.of.packages <- c("rhandsontable", "shiny", "DT", "tidyverse", "lubridate", "rdrop2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## dann müssten noch die Aktuellen Versionen der Pakete gespeichert werden, damit es universell funktioniert.

library(rhandsontable)
library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
library(rdrop2)

# token <<- readRDS("token.rds")
# drop_acc(dtoken = token)

source("fun.R")
source("fun_prod_einbuchen.R")
source("fun_warenbestand.R")
source("fun_verwaltung.R")

# bilanz_imp <- drop_read_csv("buchhaltung.csv")#, colClasses = c("Date",  "character", "numeric", "numeric", "character","integer","integer"))
# 
# mitglieder_imp <- drop_read_csv("mitglieder.csv")#, colClasses = c("integer", "integer", "character", "character", "integer", "Date"))
# produckte_imp <<- drop_read_csv("produckt.csv")#,colClasses = c("integer", "character", "character", "numeric", "character", "Date"), sep = ",")
# 
# write.table(bilanz_imp, "buchhaltung.csv", sep = ",", col.names = T, append = F, row.names = F)
# write.table(mitglieder_imp , "mitglieder.csv", sep = ",", col.names = T, append = F, row.names = F)
# write.table(produckte_imp, "produckt.csv", sep = ",", col.names = T, append = F, row.names = F)

bilanz <<- read_csv("buchhaltung.csv")
mitglieder <<- read_csv("mitglieder.csv", 
                        col_types = cols(Datum = col_date(format = "%Y-%m-%d"))
)

mitglieder <- mitglieder %>%
  arrange(Datum) %>%
  arrange(Mitgliedsnummer)


produckte <<- read_csv("produckt.csv")

## Bei Jahreswechsel werden automatisch für jeden Account neue Zeilen erstellt, für jeden Monat.
## !!! Achtung! wenn schon händisch eine Zeile des neuen Jahres eingetragen wurde, bevor diese Funktion abgeschickt wurde, wird das nicht mehr passieren!
if(lubridate::year(max(mitglieder$Datum)) != year(Sys.Date())){
  mitglieder_neu <- expand_mitglieder(mitglieder)
  write_csv(mitglieder_neu, "mitglieder.csv")
  mitglieder <<- read_csv("mitglieder.csv", 
                          col_types = cols(Datum = col_date(format = "%Y-%m-%d")))
}


########################################

shinyUI(
  navbarPage("KornkammerApp",
             tabPanel(
               "Einkaufen",
                 wellPanel(
                   splitLayout(
                     selectInput(
                       inputId = 'Name', 
                       label = 'Vorname', 
                       choices = unique(c("Konto", mitglieder$Name)), 
                       selectize=T
                     ), 
                     dateInput("date1", "Bestelldatum:", value = Sys.Date()),
                     tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))
                   ),
                   splitLayout(
                     selectInput(
                       inputId = "produckt", 
                       label = "Produckt", 
                       choices = unique(c("Auswahl",  produckte$Name)), 
                       selectize=T
                     ),
                     numericInput("menge", "Menge", min = 1, max = 1000, value = 0),
                     textOutput("einheit"), textOutput("preis_pro_gewicht"),
                     tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))
                   ),
                   splitLayout(
                     actionButton("best", "In den Warenkorb"),
                     actionButton("deleteRows", "Aus den Warenkorb"),
                     textOutput("Kontostand"),
                     textOutput("Lieferant")
                   )
                   ),
                 
                   actionButton("kaufen", 'Kaufen', style = "color: black;background-color: yellow"),
                   strong("Dein lieber Warenkorb:"),
                   dataTableOutput("table1")
                 
               
             ),
             tabPanel(
               "Produckte Einbuchen",
                 wellPanel(
                   splitLayout(
                     selectInput(
                       "inp_name", "Produckt", 
                       choices = c("Auswahl", unique(produckte$Name)),  selectize=T
                     ),
                     selectInput(
                       "inp_lieferant", "Lieferant", 
                       choices = c("Auswahl", unique(produckte$Lieferant)),  selectize=T
                     ),
                     numericInput("inp_preis", "Gesamtpreis", min = 1, max = 1000, value = 0),
                     tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))
                     ),
                   splitLayout(
                     selectInput("inp_einheit", "Einheit", choices = c("kg", "g", "l"),  selectize=T),
                     numericInput("inp_menge", "Gesamtmenge", value = 0),
                     numericInput("inp_rech_ID", "Rechnungs_ID", min = 1, max = 1000, value = 0),
                     dateInput("inp_date", "Datum Wareneingang", value = Sys.Date()),
                     tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))
                   ),
                   splitLayout(
                     actionButton("inp_merk", "Auf die Liste"),
                     actionButton("inp_delrows", "löschen von der Liste"),
                     textOutput("inp_wert"),
                     textOutput("inp_preis_pro_einheit")
                   )
                 ),
                 
                   actionButton("inp_save", 'speichern', style = "color: black;background-color: yellow"),
                   dataTableOutput("table2")
                
               
             ),
             tabPanel("Konotauszug importiern",
                        wellPanel(
                          fileInput("import", "Load Model Data",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"),
                                    buttonLabel = "Suchen"),
                          actionButton("save_gls", 'speichern', style = "color: white;background-color: red")
                        ),
                          rHandsontableOutput("hot")
                        ),
             navbarMenu(
               "Verwaltung",
               tabPanel("Kontoauszug",
                        strong("Hier kannst du deine Kontoauszüge einsehen und als pdf speichern"), 
                        selectInput("ver_date1", "Jahr", choices = 2015:2025),
                        selectInput("ver_name1", "Name", choices = c("Auswahl", unique(mitglieder$Name))),
                        downloadButton('pers_rechn', label="Eingene Rechnung")
                        ),
               tabPanel(
                 "Mitgliedschaft ändern",
                
                     wellPanel(
                       strong("Hier kannst du Informationen über deine Mitgliedschaft ändern (z.B. wenn sich die Anzahl der Mitglieder in deiner Mitgliedschaft ändert)"),
                       
                       splitLayout(
                         
                         selectInput("ver_name", "Name", 
                                     choices = c(
                                       "Auswahl", 
                                       paste0(unique(mitglieder$Name), " ", unique(mitglieder$Mitgliedsnummer))
                                     )),
                         selectInput("ver_date", "Jahr", choices = 2015:2025),
                         tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))
                       ),
                       
                       actionButton("ver_bearbeiten", 'bearbeiten', style = "color: white;background-color: red"),
                       actionButton("ver_save", 'speichern', style = "color: black;background-color: yellow")
                     ),
                       
                     DTOutput('x1')
                   
                 
               ) ,
               tabPanel("allgemeine Jahresbilanz",
                        strong("Hier kannst du die Jahresbilanz der Kornkammer einsehen."),
                        selectInput("ver_date2", "Jahr", choices = 2015:2025),
                        downloadButton('kk_rechn', label="KK Jahresbilanz"))
               
             ),
             
             navbarMenu("aktueller Warenbestand",
                        tabPanel(
                          "Warenbestand graphisch",
                          titlePanel(title=h4("aktueller Warenstand", align="center")),
                          
                            
                            selectInput(
                              "war_name",
                              "Produckt", 
                              choices = c("Auswahl", unique(produckte$Name)),  
                              selectize=F
                            ),
                            uiOutput("war_slider"),
                            
                            plotOutput("war_plot")
                          
                        ),
                        tabPanel("Warenbestand tabellarisch",
                                 DTOutput("Warenbestand_gesamt"))
            )
  )
)
