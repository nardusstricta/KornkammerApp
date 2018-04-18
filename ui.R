library(rhandsontable)
library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
library(rdrop2)


########################################

shinyUI(
  navbarPage("KornkammerApp",
             tabPanel(
               "Einkaufen",
               sidebarLayout(
                 wellPanel(
                   splitLayout(
                     selectInput(
                       inputId = 'Name', 
                       label = 'Vorname', 
                       choices = unique(c("Konto", mitglieder$Name)), 
                       selectize=F
                     ), 
                     dateInput("date1", "Bestelldatum:", value = Sys.Date())
                   ),
                   splitLayout(
                     selectInput(
                       inputId = "produckt", 
                       label = "Produckt", 
                       choices = unique(c("Auswahl",  produckte$Name)), 
                       selectize=F
                     ),
                     numericInput("menge", "Menge", min = 1, max = 1000, value = 0),
                     textOutput("einheit"), textOutput("preis_pro_gewicht")
                   ),
                   splitLayout(
                     actionButton("best", "In den Warenkorb"),
                     actionButton("deleteRows", "Aus den Warenkorb"),
                     textOutput("Kontostand"),
                     textOutput("Lieferant")
                   )
                 ),
                 mainPanel(
                   actionButton("kaufen", 'Kaufen', style = "color: black;background-color: yellow"),
                   strong("Dein lieber Warenkorb:"),
                   dataTableOutput("table1")
                 )
               )
               
             ),
             tabPanel(
               "Produckte Einbuchen",
               sidebarLayout(
                 wellPanel(
                   splitLayout(
                     selectInput(
                       "inp_name", "Produckt", 
                       choices = c("Auswahl", unique(produckte$Name)),  selectize=F
                     ),
                     selectInput(
                       "inp_lieferant", "Lieferant", 
                       choices = c("Auswahl", unique(produckte$Lieferant)),  selectize=F
                     ),
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
             navbarMenu(
               "Verwaltung",
               tabPanel("Kontoauszug",
                        strong("Hier kannst du deine Kontoauszüge einsehen und als pdf speichern"), 
                        downloadButton('pers_rechn', label="Eingene Rechnung")),
               tabPanel(
                 "Mitgliedschaft ändern",
                 sidebarPanel(
                   wellPanel(
                     strong("Hier kannst du Informationen über deine Mitgliedschaft ändern (z.B. wenn sich die Anzahl der Mitglieder in deiner Mitgliedschaft ändert)"),
                     splitLayout(
                       selectInput("ver_name", "Name", choices = c("Auswahl", unique(mitglieder$Name))),
                       selectInput("ver_date", "Jahr", choices = 2015:2025)),
                     actionButton("ver_bearbeiten", 'bearbeiten', style = "color: white;background-color: red"),
                     actionButton("ver_save", 'speichern', style = "color: black;background-color: yellow")
                     
                   )),mainPanel(
                     DTOutput('x1')
                   )
                 
               ) ,
               tabPanel("allgemeine Jahresbilanz",
                        strong("Hier kannst du die Jahresbilanz der Kornkammer einsehen."),
                        downloadButton('kk_rechn', label="KK Jahresbilanz"))
               
             ),
             
             navbarMenu("aktueller Warenbestand",
                        tabPanel(
                          "Warenbestand graphisch",
                          titlePanel(title=h4("aktueller Warenstand", align="center")),
                          sidebarPanel(
                            
                            selectInput(
                              "war_name",
                              "Produckt", 
                              choices = c("Auswahl", unique(produckte$Name)),  
                              selectize=F
                            ),
                            uiOutput("war_slider")
                          ),
                          mainPanel(plotOutput("war_plot"))
                          
                        ),
                        tabPanel("Warenbestand tabellarisch"))
  )
)
