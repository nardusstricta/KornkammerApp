library(shiny)
library(DT)
library(tidyverse)
bilanz <<- read_csv("buchhaltung.csv")
mitglieder <<- read_csv("mitglieder.csv")
produckte <<- read_csv("produckt_info.csv")

shinyUI(fluidPage(
  tabsetPanel(
  tabPanel("Einkaufen",
  sidebarLayout(
      wellPanel(style="overflow:hidden;",
                splitLayout(selectInput(inputId = 'Name', label = 'Vorname', 
                                        choices = c("Konto", mitglieder$Name), selectize=F), 
                            dateInput("date1", "Bestelldatum:", value = Sys.Date())),
                splitLayout(selectInput(inputId = "produckt", label = "Produckt", 
                                        choices = c("Auswahl",  produckte$Name), selectize=F),
                  numericInput("menge", "Menge", min = 1, max = 1000, value = 0),
                            textOutput("einheit")),
splitLayout(
  actionButton("best", "In den Warenkorb"),
  actionButton("deleteRows", "Aus den Warenkorb"),
  textOutput("Kontostand")
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
             selectInput("inp_name", "Produckt", choices = produckte$Name,  selectize=F),
             selectInput("inp_lieferant", "Lieferant", choices = c("Auswahl", produckte$Lieferant),  selectize=F),
             numericInput("inp_preis", "Gesamtpreis", min = 1, max = 1000, value = 0)),
             splitLayout(
             selectInput("inp_einheit", "Einheit", choices = c(NA,"kg", "g", "l"),  selectize=F),
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
         fileInput("file1", "Wähle aktuellen Konotauszug CSV File",
                   accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"),
                   buttonLabel = "Suchen")),
         mainPanel(
           tableOutput("contents")
         ))),
tabPanel("Verwaltung"
),
tabPanel("aktueller Warenstand"
)
)))
