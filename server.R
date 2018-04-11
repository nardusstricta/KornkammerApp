library(shiny)
library(DT)
library(dplyr)

#Temp Ordner Tabelle
data_temp <- data.frame(Datum = as.character(), Name = as.character(), 
                        Soll = as.numeric(), Produckt = as.character(), row.names = NULL)
data_temp_ini <- data_temp



shinyServer(function(input, output, session) {
  
  #Einkaufswagen:------------
  values <- reactiveValues(df = data_temp)
  values$df <- data_temp
  
  observeEvent(input$best,{
    if(input$best > 0) {
      #create the new line to be added from your inputs
      newLine <- isolate(c(as.character(input$date1),input$Name, 
                           as.numeric(input$menge) * as.numeric(unlist(get_cur_price2(ProduckteX = produckte, NameY = input$produckt, unlist(get_preis_ID2(BilanzX = bilanz, NameY = input$produckt))))),
                           input$produckt))
      isolate(values$df <- rbind(as.matrix(values$df), unlist(newLine)))
      data_temp <<- values$df
      updateNumericInput(session, "menge", value = 0)
      updateTextInput(session, "produckt", value = "Auswahl")
    }
  })
  
    observeEvent(input$deleteRows,{
     if (!is.null(input$table1_rows_selected)) {
       values$df <- values$df[-as.numeric(input$table1_rows_selected),]
     }
    })
# Daten in den Database schreiben:
      observeEvent(input$kaufen,{
        if(length(data_temp[,1] > 0)){
          withProgress(message = 'Die Bestellung wird gespeichert',
                       detail = 'Ein bisschen Geduld muss da schon sein ...', value = 0, {
                         for (i in 1:15) {
                           incProgress(1/15)
                           Sys.sleep(0.10)
                         }
                         data_temp <- as.data.frame(data_temp)
                         names(data_temp) <-  c("Datum", "Name", "Soll", "Produckt")
                         len <- length(data_temp$Datum)
                         prod_temp <- data.frame(Datum = data_temp$Datum, Name = data_temp$Produckt, Soll = rep(0, len),
                                                 Haben = as.numeric(as.character(data_temp$Soll)), Verwendung = rep("Verkauf", len),
                                                 Preis_ID = rep(1, len), Rechnungs_ID = rep(1, len))
                         
                         user_temp <- data.frame(Datum = data_temp$Datum, Name = data_temp$Name, 
                                                 Soll = as.numeric(as.character(data_temp$Soll)), Haben = rep(0, len),
                                                 Verwendung = rep("Einkauf", len), Pries_ID = rep(1, len), 
                                                 Rechnungs_ID = rep(1, len))
                        
                         bilanz_temp <- rbind(user_temp, prod_temp)
                         
                         write.table(bilanz_temp, "buchhaltung.csv", sep = ",", col.names = F, append = T, row.names = F)
                         bilanz <<- read_csv("buchhaltung.csv")
                         updateTextInput(session, "Name", value = "Konto")
                       }) 
          values$df <- data_temp_ini
          showModal(modalDialog(
            title = "Bestellung war erfolgreich!",
            paste("Der gesamt Betrag ist", sum(user_temp$Soll, na.rm = T), "Euro"),
            easyClose = TRUE
          ))
      
        }else{
          showModal(modalDialog(
            title = "Bestellung war nicht erfolgreich!",
            paste("Keine Ware im Warenkorb"),
            easyClose = TRUE
          ))
        }
    })


  output$table1 <- renderDataTable({values$df})
  
  output$Kontostand <- renderText({
    if(input$Name!= "Konto"){
    paste(input$Name, "dein Konto", unlist(fun_kont(BilanzX = bilanz, NameY = input$Name)), "€")
    }
  })
  
  output$einheit <- renderText({
    if(input$produckt != "Auswahl"){
    unlist(fun_einh(ProduckteX = produckte, NameY = input$produckt))
    }
  })
  
  #
  #Tab2 Waren import
  #
  inp_temp <- data.frame(Rechnungs_ID= as.numeric(), Name=as.character(), 
                         Lieferant = as.character(), Soll = as.numeric(), Einheit = as.character(), 
                         Datum = as.character())
  values_inp <- reactiveValues(df_inp = inp_temp)
  values_inp$df_inp <- inp_temp
  
  observeEvent(input$inp_merk,{
    if(input$inp_merk > 0) {
      #create the new line to be added from your inputs
      newLine2 <- isolate(c(input$inp_rech_ID, input$inp_name, input$inp_lieferant,
                            input$inp_preis, input$inp_einheit, as.character(input$inp_date)))
      isolate(values_inp$df_inp <- rbind(as.matrix(values_inp$df_inp), unlist(newLine2)))
      inp_temp <<- values_inp$df_inp
      updateNumericInput(session, "inp_preis", value = 0)
      updateTextInput(session, "inp_lieferant", value = "Auswahl")
    }
  })
  
  observeEvent(input$inp_delrows,{
    if (!is.null(input$table2_rows_selected)) {
      values_inp$df_inp <- values_inp$df_inp[-as.numeric(input$table2_rows_selected),]
    }
  })
  # Importierte Daten in den Database schreiben:
  observeEvent(input$inp_save,{
    if(length(inp_temp[,1] > 0)){
      withProgress(message = 'Der Import wird gespeichert',
                   detail = 'Das hast du super gemacht! Zeit für etwas Entspannung ...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.10)
                     }
                     inp_temp <- as.data.frame(inp_temp)
                     names(inp_temp) <-  c("Preis_ID", "Name", "Liferant", "Soll", "Einheit", "Datum")
                     len2 <- length(inp_temp$Datum)
                     inp_bilanz_temp <- data.frame(Datum = inp_temp$Datum, Name = inp_temp$Name, 
                                                   Soll = inp_temp$Soll,
                                                   Haben = rep(0, len2), Verwendung = rep("Wareneinkauf", len2),
                                                   Preis_ID = rep(1, len2), Rechnungs_ID = inp_temp$Preis_ID)
                     write.table(inp_bilanz_temp, "buchhaltung.csv", sep = ",", col.names = F, append = T, row.names = F)
                     bilanz <<- read_csv("buchhaltung.csv")
                   }) 
      values_inp$df_inp <- inp_temp
      showModal(modalDialog(
        title = "Der Import war erfolgreich!",
        paste("Den Warenkonten in der Kornkammer wurde in der Summe folgender Betrag gut geschrieben", 
              sum(as.numeric(as.character(inp_bilanz_temp$Soll))), "Euro"),
        easyClose = TRUE
      ))
      
    }else{
      showModal(modalDialog(
        title = "Bestellung war nicht erfolgreich!",
        paste("Keine Ware im Warenkorb"),
        easyClose = TRUE
      ))
    }
  })
  
  
  output$table2 <- renderDataTable({values_inp$df_inp})
  
  output$inp_wert <- renderText({
    paste("Gesamtpreis aktuelle Rechnung in €", sum(as.numeric(as.character(values_inp$df_inp[,4]))))
  })
  
  output$inp_preis_pro_einheit <- renderText({
    paste("Preis pro ", input$inp_einheit, ": ", input$inp_preis/input$inp_menge)
  })
  
  #      
  # Tab3 Kontoauszug import:------
  # 
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = T)
  })  

})  
