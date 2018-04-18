

shinyServer(function(input, output, session) {
  
  #Einkaufswagen:------------
  data_temp <- data.table::data.table(
    Datum = as.character(), 
    Name = as.character(), 
    Soll = as.numeric(), 
    Produckt = as.character(), 
    Preis_ID = as.character()
  )
  data_temp_ini <- data_temp
  values <- reactiveValues(df = data_temp)
  
  observeEvent(input$best,{
    if(input$best > 0) {
      if(input$Name == "Konto" | input$produckt == "Auswahl"){
        showModal(modalDialog(
          title = "Achtung, du hast noch nicht alle Felder ausgefüllt!",
          "Prüfe ob du deinen Name angegeben und ein Produkt ausgewählt hast.",
          easyClose = TRUE
        ))
      } else {
        #create the new line to be added from your inputs
        newLine <- isolate(
          c(
            as.character(input$date1),
            input$Name, 
            as.numeric(input$menge) * as.numeric(
              unlist(
                get_cur_price2(
                  ProduckteX = produckte, 
                  NameY1 = input$produckt, 
                  BilanzX1 = bilanz
                )
              )
            ), ## Menge * Preis
            input$produckt,
            unlist(
              get_cur_price2(
                ProduckteX = produckte, 
                NameY1 = input$produckt, 
                BilanzX1 = bilanz, 
                ID=T
              )
            )
          )
        )
        isolate(values$df <- rbind(as.matrix(values$df), unlist(newLine)))
        updateNumericInput(session, "menge", value = 0)
        updateTextInput(session, "produckt", value = "Auswahl")  
      }
    }
  })
  
  observeEvent(input$deleteRows,{
     if (!is.null(input$table1_rows_selected)) {
       values$df <- values$df[-as.numeric(input$table1_rows_selected),]
     }
    })
# Daten in den Database schreiben:
  observeEvent(input$kaufen,{
      data_temp <- values$df
      if(nrow(data_temp) > 0){
        
        withProgress(
          
          message = 'Die Bestellung wird gespeichert',
          detail = 'Ein bisschen Geduld muss da schon sein ...', value = 0, {
            
            for (i in 1:15) {
              incProgress(1/15)
              Sys.sleep(0.10)
            }
            data_temp <- as.data.frame(data_temp)
            names(data_temp) <-  c("Datum", "Name", "Soll", "Produckt", "Preis_ID")
            len <- length(data_temp$Datum)
            prod_temp <- data.frame(
              Datum = data_temp$Datum, 
              Name = data_temp$Produckt, 
              Soll = rep(0, len),
              Haben = as.numeric(as.character(data_temp$Soll)), 
              Verwendung = rep("Verkauf", len),
              Preis_ID = data_temp$Preis_ID, 
              Rechnungs_ID = rep(1, len)
            )
            user_temp <- data.frame(
              Datum = data_temp$Datum, 
              Name = data_temp$Name, 
              Soll = as.numeric(as.character(data_temp$Soll)), 
              Haben = rep(0, len),
              Verwendung = rep("Einkauf", len), 
              Preis_ID = data_temp$Preis_ID, 
              Rechnungs_ID = rep(1, len)
            )
            bilanz_temp <- rbind(user_temp, prod_temp)
            
            write.table(
              bilanz_temp, "buchhaltung.csv", 
              sep = ",", col.names = F, append = T, row.names = F
            )
            bilanz <<- read_csv("buchhaltung.csv")
            drop_upload("buchhaltung.csv")
            
            updateTextInput(session, "Name", value = "Konto")
          }
        ) 
        values$df <- data_temp_ini
        showModal(
          modalDialog(
            title = "Bestellung war erfolgreich!",
            paste("Der gesamt Betrag ist", sum(user_temp$Soll, na.rm = T), "Euro"),
            easyClose = TRUE
          )
        )
      } else {
        showModal(
          modalDialog(
            title = "Bestellung war nicht erfolgreich!",
            paste("Keine Ware im Warenkorb"),
            easyClose = TRUE
          )
        )
      }
  })

  output$table1 <-renderDataTable({values$df})
  
  output$Kontostand <- renderText({
    if(input$Name != "Konto"){
    paste(
      input$Name, "dein Konto", unlist(fun_kont(BilanzX = bilanz, NameY = input$Name)), "€"
    )
    }
  })
  
  output$einheit <- renderText({
    if(input$produckt != "Auswahl"){
    paste(unlist(fun_einh(ProduckteX = produckte, NameY = input$produckt)))
    }
  })
  
  output$preis_pro_gewicht <- renderText({
    if(input$produckt != "Auswahl"){
      paste(
        "Preis/Einehit", 
        unlist(get_cur_price2(ProduckteX = produckte, NameY1 = input$produckt, BilanzX1 = bilanz))
      )
    }
  })
  
  output$Lieferant <- renderText({
    if(input$produckt != "Auswahl"){
      paste(
        "Lieferant", 
        unlist(
          get_cur_Lieferant(ProduckteX = produckte, NameY1 = input$produckt, BilanzX1 = bilanz)
        )
      )
    }
  })
  #
  #Tab2 Waren import
  #
  ## Temporäre Tabelle, die im Warenimport angezeigt wird. 
  ## Aus dieser temporären Tabelle können noch Zeilen gelöscht werden.
  inp_temp <- data.table::data.table(
    Rechnungs_ID= as.numeric(), 
    Name=as.character(), 
    Lieferant = as.character(), 
    Soll = as.numeric(), 
    Einheit = as.character(), 
    Datum = as.character(), 
    Preis_ID = as.numeric()
  )
  inp_temp_ini <- inp_temp # wenn gespeichert wird, dann wird die inp_temp später mit den initialwerten (inp_temp_ini) überschrieben (= Tabelle wird geleert)
  values_inp <- reactiveValues(df_inp = inp_temp)
  
  observeEvent(input$inp_merk,{
    if(input$inp_merk > 0) {
      #create the new line to be added from your inputs
      newLine2 <- isolate(c(
        input$inp_rech_ID, 
        input$inp_name, 
        input$inp_lieferant,
        input$inp_preis, 
        input$inp_einheit, 
        as.character(input$inp_date), 
        as.numeric(fun_produkt_count(produckte, input$inp_name))
      ))
      isolate(
        values_inp$df_inp <- rbind(as.matrix(values_inp$df_inp), unlist(newLine2))
      )
      updateNumericInput(session, "inp_preis", value = 0)
      updateTextInput(session, "inp_name", value = "Auswahl")
    }
  })
  
  observeEvent(input$inp_delrows,{
    if (!is.null(input$table2_rows_selected)) {
      values_inp$df_inp <- values_inp$df_inp[-as.numeric(input$table2_rows_selected),]
    }
  })
  # Importierte Daten in den Database schreiben:
  
  observeEvent(input$inp_save,{
    inp_temp <- values_inp$df_inp
    if(nrow(inp_temp)>0){
      withProgress(message = 'Der Import wird gespeichert',
                   detail = 'Das hast du super gemacht! Zeit für etwas Entspannung ...', 
                   value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.10)
                     }
                     inp_temp <- as.data.frame(inp_temp)
                     names(inp_temp) <-  c(
                       "Rechnungs_ID", "Name", "Lieferant", "Soll", "Einheit", "Datum","Preis_ID"
                     )
                     len2 <- length(inp_temp$Datum)
                     inp_bilanz_temp <- data.frame(
                       Datum = inp_temp$Datum, 
                       Name = inp_temp$Name, 
                       Soll = inp_temp$Soll,
                       Haben = rep(0, len2), 
                       Verwendung = rep("Wareneinkauf", len2),
                       Preis_ID = inp_temp$Preis_ID, 
                       Rechnungs_ID = inp_temp$Rechnungs_ID
                     )
                     
                     write.table(
                      inp_bilanz_temp, "buchhaltung.csv", 
                      sep = ",", col.names = F, append = T, row.names = F
                     )
                    bilanz <<- read_csv("buchhaltung.csv")
                     
                     drop_upload("buchhaltung.csv")
                     
                     
                     inp_produckt_temp <- data.frame(
                       Preis_ID = inp_temp$Preis_ID,
                       Name = inp_temp$Name,
                       Lieferant = inp_temp$Lieferant,
                       Preis = inp_temp$Preis,
                       Einheit = inp_temp$Einheit,
                       Datum = inp_temp$Datum
                     )
                     write.table(
                       inp_produckt_temp, "produckt.csv", 
                       sep = ",", col.names = F, append = T, row.names = F
                     )
                     produckte <<- read_csv("produckt.csv")
                     drop_upload("produckt.csv")
                   }) 
      values_inp$df_inp <- inp_temp_ini ## Inputwerte werden aus initialWerte gesetzt.
      showModal(modalDialog( ## Bestätigung, dass es geklappt hat.
        title = "Der Import war erfolgreich!",
        paste("Den Warenkonten in der Kornkammer wurde in der Summe folgender Betrag gut geschrieben", 
              sum(as.numeric(as.character(inp_bilanz_temp$Soll))), "Euro"),
        easyClose = TRUE
      ))
      
    } else {
      showModal(modalDialog(
        color = "red",
        title = "Bestellung war nicht erfolgreich! :-(",
        paste("Keine Ware auf der Liste"),
        easyClose = TRUE
      ))
    }
  })
  
  
  #output$table2 <- renderDataTable({values_inp$df_inp})
  
  output$table2 <- renderDataTable(
    values_inp$df_inp,
    # Hide logical columns
    options=list(
      columnDefs = list(list(visible=FALSE, targets=6))
    )
  )
  
   output$inp_wert <- renderText({
     paste(
       "Gesamtpreis aktuelle Rechnung in €", 
       sum(as.numeric(as.character(values_inp$df_inp[,4])))
     )
   })
  
  output$inp_preis_pro_einheit <- renderText({
    paste("Preis pro ", input$inp_einheit, ": ", input$inp_preis/input$inp_menge)
  })
  
#      
# Tab3 Kontoauszug import:------
# 

  observeEvent(input$import, {
    
    DF_in <- read_csv(
      input$import$datapath, col_types = cols(Wertstellung = col_date(format = "%d.%m.%Y"))
    )
    tmp_df <<- DF_in
    
    output$hot <- renderRHandsontable({
      
      if (exists("tmp_df")) {
        DF <<- tmp_df
        message("*** loaded data frame from file ***")
        rm(tmp_df, envir = .GlobalEnv)
      }
      
      rhandsontable(DF, rowHeaders = NULL) %>%
        hot_validate_character(cols = "Buchungstext", choices = unique(bilanz$Verwendung))
      
    }) 
    
  })
  
  observeEvent(input$save_gls, {
    
    test <<- isolate(hot_to_r(input$hot))
    rm(DF)
    
  })

  #
  #Tab4 Verwaltung: ###############################
  #
  
  observeEvent(input$ver_bearbeiten,{
    x <- get_mitglieder(MitgliederX=mitglieder, NameY = input$ver_name, DateZ = input$ver_date)
    if(exists("x")) {
      
      output$x1 = renderDT(x,selection = 'none', rownames = F, editable = T)
      x$Date = Sys.time() + seq_len(nrow(x))
      
      proxy <<- dataTableProxy('x1')
      
      observeEvent(input$x1_cell_edit, {
        info = input$x1_cell_edit
        #str(info)
        i = info$row
        j = info$col + 1  # column index offset by 1
        v = info$value
        x[i, j] <<- DT::coerceValue(v, x[i, j]) #vlt. ohne glob.env
        replaceData(proxy, x, resetPaging = FALSE, rownames = FALSE)
        xtest22 <<- x
      })
    }
  })
  observeEvent(input$ver_save,{
    replaceData(proxy, NULL, rownames = FALSE)
    testx <- xtest22 %>% 
      select(-Monat, -Date) %>% 
      mutate(Anzahl_Personen = as.numeric(as.character(Anzahl_Personen))) %>% 
      mutate(Forderung_mtl = Anzahl_Personen * 3)
    
    mitglieder_neu <- mitglieder %>% 
      mutate(
        Forderung_mtl = replace(
          Forderung_mtl, 
          Name == input$ver_name & year(Datum) == input$ver_date, 
          testx$Forderung_mtl
        )
      ) %>% 
      mutate(
        Anzahl_Personen = replace(
          Anzahl_Personen, 
          Name == input$ver_name & year(Datum) == input$ver_date, 
          testx$Anzahl_Personen
        )
      ) %>% 
      mutate(
        E_Mail = replace(
          E_Mail, 
          Name == input$ver_name & year(Datum) == input$ver_date, 
          testx$E_Mail
        )
      )
    if(nrow(mitglieder)==nrow(mitglieder_neu)){
      
      mitglieder <- mitglieder_neu
      write.table(mitglieder, "mitglieder.csv", sep = ",", row.names = F)
      drop_upload("mitglieder.csv")
      #Aktualisieren der Bilanz Tabelle
      bilanz_neu <- fun_pers_replace(
        BilanzX=bilanz, 
        sum_sollY=sum(testx$Forderung_mtl), 
        DatumZ = input$ver_date, 
        NameA=input$ver_name
      )
      if(nrow(bilanz)==nrow(bilanz_neu)){
        
        bilanz <- bilanz_neu 
        write.table(bilanz, "buchhaltung.csv", sep = ",", row.names = F)
        drop_upload("buchhaltung.csv")
        
        showModal(modalDialog(
          title = "Die Änderung war erfolgreich!",
          paste("Du hast jetzt einen neuen Jahresbeitrag von", sum(testx$Forderung_mtl)),
          easyClose = TRUE
        ))
        
      } else {
        rm(bilanz_neu)
        showModal(modalDialog(
          title = "Die Änderung leider nicht erfolgreich!",
          paste("Lade die App neu vlt geht es dann besser", sum(testx$Forderung_mtl)),
          easyClose = TRUE))
      }
    } else {
      rm(mitglieder_neu)
      showModal(modalDialog(
        title = "Die Änderung leider nicht erfolgreich!",
        paste("Lade die App neu vlt geht es dann besser", sum(testx$Forderung_mtl)),
        easyClose = TRUE)) 
    }
    #rm(testx, xtest22, bilanz_neu, mitglieder_neu, proxy)
    updateTextInput(session, "ver_name", value = "Auswahl")
  })
 
  
  
  output$pers_rechn <- downloadHandler(
    filename = "pers_rechn.pdf",
    content = function(file){
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "pers_rechn.Rmd")
      file.copy("pers_rechn.Rmd", tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(
        n = per_bilanz_fun(
          BilanzX = bilanz,
          NameY = input$ver_name,
          DatumZ = input$ver_date
        )
      )
      rmarkdown::render(
        tempReport, output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$kk_rechn <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "kk_bilanz.pdf",
    content = function(file){
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "kk_bilanz.Rmd")
      file.copy("kk_bilanz.Rmd", tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(
        n = kk_bilanz_fun(
          BilanzX = bilanz,
          DatumZ = input$ver_date
        )
      )
      rmarkdown::render(
        tempReport, output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
 
  #      
  # Tab5 Aktueller Warenstand:------
  # 
  output$war_slider <- renderUI({
    dateRangeInput(
      "war_date_from",
      "Daten von:",
      min = min(fun_war(BilanzX = bilanz, NameY = input$war_name)$Datum),
      max = max(fun_war(BilanzX = bilanz, NameY = input$war_name)$Datum),
      start = Sys.Date()-2, end = max(fun_war(BilanzX = bilanz, NameY = input$war_name)$Datum)
    )
  })
  
  output$war_plot <- renderPlot({
    zwar <- fun_war(BilanzX = bilanz, NameY = input$war_name)
    min <- as.character(input$war_date_from[1])
    max <- as.character(input$war_date_from[2])
    
    # draw the histogram with the specified number of bins
    ggplot(zwar[zwar$Datum >= min & zwar$Datum <= max, ], aes(x=Datum, y=cumsoll))+
      geom_line()+
      #geom_smooth()+
      labs(title=paste("Num")) +
      xlab("Time") +
      ylab("NumP") 
  })
})  
