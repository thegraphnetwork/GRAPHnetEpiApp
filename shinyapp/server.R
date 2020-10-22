library(shiny)

shinyServer(function(input, output, session) {
    
    # Selecting country
    output$select_country <- renderUI({
        selectInput("selected_country",
                    label = "Select a country:",
                    choices = contries_list,
                    selected = contries_list[1])
    })
    
    output$confirmedCases <- renderText({
        if(input$selected_country=="All countries"){
            prettyNum(115,decimal.mark = ",", big.mark = ".")
        }else{
            prettyNum(sample(c(1:30),1),decimal.mark = ",", big.mark = ".")
        }
    })
    
    output$newCases <- renderText({
        if(input$selected_country=="All countries"){
            paste0(prettyNum(15,decimal.mark = ",", big.mark = ".")," cases this week")
        }else{
            paste0(prettyNum(sample(c(1:10),1),
                decimal.mark = ",", big.mark = ".")," cases this week")
        }
    })
    
    output$Deaths <- renderText({
        if(input$selected_country=="All countries"){
            prettyNum(30,decimal.mark = ",", big.mark = ".")
        }else{
            prettyNum(sample(c(1:20),1),decimal.mark = ",", big.mark = ".")
        }
    })
    
    output$newDeaths <- renderText({
        if(input$selected_country=="All countries"){
            paste0(prettyNum(9,decimal.mark = ",", big.mark = ".")," deaths this week")
        }else{
            paste0(prettyNum(sample(c(1:6),1),
                             decimal.mark = ",", big.mark = ".")," deaths this week")
        }
    })

    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addProviderTiles(providers$CartoDB.Positron, group = "Clear (default)") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
            addProviderTiles(providers$HERE.satelliteDay, group = "Satelite") %>%
            addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Geomap") %>%
            setView(lng = 22.0000, lat = -5.0000, zoom = 3) %>%
            addEasyButton(easyButton(position = "topleft",
                                     icon    = "glyphicon glyphicon-globe", title = "Resetar zoom",
                                     onClick = JS("function(btn, map){ map.setView([-15.5989, -56.0949], 4); }"))) %>%
            addEasyButton(easyButton(position = "topleft",
                                     icon    = "glyphicon glyphicon-map-marker", title = "Localizar-me",
                                     onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }"))) %>%
            addLayersControl(
                position = "topright",
                baseGroups = c("Clear (default)", "Dark", "Satelite", "Geomap"),
                options = layersControlOptions(collapsed = TRUE)
            ) %>%
            addLegend("bottomleft", title= "Quantitative", opacity = 0.92,
                      colors =  RColorBrewer::brewer.pal(n = 7, name = "YlOrRd"),
                      labels = c("No cases","1 - 10", "11-100", "101 - 1.000", "1.001 - 10.000", "10.001 - 100.000", "Acima de 100 mil"))
    })
    
    arq_data1 <- reactiveValues(data = NULL)
    observeEvent(input$file_data1,{
        showNotification("Importing file.",duration = 3,type = "message")
        dat <- data.table::fread(input$file_data1$datapath,stringsAsFactors = F,
                                 header = input$header,
                                 sep = ";")
        if(ncol(dat)<=2){
            dat <- data.table::fread(input$file_data1$datapath,stringsAsFactors = F,
                                     header = input$header,
                                     sep = ",")  
        }
        
        if ("patinfo_ID" %in% names(dat) &
            "patinfo_resadmin1" %in% names(dat) &
            "patcourse_status" %in% names(dat)){
            
            arq_data1$data <- dat
            assign("data1",arq_data1$data,envir = .GlobalEnv)
            
            output$arq_data1_imported = DT::renderDT(
                arq_data1$data,
                options = list(scrollX = TRUE, pageLength = 5, language = list(search = 'Search:'),
                               initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#24ccff', 'color': '#000000'});",
                                   "}")
                               )
            )
            aux_data1=sapply(arq_data1$data, nchar)
            aux_data1B=data.frame(names(arq_data1$data),as.vector(sapply(arq_data1$data, typeof)),
                                  as.vector(apply(aux_data1,2,function(x){sum(x==0,na.rm = T)})),
                                  apply(aux_data1,2,function(x){sum(is.na(x)==TRUE,na.rm = T)}),
                                  row.names = NULL)
            
            output$var_data1 = DT::renderDT(
                aux_data1B,
                options = list(scrollX = TRUE, pageLength = 5, language = list(search = 'Search:'),
                               initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#24ccff', 'color': '#000000'});",
                                   "}")
                ),
                rownames= FALSE,
                colnames=c("Variable","Type","Empty values","Missing data")
            )
            assign("aux_data1B",aux_data1B,envir = .GlobalEnv)
            
            #selecionar camps da variavel data1
            arq_data1_names <- names(arq_data1$data)
            cb_options_data1 <- list()
            cb_options_data1[arq_data1_names] <- arq_data1_names
            updateSelectInput(session, "arq_data1_xaxis",
                              label = "X-Axis",
                              choices = cb_options_data1,
                              selected = "")
            updateSelectInput(session, "arq_data1_yaxis",
                              label = "Y-Axis",
                              choices = cb_options_data1,
                              selected = "")
            }else{
            sendSweetAlert(
                session = session,
                title = "Error!",
                text = "You should import a file that have at least those columns:\n
                patinfo_ID, patinfo_resadmin1, patcourse_status",
                type = "error"
            )
        }
    })
    
    output$clean_data <- renderUI({
        if(!is.null(arq_data1$data)){
            actionButton("clean_data_button", label = "Clean data",
                         style="color: #000000; background-color: #fff; border-color: #24ccff")
        }
    })
    
    observeEvent(input$clean_data_button,{
        showNotification("Data is being cleaning.",duration = 5,type = "message")
    })
    
})
