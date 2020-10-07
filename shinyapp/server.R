
library(shiny)

shinyServer(function(input, output) {
    
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
    
})
