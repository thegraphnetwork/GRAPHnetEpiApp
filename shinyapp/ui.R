
header <- dashboardHeader(title="WHO Dashboard", disable = T)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(style = "background-color: #fcfcfc;",
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom-box.css"), 
                      fluidPage(shinyjs::useShinyjs(),
                                tags$script(src = "plugins/scripts.js"),
                                tags$head(
                                    tags$link(rel = "stylesheet", 
                                              type = "text/css", 
                                              href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css")
                                ),
                                navbarPage(title = "WHO Dashboard", windowTitle = "Main", id = "navbar", selected = NULL,
                                           fluid = T, collapsible=TRUE, position = "fixed-top",
                                           theme = "style/style.css",
                                           footer = includeHTML("footer.html"),
                                           tabPanel(title = "Main",
                                                    br(),
                                                    br(),
                                                    fluidRow(
                                                        column(width=12,align="left",style='padding-left:30px;',
                                                                    div(style="display:inline-block;vertical-align:bottom;",
                                                                        br(),
                                                                        uiOutput("select_country")
                                                                    )
                                                                  
                                                    )
                                                    ),
                                                    fluidRow(tags$div(class = "line",style="height: 5px;"),
                                                             column(width=12,align="center",
                                                                    #br(),
                                                                    box(width=4,id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                        h1(textOutput("confirmedCases"),
                                                                           align = "center"),
                                                                        tags$b(h4("Confirmed cases",align = "center")),
                                                                        p(textOutput("newCases"),
                                                                          align = "center")
                                                                    ),
                                                                    box(width=4,id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                        h1(textOutput("Deaths"),
                                                                           align = "center"),
                                                                        tags$b(h4("Deaths",align = "center")),
                                                                        p(textOutput("newDeaths"))
                                                                    ),
                                                                    box(width=4,id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                        h1("Some info",
                                                                            #textOutput("released"),
                                                                            align = "center"),
                                                                        tags$b(h4("Placeholder",align = "center")),
                                                                        p("-",style = "color: white")
                                                                    )
                                                             ),
                                                             column(width = 12,
                                                                    br(),
                                                                    box(width = 6, height = "600px", id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                        tabsetPanel(
                                                                            tabPanel(title = "Charts",
                                                                                     h1("We can fit a time series here",
                                                                                        #textOutput("plot_series_title"),
                                                                                        align = "center")
                                                                                     #,plotlyOutput('plot_serie')
                                                                            ),
                                                                            tabPanel(title = "Table",
                                                                                     h1("We can fit a table here",
                                                                                        #textOutput("tabela_municipios_title"),
                                                                                        align = "center")
                                                                                     #,dataTableOutput('tabela_municipios')
                                                                            )
                                                                        )
                                                                    ),
                                                                    
                                                                    
                                                                    box(width = 6, height = "600px", id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                        leafletOutput("map",height = "580px")
                                                                    )
                                                                    
                                                             ),
                                                             column(width = 12,
                                                                    hr(),
                                                                    br(),
                                                                    box(width = 6, height = "700px", id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                        h1("Cases by district",align = "center")
                                                                        #,plotlyOutput("ranking_plot_Unidade", height = "600px")
                                                                    ),
                                                                    box(width = 6, height = "700px", id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                        h1("Some statistics here",align = "center")
                                                                        #,plotlyOutput("ranking_plot_infeccao", height = "600px")
                                                                    )
                                                                    
                                                             ),
                                                             column(width = 12,
                                                                    hr(),
                                                                    br(),
                                                                    box(width = 12,id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                        h1("Profile",align = "center"),
                                                                        tabsetPanel(
                                                                            tabPanel(title = "Infected",
                                                                                     br(),
                                                                                     box(width = 4,id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                                         h2("Sex",align = "center")
                                                                                         #,plotlyOutput("donnut_sex")
                                                                                     ),
                                                                                     box(width = 4,id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                                         h2("Color skin",align = "center")
                                                                                         #,plotlyOutput("donnut_race")
                                                                                     ),
                                                                                     box(width = 4,id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                                         h2("Age",align = "center")
                                                                                         #,plotlyOutput("histogram_age")
                                                                                     )
                                                                            )
                                                                        )
                                                                    )
                                                             ),
                                                             column(width = 12,
                                                                    hr(),
                                                                    br(),
                                                                    box(width = 12,id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                        h1("Quantitative",align = "center")
                                                                        #,dataTableOutput("table_1")
                                                                    )
                                                             )
                                                    )
                                           ),
                                           tabPanel("Downloads")
                                )
                      )
)

ui <- shinyUI(
    dashboardPage(
        header,
        sidebar,
        body
    ) #dashboardPage
)#shinyUI


