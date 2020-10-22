
header <- dashboardHeader(title="WHO Dashboard", disable = T)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(style = "background-color: #fcfcfc;",
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom-primary.css"),
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom-box.css"), 
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom-style.css"), 
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom-progressbar.css"), 
                      fluidPage(shinyjs::useShinyjs(),
                                tags$script(src = "plugins/scripts.js"),
                                tags$head(
                                    tags$link(rel = "stylesheet", 
                                              type = "text/css", 
                                              href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css")
                                ),
                                div(
                                    img(src = "images/who-logo.png", height = 100, width = 290),
                                    hr()
                                ),
                                navbarPage(title = "WHO Dashboard", windowTitle = "Main", id = "navbar", selected = NULL,
                                           fluid = T, collapsible=TRUE, 
                                           footer = includeHTML("footer.html"),
                                           tabPanel(title = "Main",
                                                    
                                                    fluidRow(
                                                        column(width=12,align="left",style='padding-left:30px;',
                                                               div(style="display:inline-block;vertical-align:bottom;",
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
                                           tabPanel("Data",
                                                    h2("Importing data",align="center"),
                                                    
                                                    #Importing data
                                                    box(width=12,title="Main file",status="primary",
                                                        solidHeader = TRUE, collapsible = F,
                                                        column(width = 4,
                                                               # Input: Select a file ----
                                                               
                                                               fileInput("file_data1", "Choose .CSV file", buttonLabel = "Import",
                                                                         multiple = FALSE, placeholder = "Select a file",
                                                                         accept = c("text/csv",
                                                                                    "text/comma-separated-values,text/plain",
                                                                                    ".csv",".xlsx")),
                                                               
                                                               checkboxInput("header", "Header", TRUE),
                                                               #checkboxInput("preview", "Visualizar ao abrir", TRUE),
                                                               
                                                               uiOutput("clean_data")
                                                        ),
                                                        #municipal
                                                        column(width = 8,
                                                               tabsetPanel(id="view_data1",
                                                                           tabPanel("Data",br(),
                                                                                    DT::DTOutput('arq_data1_imported')),
                                                                           tabPanel("Variables",column(width = 8,br(),
                                                                                                       DT::DTOutput('var_data1'))))
                                                        ))
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


