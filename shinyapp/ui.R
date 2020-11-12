
header <- dashboardHeader(title="WHO Dashboard", disable = T)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(style = "background-color: #fcfcfc;", #fafeff;
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom-primary_who.css"),
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
                                                               ),
                                                               div(style="display:inline-block;vertical-align:middle;",
                                                                   h4(textOutput("last_update")),
                                                                   br()
                                                               )
                                                        )
                                                    ),
                                                    fluidRow(
                                                        tags$div(class = "line",style="height: 5px;"),
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
                                                                   h1(textOutput("CasesPerMillion"),
                                                                      align = "center"),
                                                                   tags$b(h4("Cases per million",align = "center")),
                                                                   p(textOutput("DeathsPerMillion"))
                                                               )
                                                        ),
                                                        
                                                        # Column 1
                                                        column(width = 12,
                                                               h1("Cronological view"),
                                                               
                                                               #Box 1
                                                               box(width = 6, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   tabsetPanel(
                                                                       tabPanel(title = "Chart",
                                                                                br(),
                                                                                plotlyOutput("ts_growth_rate_tab")
                                                                       ),
                                                                       tabPanel(title = "Table",
                                                                                br(),
                                                                                DT::DTOutput("table_all_contries",width = "100%") 
                                                                       )
                                                                   )
                                                               ),
                                                               
                                                               #Box 2
                                                               box(width = 6, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   tabsetPanel(
                                                                       tabPanel("Cases",
                                                                                br(),
                                                                                plotlyOutput("ts_epi_curve_ll_confirmed")
                                                                       ),
                                                                       tabPanel("Deaths",
                                                                                br(),
                                                                                plotlyOutput("ts_epi_curve_ll_deaths")
                                                                       )
                                                                   )
                                                               )
                                                               
                                                        ),
                                                        
                                                        # Column 2
                                                        column(width = 12,
                                                               hr(),
                                                               br(),
                                                               
                                                               #Box 3
                                                               box(width = 6, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   br(),
                                                                   tabsetPanel(
                                                                       tabPanel(title = "Chart",
                                                                                br(),
                                                                                div(
                                                                                    uiOutput("select_region")
                                                                                ),
                                                                                plotlyOutput("ts_df_regional_comparison")
                                                                       ),
                                                                       tabPanel(title = "Table",
                                                                                br(),
                                                                                DT::DTOutput("table_all_regions",width = "100%") 
                                                                       )
                                                                   )
                                                               ),
                                                               
                                                               #Box 4
                                                               box(width = 6, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   h1("Placeholder",align = "center")
                                                                   #,plotlyOutput("ranking_plot_infeccao", height = "600px")
                                                               )
                                                        ),
                                                        
                                                        # Column 3
                                                        column(width = 12,
                                                               h1("Africa view"),
                                                               box(width = 6, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   h2("Cases per Million"),
                                                                   leafletOutput("map_cases_per_million",height = "580px")
                                                               ),
                                                               
                                                               box(width = 6, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   h2("Deaths per Million"),
                                                                   leafletOutput("map_deaths_per_million",height = "580px")
                                                               )
                                                        ),
                                                        
                                                        # Column 4
                                                        column(width = 12,
                                                               box(width = 6, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   h2("Cumulative Cases"),
                                                                   leafletOutput("map_cumulative_cases",height = "580px")
                                                               ),
                                                               
                                                               box(width = 6, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   h2("Cumulative Deaths"),
                                                                   leafletOutput("map_cumulative_deaths",height = "580px")
                                                               )
                                                        ),
                                                        
                                                        # Column 5
                                                        column(width = 12,
                                                               h1("Risk map (LVL 1)"),
                                                               box(width = 4, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   h2("Mortality Risk Index (raw)"),
                                                                   p("Raw and not including distance from medical facility", align = "center"),
                                                                   leafletOutput("map_tri_lvl1_1",height = "580px")
                                                               ),
                                                               
                                                               box(width = 4, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   h2("Normalized Transmission Risk Index"),
                                                                   p("Raw and not including distance from medical facility", align = "center"),
                                                                   leafletOutput("map_tri_lvl1_2",height = "580px")
                                                               ),
                                                               
                                                               box(width = 4, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                                   h2(" Normalized Mortality Risk Index"),
                                                                   p("Including distance from medical facility between 0 and 100"),
                                                                   leafletOutput("map_tri_lvl1_3",height = "580px")
                                                               )
                                                        )
                                                        
                                                        # Column 6
                                                        # column(width = 12,
                                                        #        h1("Risk map (LVL 1)"),
                                                        #        box(width = 6, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                        #            h2(" Normalized Mortality Risk Index"),
                                                        #            leafletOutput("map_tri_lvl1_3",height = "580px")
                                                        #        ),
                                                        # 
                                                        #        box(width = 6, id="box_info",solidHeader = TRUE, status = "primary", collapsible = F,
                                                        #            h1("Placeholder",align = "center")
                                                        #        )
                                                        # )
                                                        
                                                        
                                                    )
                                           ),
                                           tabPanel("Data",
                                                    h2("Importing data",align="center"),
                                                    
                                                    #Importing data
                                                    box(width=12, title="Main file", status="primary",
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
                                                        )),
                                                    
                                                    #Importing GPKG files
                                                    box(width=12, title="GPKG file", status="primary",
                                                        solidHeader = TRUE, collapsible = F,
                                                        column(width = 4,
                                                               # Input: Select a file ----
                                                               
                                                               fileInput("file_data2", "Choose .GPKG file", buttonLabel = "Import",
                                                                         multiple = FALSE, placeholder = "Select a file",
                                                                         accept = c(".gpkg")),
                                                               uiOutput("combine_data")
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


