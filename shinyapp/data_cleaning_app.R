# This is a Shiny web application

##################
# START global.R #
##################

# load libraries
library(shiny)
library(readr)
library(rmarkdown)
library(DT)
library(visdat)
library(dplyr)

# turn off scientific notation
options(scipen = 999)

# remove all existing objects
rm(list = ls())

# loading sources
#cleaning <- source(here::here("Scripts", "generic_run_csv.R"), local = T)#$value
#quality <- source(here::here("Scripts", "data_quality_report.R"))$value
map <- source(here::here("Scripts", "map_kenya.R"), local = T)$value
anonym <- source(here:here("Scripts", "anonymisation.R"), local = T)$value

# create list of countries
map_files <- list.files(here::here("scripts"))
countries <- tools::file_path_sans_ext(stringr::str_remove_all(map_files[grepl("map_", map_files)], "map_"))
#countries <- gsub(".R", "", gsub("map_", "", map_files[grep("map_", map_files)]))
#countries_names <- sort(toupper(gsub("_", " ", countries)))
countries_names <- sort(toupper(countries))

# remove specific empty columns
drops <- c("patinfo_first_name",
           "patinfo_last_name",
           "patinfo_resadmin3",
           "patinfo_resadmin4")

################
# END global.R #
################

##############
# START ui.R #
##############

ui <- fluidPage(
    
    tags$head(
        tags$head(
            tags$style(HTML("
                    #quality_report {
                      color: black;
                      background: white;
                      font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif;
                      font-size: 16px;
                      }
                            "))
            )),
                    
    # title
    titlePanel("Data Cleaning Project"),

    # sidebar with a: - select input for country
    #                 - action button for running cleaning script
    #                 - action button for running data quality report
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "country", label = "Country:", choices = countries_names, selected = "Niger"),
            actionButton(inputId = "cleaning_run", label = "Execute Cleaning"),
            actionButton(inputId = "quality_run", label = "Generate Data Quality Report"),
            downloadButton("report", "Export")),
        
        # show clean csv table
        mainPanel(
            h2(uiOutput("country_title")),
            h3(format(Sys.time(), '%d %B, %Y')),
            dataTableOutput("table"),
            h2(textOutput("caption")),
            plotOutput("vis_dat_1"),
            plotOutput("vis_dat_2"),
            verbatimTextOutput("complete_columns"),
            dataTableOutput("missing_values"),
            p(verbatimTextOutput("quality_report")),
            h2("List of Main Issues to Note:"),
            p(fileInput('file1', 'Select the <country>.txt file',
                        accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
              tags$hr())
            )
        )
)

############
# END ui.R #
############

##################
# START server.R #
##################

# define server logic
server <- function(input, output, session) {
   
    # Anonimyzation of the datasets and storage of data 
    anonyms <- observe({anonym(tolower(input$country))})
    
    
    # global reactive values
    df <- reactive({df <- as.data.frame(read_csv(here::here("data/CleanCSV", glue::glue("{input$country}_clean.csv")))) %>%
        select(-drops)
    })
    
    num_cols <- reactive({
        num_cols <- ncol(df())
    })
    
    # adaptive UI
    output$country_title <- renderUI({
        toupper(gsub("_", " ", input$country))
    })
    
    # Needs to be adapted
    observe({
        file1 = input$file1
        if (is.null(file1)) {
            return(NULL)
        }
        output$file1 <- renderText({file.show("~/data-cleaning/text/niger")
    })
    })
    
    output$country <- renderUI({
        tolower(gsub(" ", "_", input$country))
    })
    
    # run cleaning script if activated
    observeEvent(input$cleaning_run, {
        
        cleaning_reactive <- reactiveValues()
        country <- tolower(gsub(" ", "_", input$country))
        cleaning_reactive$cleaning_out <- cleaning_run(input$country)
        showModal(modalDialog(paste0("Cleaning for ", input$country, " done!")))
        })
    
    # run quality script if activated
    observeEvent(input$quality_run, {
        
        output$table <- DT::renderDataTable({
            datatable(df(), filter = "top")
        })
        
        output$caption <- renderText({
            "Checking Missing Values"
        })
        
        output$vis_dat_1 <- renderPlot({
            vis_dat(df()[, 1:round(num_cols() / 2)], sort_type = FALSE)
        })
        
        output$vis_dat_2 <- renderPlot({
            vis_dat(df()[, (round(num_cols() / 2) + 1):num_cols()], sort_type = FALSE)
        })
        
        na_percentages <- reactive({
            round(colMeans(is.na(df())) * 100, 2)
        })
        
        cols_complete <- reactive({
            names(na_percentages()[na_percentages() == 0])
        })
        cols_partial <- reactive({
            na_percentages()[na_percentages() > 0 & na_percentages() < 100]
        })
        cols_empty <- reactive({
            names(na_percentages()[na_percentages() == 100])
        })
        cols_notempty <- reactive({
            names(na_percentages()[na_percentages() != 100])
        })
        
        # output$complete_columns <- renderPrint({
        #     paste0("\n", length(cols_complete()), " complete columns:\n")
        #     paste(cols_complete(), collapse = ", ")
        # })
        # 
        # cat(paste0("\n\n", length(cols_partial), " partially filled columns (see percentage of NA below):\n"))
        # print(datatable(as.data.frame(sort(cols_partial))))
        # 
        # cat(paste0("\n", length(cols_empty), " empty columns:\n"))
        # cat(paste(cols_empty, collapse = ", "))
        
        output$missing_values <- DT::renderDataTable({
            datatable(as.data.frame(sort(cols_partial())))
        })
        
        # run quality report
        quality_reactive <- reactiveValues()
        output$quality_report <- renderPrint({
            quality_reactive$quality_out <- quality(df())
        })
        
    })
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.doc",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(input$country)
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
}

################
# END server.R #
################

# run the application 
shinyApp(ui = ui, server = server)
