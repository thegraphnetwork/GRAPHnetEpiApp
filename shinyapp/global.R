#Install package manager if it is not installed
if(!require('pacman')) install.packages('pacman'); library('pacman') 

library(pacman)

p_load(char=c("lubridate", # for date manipulation
              "MMWRweek", # for epiweek manipulation
              "knitr", # for kniting this markdown
              "sf", "rnaturalearth", # for map creation
              "zoo", # for rolling averages (e.g. 7 day rolling average)
              "ggnewscale", # for calibrating multiple scales
              "ggrepel", # avoiding overlap on geom_text and geom_label aesthetics
              "viridis", # palettes
              "pander", # for generating tables that output to both pdf and docx
              "kableExtra", # for some tables. Only outputs to pdf
              "visdat", # for visualizing data frames
              "janitor", # for some handy cleaning functions not in tidyverse
              "formattable", "flextable", # for pretty tables
              "webshot", "htmltools", "magick", # for taking screenshots of pretty tables
              "see", "htmlwidgets", 
              "treemapify", # for ggplot treemaps
              "grid", "gridExtra", "ggpubr", # for grids
              "png", # for importing PNGs as rasters
              "stringr", # for text manipulation
              "ggspatial", # to add cross, scale-bar and other important annotations in maps
              "raster", # to succesfully import and extract population from the raster file
              "rgdal", # for several spatial functions
              "ggcharts", 
              "RColorBrewer", # for palettes
              "ggforce", # for automatic facetting in facet plots
              "ggridges",
              "tidyr",
              "forcats",
              "dplyr", # for data man
              "shiny",
              "shinyWidgets",
              "shinydashboard",
              "shinythemes",
              "shinycssloaders",
              "tidyverse",
              "leaflet",
              "ggplot2",
              "plotly",
              "reticulate",
              "DT",
              "stringr",
              "stringi",
              "lubridate",
              "yaml",
              "RColorBrewer"),
update = FALSE)


#loading dictonary
load("dictionary.Rdata")
colnames(dictionary) <- c("Incorrect", "Correct", "AdminLvl")
assign("dictionary",dictionary,envir = .GlobalEnv)

#loading population data
pop <- raster("gpw_v4_population_count_rev11_2020_15_min.tif")
assign("pop",pop,envir = .GlobalEnv)

options(shiny.maxRequestSize=1000*1024^2)

contries_list <- c("All countries",
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo", "Cote d'Ivoire",
  "Democratic Republic of Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara", "Zambia", "Zimbabwe"
)
