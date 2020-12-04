# Niger data cleaning
# author: Daniel Camara & Izabel Reis
# date: "Aug 08, 2020"


###################################
###################################
##                               ##
##      INSTALATION SECTION      ##
##                               ##
###################################
###################################

# devtools::install_github("reconhub/linelist")
# install.packages("pacman") #216298405250
# library(linelist)
library(pacman)# installs and/or loads everything

p_load(char = c("lubridate", # for date manipulation
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
                "ggspatial", # to add cross, scale-bar and other important annotations in maps
                "raster", # to succesfully import and extract population from the raster file
                "rgdal", # for several spatial functions
                "ggcharts", 
                "RColorBrewer", # for palettes
                "ggforce", # for automatic facetting in facet plots
                "ggridges",
                "tidyverse"), # for everything, comes at the end to prevent masking of crucial functions
       update = FALSE)

# install for screenshots
webshot::install_phantomjs(force = TRUE)

# to unload libraries
# pacman::p_unload(pacman::p_loaded(), character.only = TRUE)


############################################################
############################################################
##                                                        ##
##      IMPORTING DATA AND CREATING DFs/GPKGs SECTION     ##
##                                                        ##
############################################################
############################################################

############################
##                        ##
##     Defining paths     ##
##                        ##
############################

# defining faux linelist (Faux LL) path
LL_raw_path <- "~/data-platform/data/ConfirmedCases.csv"

# defining country info path (regions, populations and isocodes for continental Africa, used for Faux LL)
country_info_path <- "~/data-platform/data/country_info/country_info_africa.csv"

# loeading country info for continental Africa analyses (faux LL)
country_info <- read_csv(country_info_path, trim_ws = T)

# Set the last date for which Faux LL analyses should be run (will remove deaths and recoveries occurring after this date also)
lastdate <- "2020-09-01"

# defining wihch country to analyze
my_country <- "Niger"

# defining relative path to import stuff
in_path = "/work/data-platform/data/"

# defining relative path to export figures, tables and maps
out_path = paste0("../data-platform/data/maps_graphs_results/", my_country)

# defining relative path to import cleanCSV
clean_csv_path = paste0(in_path, "cleanCSV/")


########################
##                    ##
##      Set Theme     ##
##                    ##
########################

# Creating a palette of colors for visual identity
my_palette <- c("#56bfa3", "#f79f57" ,"#7570B3","#56B4E9",  # greenblue, lightorange, purplepastel, lightblue
                "#3758a6" , "#CC79A7" , "#91142c", "#7abcd6", # darkblue, pinkpurple, wine, teal
                "#a3b0c4", "#870476", "#479444", "#3cd6d6" ) # grey, royalpurple, darkgreen, cyan

# Setting a theme for all graphs for visual identity
my_theme <- theme_classic() +
  theme(text = element_text(color = "gray20", size = 12.5),
        rect = element_blank(), # transparent background
        plot.title = (element_text(face = "bold", hjust = 0.5, size = 11)),
        plot.subtitle = (element_text( hjust = 0.5, size = 8, color = alpha("black", 0.7))),
        plot.caption = element_text(size = 6.5, color = "gray50", hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(1,"line"),
        axis.title = element_text( color = alpha("black", 0.8), size = 9),
        axis.text = element_text(color = alpha("gray20", 0.7), size = 7 ),
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = alpha("gray50", 0.1), size = 0.25), 
        strip.background = element_rect(fill=alpha("#0070c0", 1), color = "transparent"),
        strip.text = element_text(face = "bold", colour = 'white'),
        panel.spacing.x = unit(3, "mm"), 
        title = element_text(face = "bold"))

# setting our theme as default
theme_set(my_theme)

# turning off annoying scientific notation
options(scipen = 999)


##########################################
##                                      ##
##     Custom functions for tables      ##
##                                      ##
##########################################

# function for exporting formattable outputs 
export_formattable <- function(f, file, width = "100%", height = NULL, background = "white", delay = 0.3)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay, zoom = 3 ,  )
}

# define custom formatter for formattable
growth_formatter <- 
  formattable::formatter("span", 
                         style = x ~ formattable::style(font.weight = "bold", 
                                                        color = ifelse(x > 0, "#eb345e", # positive change, red
                                                                       ifelse(x < 0, "#22ab3d", # negative change, green
                                                                              "black"))), # otherwise black
                         # icon followed by text
                         x ~ formattable::icontext(ifelse(x > 0, "arrow-up", # positive change , up arrow
                                                          ifelse(x < 0, "arrow-down",#negative change down arrow
                                                                 "none") # otherwise nothing
                         ), x) # then paste the text
  )


# modify color-tile function to print small text 
color_tile_small_text <- function (...) 
{
  formattable::formatter("span", style = function(x) formattable::style(display = "block", 
                                                                        padding = "0 4px", "font-size:11px",  `border-radius` = "4px", `background-color` = csscolor(gradient(as.numeric(x), 
                                                                                                                                                                              ...))))
}

# modify color-tile function to print small text 
color_tile_small_text_bold <- function (...) 
{
  formattable::formatter("span", style = function(x) formattable::style(display = "block", 
                                                                        padding = "0 4px", "font-size:10px", "font-weight:bold",  `border-radius` = "4px", `background-color` = csscolor(gradient(as.numeric(x), 
                                                                                                                                                                                                  ...))))
}

# function to reduce header size
make_header_small <- formattable::formatter("span", style = "font-size:9px") 

# color bar without padding on 0 values
color_bar2 <- function (color = "lightgray", fun = "proportion") {
  fun <- match.fun(fun)
  formattable::formatter("span", style = function(x) formattable::style(display = "inline-block", 
                                                                        direction = "rtl", `border-radius` = "4px", `padding-right` = "0px",  # right padding to 0px 
                                                                        `background-color` = csscolor(color), width = percent(fun(as.numeric(x)))))
}


##########################################################
##########################################################
##                                                      ##
##                                                      ##
##                  CODE FOR SECTION 1                  ##
##                                                      ##
##                  WHERE WE USE THE                    ##
##                                                      ##
##                    FAUX LINELIST                     ##
##                                                      ##
##                                                      ##
##########################################################
##########################################################



#########################################################################################
##                                                                                     ##
##      Loading and formatting  faux LL reference and data for continental Africa      ##
##                                                                                     ##
#########################################################################################

# Load Faux LL data
LL_raw <- read_csv(LL_raw_path, trim_ws = T, na = c("", " ", "NA")) 

# Manipulating country labels so that merging will be easier later
LL_raw <- LL_raw %>% 
  mutate(Country = as.character(Country)) %>% 
  # renaming mislabelled countries to merge with map polygon later
  mutate(Country = case_when(Country == "CAF" ~ "Central African Republic",
                             Country == "CÃ´te d'Ivoire" ~ "Côte d'Ivoire",
                             Country == "Cote d'Ivoire" ~ "Côte d'Ivoire", 
                             Country == "DRC" ~ "Democratic Republic of the Congo",
                             Country == "Congo (Republic of)" ~ "Republic of Congo",
                             Country == "Eq. Guinea" ~ "Equatorial Guinea",
                             Country == "Eswatini" ~ "Swaziland",
                             Country == "Gambia" ~ "The Gambia",
                             Country == "Sao Tome and Principe" ~ "São Tomé and Principe",
                             Country == "United Republic of Tanzania" ~ "Tanzania",
                             TRUE ~ Country))

# replace spaces with period in all column names
names(LL_raw) <- str_replace_all(names(LL_raw), " ", ".")

# renaming mislabelled countries to merge with map polygon later
country_info <- country_info %>% 
  mutate(Country = case_when(Country == "CAF" ~ "Central African Republic",
                             Country == "Congo (Rep)" ~ "Republic of Congo",
                             Country == "Cote d'Ivoire" ~ "Côte d'Ivoire", 
                             Country == "DRC" ~ "Democratic Republic of the Congo",
                             Country == "Eq. Guinea" ~ "Equatorial Guinea",
                             Country == "Eswatini" ~ "Swaziland",
                             Country == "Gambia" ~ "The Gambia",
                             Country == "Sao Tome e P" ~ "São Tomé and Principe",
                             TRUE ~ Country),
         Region = if_else(Region == "Easterb Africa", "Eastern Africa", Region))

# Store as LL_format to fix a bunch of issues
LL_format <- LL_raw %>%
  mutate(Outcome = case_when(Outcome == "dead" ~ "Dead",
                             Outcome == "recovered" ~ "Recovered",
                             TRUE ~ Outcome)) %>%
  # impute dates of death
  mutate(Date.of.Death = case_when(is.na(Outcome) ~ "NA", # if no OUTCOME, Date of Death should be NA
                                   # if dead and no recorded date of death, 
                                   Outcome == "Dead" & is.na(Date.of.Death) ~ 
                                     # plug in their date of discharge, or report date, if missing
                                     ifelse(!is.na(Date.of.Discharge), as.character(Date.of.Discharge), 
                                            as.character(Reporting_Date)),
                                   TRUE ~ as.character(Date.of.Death))) %>%
  # Recode Dates of Discharge as above
  mutate(Date.of.Discharge = case_when(is.na(Outcome) ~ "NA",
                                       Outcome == "Recovered" & is.na(Date.of.Discharge) ~ 
                                         ifelse(!is.na(Date.of.Discharge), as.character(Date.of.Death), 
                                                as.character(Reporting_Date)),
                                       TRUE ~ as.character(Date.of.Discharge))) %>%
  # remove death and recovered dates from individuals neither dead nor recovered
  mutate(Date.of.Death = if_else(Outcome != "Dead", NA_character_, Date.of.Death),
         Date.of.Discharge = if_else(Outcome != "Dead", NA_character_, Date.of.Discharge)) %>%
  # Clip data to last date requested
  filter(Reporting_Date <= lastdate) %>%
  # if death occurred after lastdate, patient is still alive
  mutate(Outcome = if_else(Outcome == "Dead" & Date.of.Death > lastdate, "Alive", Outcome),
         # removing dates of death that occurred after lastdate
         Date.of.Death = if_else(Date.of.Death > lastdate, NA_character_, Date.of.Death),
         Outcome = if_else(Outcome == "Recovered" & Date.of.Discharge > lastdate, "Alive", Outcome),
         Date.of.Discharge = if_else(Date.of.Discharge > lastdate, NA_character_, Date.of.Discharge)) %>%
  # format dates back to dates.
  mutate(Reporting_Date = as.Date(Reporting_Date),
         Date.of.Death = as.Date(Date.of.Death),
         Date.of.Discharge = as.Date(Date.of.Discharge)) %>%
  # replicate rows with multiple recorded NumCases, then change counts to 1
  uncount(NumCases, .remove = FALSE) %>%
  mutate(NumCases = 1)

# The slightly cleaned linelist is now stored in the object called "df_LL"
df_LL <- LL_format

# selects region for comparisons. Change my_country to change
my_region <- country_info %>% 
  filter(Country == my_country) %>% 
  select(Region) %>% 
  pull()


##############################################################
##                                                          ##
##      BUILDING EPICURVE DFs FOR REGIONAL COMPARISONS      ##
##                                                          ##
##############################################################

# Expand dates per country
all_dates <- df_LL %>% 
  # complete to include all dates, even whe case was not recorded
  complete(Reporting_Date = seq.Date(min(Reporting_Date), max(Reporting_Date), by = "day")) %>%
  # expand to include all dates per country 
  tidyr::expand (Reporting_Date, Country) %>% 
  # arrange to make things clearer
  arrange(Country, Reporting_Date) %>% 
  # faux variables for death and discharge; needed to bind with data frames later
  mutate(Date.of.Death = Reporting_Date,
         Date.of.Discharge = Reporting_Date)

# Calculate epicurve for cases
reported_country <- df_LL %>%  
  bind_rows(all_dates) %>%
  select(Reporting_Date, Country, NumCases) %>%
  group_by(Country, Reporting_Date) %>%
  summarise(Confirmed_this_day = sum(NumCases, na.rm = TRUE))

# Calculate epicurve for deaths as above
deaths_country <- df_LL %>% 
  bind_rows(all_dates) %>% 
  select(Date.of.Death, Country, NumCases) %>%
  group_by(Country, Date.of.Death) %>%
  summarise(Deaths_this_day = sum(NumCases, na.rm = TRUE)) %>% 
  mutate(Reporting_Date = Date.of.Death)

# Calculate epicurve for discharges as above
discharges_country <- df_LL %>% 
  bind_rows(all_dates) %>% 
  select(Date.of.Discharge, Country, NumCases) %>%
  group_by(Country, Date.of.Discharge) %>%
  summarise(Discharges_this_day = sum(NumCases, na.rm = TRUE)) %>% 
  mutate(Reporting_Date = Date.of.Discharge)

# Combine into single epicurve
df_country <- inner_join(reported_country, deaths_country,
                         by= c("Reporting_Date", "Country"))  %>% 
  inner_join(discharges_country,
             by= c("Reporting_Date", "Country")) %>% 
  # sort by date within each country 
  arrange(Country, Reporting_Date) %>% 
  # delete useless vars
  select(Reporting_Date, Country, Confirmed_this_day, Deaths_this_day, Discharges_this_day) %>% 
  dplyr::rename(Cases_this_day = Confirmed_this_day) %>% 
  # add in EpiWeek 
  mutate(Epiweek = lubridate::epiweek(Reporting_Date)) %>% 
  # Calculate cumulative totals
  group_by(Country) %>% 
  mutate(Cum_cases = cumsum(Cases_this_day),
         Cum_deaths = cumsum(Deaths_this_day),
         Cum_discharges = cumsum(Discharges_this_day),
         Active_cases = Cum_cases - Cum_deaths - Cum_discharges) %>% 
  # remove NA countries (artifact of expansion I think)
  filter(!is.na(Country)) %>% 
  # Calculate crude CFR at each time point
  group_by(Country) %>% 
  mutate(CFR = round(100 * Cum_deaths / Cum_cases, digits = 1)) %>% 
  # Calculate rolling 7 day sums ( past week of cases and deaths)
  mutate(Cases_past_week = rollsum(x = Cases_this_day, k = 7, align = "right",  
                                   fill = na.fill(Cases_this_day, 0)) ,
         Deaths_past_week=rollsum(x = Deaths_this_day, k = 7, align = "right",  
                                  fill = na.fill(Deaths_this_day, 0))) %>% 
  left_join(country_info, by = "Country") %>% 
  mutate(Cases_per_million = round((Cum_cases / Population) * 1e6, digits = 1),
         Deaths_per_million = round((Cum_deaths / Population) * 1e6, digits = 1)) %>% 
  # daily cases and deaths per million
  mutate(Cases_per_million_daily = round((Cases_this_day / Population) * 1e6, digits = 1),
         Deaths_per_million_daily = round((Deaths_this_day / Population) * 1e6, digits = 1)) %>% 
  # daily cases and deaths per million smoothed
  mutate(Cases_per_million_daily_smooth = rollmean(Cases_per_million_daily, k = 7, 
                                                   fill = na.fill(Cases_per_million_daily, "extend")),
         Deaths_per_million_daily_smooth = rollmean(Deaths_per_million_daily, k = 7, 
                                                    fill = na.fill(Deaths_per_million_daily, "extend"))) %>% 
  ungroup() 


# regional epicurve (for African regions)
df_region <- df_country %>% 
  select(Reporting_Date, Country, Region, Population, 
         Cases_this_day, Deaths_this_day, Discharges_this_day) %>% 
  arrange(Region, Reporting_Date) %>% 
  # sum up cases for all countries in given region for a given day
  group_by(Region, Reporting_Date) %>%
  mutate(Cases_this_day = sum(Cases_this_day),
         Deaths_this_day = sum(Deaths_this_day),
         Discharges_this_day = sum(Discharges_this_day),
         Population = sum(Population),
         Country = NULL) %>% 
  # discard unneeded rows
  slice(1) %>% 
  # calculate cumulatives
  group_by(Region) %>% 
  mutate(Cum_cases = cumsum(Cases_this_day),
         Cum_deaths = cumsum(Deaths_this_day),
         Cum_discharges = cumsum(Discharges_this_day),
         Active_cases = Cum_cases - Cum_deaths - Cum_discharges) %>% 
  # cases and deaths per million
  mutate(Cases_per_million = round((Cum_cases/Population) * 1e6, digits = 1),
         Deaths_per_million = round((Cum_deaths/Population) * 1e6, digits = 1))


###################################################
##                                               ##
##     Creating x-axis for African epicurves     ##
##                                               ##
###################################################

# The following will need to be added to each ggplot to use these labels
# + scale_x_date(breaks = coveredsundays,
#               labels = epiweekanddate)
daysinrange <- seq(min(na.omit(df_LL$Reporting_Date)), 
                   max(na.omit(df_LL$Reporting_Date)),
                   by = "days")

# Subset just the Sundays. Epiweeks start on Sunday, so we will need these to assign labels
coveredsundays <- daysinrange[lubridate::wday(daysinrange) == 1]

# Identify the epiweek that each sunday belongs to
epiweeks <- lubridate::epiweek(coveredsundays) 

# Extract a nicely formatted date for each Epiweek sunday start
epiweeksundaydate <- format.Date(coveredsundays, format = "%b %d")
epiweekanddate <- paste0(epiweeksundaydate, "\n", "(", "EW", epiweeks, ")")
# epiweekanddate is our final output
# check that all is well with : as.data.frame(coveredsundays,epiweekanddate)

coveredsundays <- rev(coveredsundays)
epiweekanddate <- rev(epiweekanddate)

# every other sunday. For tight plots
coveredsundays2 <- coveredsundays[c(T,F)] 
epiweekanddate2 <- epiweekanddate[c(T,F)]

# every third sunday. For really tight plots. Add aditional FALSES for tighter plots
coveredsundays3 <- coveredsundays[c(T,F,F)] 
epiweekanddate3 <- epiweekanddate[c(T,F,F)]

coveredsundays4 <- coveredsundays[c(T,F,F,F)] 
epiweekanddate4 <- epiweekanddate[c(T,F,F,F)]

coveredsundays5 <- coveredsundays[c(T,F,F,F,F)] 
epiweekanddate5 <- epiweekanddate[c(T,F,F,F,F)]


##########################################################
##                                                      ##
##  CREATING TABLES FOR FIGURES AND MAPS AND TABLES     ##
##                                                      ##
##########################################################

all_country_tab <- df_country %>% 
  group_by(Country) %>% 
  slice(which.max(Reporting_Date)) %>% 
  dplyr::rename(Cases = Cum_cases, Deaths = Cum_deaths) %>% 
  mutate(`Crude \nCFR (%)` = round(100 * Deaths / Cases, digits = 1)) %>% 
  mutate(`Cases per million` = (Cases / Population) * 1e6,
         `Deaths per million` = (Deaths / Population) * 1e6) %>% 
  select(Country, Cases,`Cases per million`, Deaths, `Deaths per million`) %>% 
  arrange(-Cases) %>% 
  left_join(country_info) %>% 
  ungroup()


regional_tab <- df_country %>% 
  select(Reporting_Date, Country, Cum_cases, Cum_deaths, Region, Population) %>% 
  group_by(Country) %>% 
  slice(which.max(Reporting_Date)) %>% 
  group_by(Region) %>% 
  mutate(Cum_cases_region = sum(Cum_cases),
         Cum_deaths_region = sum(Cum_deaths),
         Population = sum(Population)) %>% 
  slice(which.max(Reporting_Date)) %>% 
  rename(Cases = Cum_cases_region, Deaths = Cum_deaths_region) %>% 
  mutate(`Crude \nCFR (%)` = round(100 * Deaths / Cases, digits = 1)) %>% 
  mutate(`Cases per million` = (Cases / Population) * 1e6,
         `Deaths per million` = (Deaths / Population) * 1e6) %>% 
  select(Region, Cases,`Cases per million`, Deaths, `Deaths per million`) %>% 
  arrange(-Cases) %>% 
  ungroup()


######################################################
##                                                  ##
##      MAPS FOR CONTINENTAL AND INSULAR AFRICA     ##
##                                                  ##
######################################################

# Import and merging Africa map from natural earth package (including small insular countries)
africa_map <- subset(rnaturalearthdata::countries50, region_un == "Africa" & type == "Sovereign country")

# Extracting centroids
centroids_df <- data.frame(coordinates(africa_map)) %>%
  rename(X = X1,
         Y = X2)

# Inserting centroids into continental map
africa_union <- cbind(africa_map, centroids_df)

# Importing 
asia_map <- ne_countries(continent = "asia", returnclass = "sf") %>% 
  mutate(iso_code = adm0_a3_is) %>% 
  st_set_crs(4326)  

europe_map <- ne_countries(continent = "europe", returnclass = "sf") %>% 
  mutate(iso_code = adm0_a3_is) %>% 
  st_set_crs(4326)  

# join africa geoms with the COVID info about countries
africa_map <- africa_union %>% 
  st_as_sf() %>%
  mutate(name_long = case_when(name_long == "Cape Verde" ~ "Cabo Verde",
                               TRUE ~ name_long)) %>%
  rename(Country = name_long) %>%
  left_join(all_country_tab, by = "Country")

# find the centroid of each country
africa_points <- cbind(africa_map, st_coordinates(st_centroid(africa_map$geometry)))


##########################################################################
##                                                                      ##
##      Extract and creating categories for cases per million maps      ##
##                                                                      ##
##########################################################################

# extract breaks for the sextiles from the data
# this is a very roundabout way to do this.
breaks_cases_per_million <- africa_map %>% 
  # force drop geometry 
  st_set_geometry(NULL) %>% 
  select(`Cases per million`) %>%
  bind_cols(sextile = ntile(.$`Cases per million`, 6)) %>% 
  group_by(sextile) %>% 
  arrange(sextile) %>% 
  slice(c(which.min(`Cases per million`), 
          which.max(`Cases per million`))) %>% 
  ungroup %>% 
  select(`Cases per million`) %>% 
  pull() %>% 
  round(1) 

# delete as needed to extract only the sextile boundaries
breaks_cases_per_million <- breaks_cases_per_million[c(T, T, F, T, F, T, F, T, F, T, F, T)]

# add cushioning, otherwise the top and bottom countries gets dropped.
breaks_cases_per_million[1] <- breaks_cases_per_million[1] - 0.1
breaks_cases_per_million[7] <- breaks_cases_per_million[7] + 0.1
breaks_cases_per_million <- round(breaks_cases_per_million, 2)

# create labels for the quintile values
labels_cases_per_million <- africa_map %>% 
  mutate(cases_rate_cat = cut(`Cases per million`,
                              breaks = breaks_cases_per_million, 
                              right = FALSE, dig.lab = 8)) %>% 
  select(cases_rate_cat) %>%
  # force drop geometry. Messy. Will fix when brain is working
  st_set_geometry(NULL) %>% 
  unique() %>% pull() %>% 
  gtools::mixedsort() %>% 
  as.character()

labels_cases_per_million <- labels_cases_per_million[!is.na(labels_cases_per_million)]
labels_cases_per_million <- gsub(",", " - " ,labels_cases_per_million)
labels_cases_per_million[6] <- gsub(")", "]" ,labels_cases_per_million[6])


###########################################################################
##                                                                       ##
##      Extract and creating categories for deaths per million maps      ##
##                                                                       ##
###########################################################################

# extract breaks for the sextiles from the data
# this is a very roundabout way to do this.
breaks_deaths_per_million <- africa_map %>% 
  st_set_geometry(NULL) %>% 
  select(`Deaths per million`) %>%
  bind_cols(sextile = ntile(.$`Deaths per million`, n = 6)) %>% 
  group_by(sextile) %>% arrange(sextile) %>% 
  slice(c(which.min(`Deaths per million`), 
          which.max(`Deaths per million`))) %>% 
  ungroup %>% 
  select(`Deaths per million`) %>% 
  pull() %>% 
  round(1) 

breaks_deaths_per_million <- breaks_deaths_per_million[c(T,T,F,T,F,T,F,T,F,T,F,T)]

# needed otherwise the top country gets dropped. 
# breaks_deaths_per_million[1] <- breaks_deaths_per_million[1] - 0.1
breaks_deaths_per_million[7] <- breaks_deaths_per_million[7] + 0.1


# create labels for the sextile values
labels_deaths_per_million <- africa_map %>% 
  mutate(death_rate_cat = cut(`Deaths per million`,
                              breaks = breaks_deaths_per_million, 
                              right = F, dig.lab = 8)) %>% 
  select(death_rate_cat) %>% 
  # force drop geometry. Messy. Will fix when brain is working
  st_set_geometry(NULL) %>% 
  unique() %>% 
  pull() %>% 
  gtools::mixedsort() %>% 
  as.character()

labels_deaths_per_million <- labels_deaths_per_million[!is.na(labels_deaths_per_million)]
labels_deaths_per_million <- gsub(",", " - " ,labels_deaths_per_million)
labels_deaths_per_million[6] <- gsub(")", "]" ,labels_deaths_per_million[6])


################################################
##                                            ##
##      SECTION 1 GRAPHS, MAPS AND TABLES     ##
##                                            ##
################################################

#######################################################
##                                                   ##
##      Daily absolute cases plot for my_country     ##
##                                                   ##
#######################################################

df_country %>% 
  filter(Country == my_country) %>% 
  select(Reporting_Date, Cases_this_day) %>% 
  mutate(seven_day_avg = rollmean(x = Cases_this_day, k = 7, align = "right",  
                                  fill = na.fill(Cases_this_day, 0)),
         fourteen_day_avg = rollmean(x = Cases_this_day, k = 14, align = "right",  
                                     fill = na.fill(Cases_this_day, 0))) %>% 
  ggplot() +
  geom_bar(aes(x = Reporting_Date, y = Cases_this_day), stat = "identity", 
           fill = "#1B9E77", colour = alpha("white", 0.05)) +
  geom_line(aes(x = Reporting_Date, y = seven_day_avg, lty = "7-day rolling average")) +
  geom_line(aes(x = Reporting_Date, y = fourteen_day_avg, lty = "14-day rolling average")) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_country$Reporting_Date, na.rm = TRUE) - 1, 
                          max(df_country$Reporting_Date, na.rm = TRUE))) +
  labs(x = "Date of Reporting", y = "Absolute number of COVID-19 cases") +
  ggtitle(paste("Daily COVID-19 cases in", my_country)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0.3)))

########################################################
##                                                    ##
##      Daily absolute deaths plot for my_country     ##
##                                                    ##
########################################################

df_country %>% 
  filter(Country == my_country) %>% 
  select(Reporting_Date, Deaths_this_day) %>% 
  mutate(seven_day_avg = rollmean(x = Deaths_this_day, k = 7, align = "right",
                                  fill = na.fill(Deaths_this_day, 0)),
         fourteen_day_avg = rollmean(x = Deaths_this_day, k = 14, align = "right",
                                     fill = na.fill(Deaths_this_day, 0))) %>% 
  ggplot() +
  geom_bar(aes(x = Reporting_Date, y = Deaths_this_day), stat = "identity",
           fill = "#D95F02", colour = alpha("white", 0.05)) +
  geom_line(aes(x = Reporting_Date, y = seven_day_avg, lty = "7-day rolling average")) +
  geom_line(aes(x = Reporting_Date, y = fourteen_day_avg, lty = "14-day rolling average")) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_country$Reporting_Date, na.rm = TRUE) - 1, 
                          max(df_country$Reporting_Date, na.rm = TRUE)))+
  labs(x = "Date of Reporting", y = "Absolute number of COVID-19 deaths") +
  ggtitle(paste("Daily COVID-19 Deaths in", my_country) )  +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0.3)))


################################################################
##                                                            ##
##      Confirmed cases and deaths table for your country     ##
##                                                            ##
################################################################

# edit and transpose table for better printing
epi_table <- df_country %>% 
  filter(Country == my_country) %>% 
  select(Reporting_Date, Cases_this_day, Deaths_this_day)  %>% 
  rename(Date = Reporting_Date, 
         Cases = Cases_this_day,
         Deaths = Deaths_this_day) %>% 
  mutate(Date = format.Date(Date, format = "%b%d")) %>% 
  t() %>% 
  as_tibble(.name_repair = "unique")

# column names and row names
names(epi_table) <- epi_table %>% 
  slice(1) %>% 
  unlist()

epi_table <- epi_table %>% 
  slice(-1) %>% 
  as.data.frame()

# inserting row names
rownames(epi_table) <- c("Cases", "Deaths")

# adjusting pander options
panderOptions('table.continues', '')
panderOptions('table.caption.prefix', '')
panderOptions('table.style', 'multiline')

# pander print
pander(epi_table, split.table = 150, split.cells = 1.2)

# summary statistics for confirmed cases
pander(df_country %>%
         filter(Country == my_country) %>% 
         summarise(`Cases (mean)` = mean(Cases_this_day, na.rm = TRUE),
                   `Cases (median)` = median(Cases_this_day, na.rm = TRUE),
                   `Cases (min)` = min(Cases_this_day, na.rm = TRUE),
                   `Cases (max)` = max(Cases_this_day, na.rm = TRUE),
                   `Cases (std. dev)` = sd(Cases_this_day, na.rm = TRUE),
                   `Cases (total)` = sum(Cases_this_day, na.rm = TRUE)))

# summary statistics for deaths
pander(df_country %>%
         filter(Country == my_country) %>% 
         summarise(`Deaths (mean)` = mean(Deaths_this_day, na.rm = TRUE),
                   `Deaths (median)` = median(Deaths_this_day, na.rm = TRUE),
                   `Deaths (min)` = min(Deaths_this_day, na.rm = TRUE),
                   `Deaths (max)` = max(Deaths_this_day, na.rm = TRUE),
                   `Deaths (std. dev)` = sd(Deaths_this_day, na.rm = TRUE),
                   `Deaths (total)` = sum(Deaths_this_day, na.rm = TRUE)))


##############################################
##                                          ##
##      GROWTH RATE OF CASES OVER TIME      ##
##                                          ##
##############################################

# growth rate of reported cases df
growth_rate_tab <- df_country %>% 
  filter(Country == my_country) %>% 
  select(Reporting_Date, Cases_past_week, Epiweek) %>% 
  group_by(Epiweek) %>% 
  # take the last day of each epiweek
  slice(which.max(Reporting_Date)) %>% 
  ungroup() %>% 
  # Cases in the past week vs cases two weeks ago
  mutate(diff_cases = Cases_past_week - lag(Cases_past_week,1), 
         week_growth = diff_cases/lag(Cases_past_week,1),
         week_growth_perc = 100 * week_growth, 
         # formula to convert weekly_growth to daily_growth equivalent
         growth = (((1 + week_growth) ^ (1/7)) - 1), 
         growth_perc = 100 * growth)

# growth rate of cases plot
ggplot(growth_rate_tab) +
  # line tracking the past 14 day changes.
  geom_line(aes(x = Reporting_Date, y = week_growth_perc ), 
            color = alpha("deepskyblue", 0.5), size = 1)  +
  geom_point(aes(x = Reporting_Date, y = week_growth_perc), 
             color = alpha("deepskyblue", 1), size = 1.5)  +
  coord_cartesian(ylim=c(-100, 500)) +
  # shade area above line red
  geom_area(aes(y = Inf, x = Reporting_Date), fill = alpha("#f79f57", 0.3)) +
  # share area below line green
  geom_area(aes(y = -Inf, x = Reporting_Date), fill = alpha("#56bfa3", 0.3)) +
  ggtitle(paste0("Week-on-week growth rate of new COVID-19 cases in ", my_country) , 
          subtitle = "Growth rate is the % change in new cases in the past week relative to the cases in the previous week") +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3)+
  labs(y = "Average daily growth rate (%) each week", x = "") +
  labs(caption = "Falls in growth rate at the end of the epicurve should be interpreted with caution. \nThis drop is often caused by case reporting lag.")

# growth rate of cases table
growth_rate_tab_prep <- growth_rate_tab %>% 
  select(Epiweek, Reporting_Date, Cases_past_week, week_growth_perc) %>%
  # replace either with 0, or with the percent formatted value
  mutate(week_growth_perc = ifelse(!is.na(week_growth_perc), week_growth_perc, 0)) %>% 
  mutate(week_growth_perc = round(week_growth_perc, 1)) %>% 
  mutate(Reporting_Date = format.Date(Reporting_Date, "%b %d")) %>% 
  rename(`Last day of week` = Reporting_Date,
         `Cases in past week` = Cases_past_week,
         `% Change from prior week` = week_growth_perc)

growth_rate_format_tab <- formattable(growth_rate_tab_prep, 
                                      list(`Cases in past week` = color_bar2("orange"), 
                                           `% Change from prior week` = growth_formatter))

# export table
export_formattable(growth_rate_format_tab, file = "Tab_GrowthRate_CasesReported.png")


#######################################################
##                                                   ##
##      REGIONAL CASE COMPARISONS PREP FOR PLOT      ##
##                                                   ##
#######################################################

# select comparison countries to label. You can select these manually as below
# and then comment out the next few lines
# countries_to_label <- c(my_country, "Nigeria","Mauritania","Cameroon", "Guinea-Bissau")

# otherwise the script labels the top 5 countries in region with most cumulative cases per million
countries_to_label <- df_country %>% 
  filter(Region == my_region) %>% 
  group_by(Country) %>% 
  top_n(1, wt = Reporting_Date) %>% 
  ungroup() %>% 
  top_n(n = 5,wt = Cases_per_million) %>% 
  select(Country) %>% 
  pull()

# subset df_country according to needed labels
country_comparisons <- df_country %>% 
  filter(Country %in% countries_to_label |
           Country == my_country)  %>% 
  # paste region and cases at maxdate. Needed for plot
  group_by(Country) %>% 
  mutate(label_max_cases = if_else(Reporting_Date == max(Reporting_Date, na.rm=T),
                                   paste(as.character(Country), "\n",  
                                         max(Cases_per_million, na.rm = T)) ,
                                   NA_character_),
         label_max_deaths = if_else(Reporting_Date == max(Reporting_Date, na.rm=T),
                                    paste(as.character(Country), "\n",  
                                          max(Deaths_per_million, na.rm = T)) ,
                                    NA_character_))

###########################################################################################
##                                                                                       ##
##      Time series of cases and deaths per million in countries of the same region      ##
##                                                                                       ##
###########################################################################################

# comparison of incidence per million inhabitants with countries of the same region
df_country %>% 
  filter(Region == my_region)%>% 
  select(Reporting_Date, Cases_per_million, Country) %>% 
  ggplot() +
  geom_line(aes(x = Reporting_Date, y = Cases_per_million, group = Country),
            colour = alpha("black",0.6 ), size = 0.4) +
  geom_line(data = country_comparisons,
            aes(x = Reporting_Date, y = Cases_per_million, colour = Country),
            size = 1) +
  geom_label_repel(data = country_comparisons,
                   aes(x = Reporting_Date, y = Cases_per_million, 
                       colour = Country, label = label_max_cases),
                   na.rm = TRUE, size = 2.8, fill = alpha("white", 0.8),
                   fontface = "bold") +
  scale_color_manual(values= my_palette) +
  ggtitle(paste("Cumulative cases per million for countries in", my_region),
          paste(my_country, "and select others highlighted")) +
  scale_x_date(breaks = coveredsundays4,
               labels = epiweekanddate4,
               limits = c(min(df_country$Reporting_Date, na.rm = TRUE), NA))+
  labs(x = "Date of Reporting", y = "Cases per million") +
  theme(legend.position = "none")


# comparison of mortality per million inhabitants with countries of the same region
df_country %>% 
  filter(Region == my_region)%>% 
  select(Reporting_Date, Deaths_per_million, Country) %>% 
  ggplot() +
  geom_line(aes(x = Reporting_Date, y = Deaths_per_million, group = Country),
            colour = alpha("black",0.6 ), size = 0.4) +
  geom_line(data = country_comparisons,
            aes(x = Reporting_Date, y = Deaths_per_million, colour = Country),
            size = 1) +
  geom_label_repel(data= country_comparisons,
                   aes(x = Reporting_Date, y = Deaths_per_million, 
                       colour = Country, label = label_max_deaths),
                   na.rm = TRUE, size = 2.8, fill = alpha("white", 0.8),
                   fontface = "bold") +
  scale_color_manual(values = my_palette) +
  ggtitle(paste("Cumulative deaths per million for countries in", my_region),
          paste(my_country, "and select others highlighted")) +
  scale_x_date(breaks = coveredsundays4,
               labels = epiweekanddate4,
               limits = c(min(df_country$Reporting_Date, na.rm = TRUE), NA))+
  labs(x = "Date of Death", y = "Deaths per million") +
  theme(legend.position = "none")


#################################################################
##                                                             ##
##      Table of cumulative cases and deaths in my region      ##
##                                                             ##
#################################################################

# table for my region
regional_tab <- df_country %>% 
  select(Reporting_Date, Country, Cum_cases, Cum_deaths, Region, Population) %>% 
  group_by(Country) %>% 
  slice(which.max(Reporting_Date)) %>% 
  group_by(Region) %>% 
  mutate(Cum_cases_region = sum(Cum_cases),
         Cum_deaths_region = sum(Cum_deaths),
         Population = sum(Population)) %>% 
  slice(which.max(Reporting_Date)) %>% 
  rename(Cases = Cum_cases_region, Deaths = Cum_deaths_region) %>% 
  mutate(`Crude \nCFR (%)` = round(100 * Deaths/Cases, digits = 1)) %>% 
  mutate(`Cases per million` = (Cases/Population) * 10E5,
         `Deaths per million` = (Deaths/Population) * 10E5) %>% 
  select(Region, Cases,`Cases per million`, Deaths, `Deaths per million`) %>% 
  arrange(-Cases) %>% 
  ungroup()

# pander options
panderOptions('table.continues', '')
panderOptions('table.caption.prefix', '')
panderOptions('table.style', 'rmarkdown')

# bold for my region
emphasize.strong.rows(which(regional_tab == my_region, arr.ind = T)[1])

# build table
pander(regional_tab, split.table = 1000, split.cells = 8)


##############################################################################
##                                                                          ##
##      Table of cumulative cases and deaths in countries in my region      ##
##                                                                          ##
##############################################################################

# table for countries in my region
country_tab_my_region <- all_country_tab %>%
  mutate(`Crude CFR` = round((100* Deaths/Cases), digits = 1),
         `CasesPerMil` = round(`Cases per million`, 1),
         `Deaths per million` = round(`Deaths per million`, 1)) %>% 
  left_join(country_info) %>% 
  filter(Region == my_region) %>% 
  select(Country, Cases, Deaths, `Crude CFR`, Population, 
         `Cases per million`, `Deaths per million`) %>% 
  as_tibble()

# pander options
panderOptions('table.continues', '')
panderOptions('table.caption.prefix', '')
panderOptions('table.style', 'rmarkdown')

# bold for my country
emphasize.strong.rows(which(country_tab_my_region == my_country, arr.ind = T)[1])

# build table
pander(country_tab_my_region, split.table = 1000, split.cells = 8)

# plot for countries in my region
country_tab_my_region %>% 
  mutate(Cases_alone = Cases - Deaths) %>% 
  select(Country, Cases_alone, Deaths, `Crude CFR`) %>% 
  pivot_longer( cols = c(Cases_alone, Deaths), names_to = "Classification", values_to = "Value") %>% 
  mutate(Classification = fct_rev(Classification)) %>% 
  mutate(Country = fct_reorder(Country, Value)) %>% 
  ggplot() + 
  geom_bar(aes(x = Country, y = Value, fill = Classification), stat = "identity", position = "stack") +
  geom_text(data = country_tab_my_region, aes(x = Country, y = Cases,
                                              label = paste0("CFR: ", `Crude CFR`, "%", 
                                                             " (", Deaths, " of ", Cases, ")")), 
            size = 2.5, hjust = 0, colour = "#3758a6") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0,0.4))) +
  scale_fill_manual(values = c("red", "#56B4E9"), labels = c("Deaths", "Cases")) +
  theme(legend.title = element_blank()) +
  ggtitle(paste("Cases, deaths and crude case-fatality ratio\nacross countries in", my_region)) +
  labs(x = "Region", y = "Count")



##################################################################
##                                                              ##
##      Map of cumulative cases per million continent-wide      ##
##                                                              ##
##################################################################

africa_map %>% 
  # add in case categories. Breaks and labels are sextiles.
  # defined in the analysis script
  mutate(cases_rate_cat = cut(`Cases per million`,
                              breaks = breaks_cases_per_million,
                              labels = labels_cases_per_million,
                              right = F, dig.lab = 8)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = cases_rate_cat),
          color = "white", size = 0.05) +
  # add in asia and europe maps
  geom_sf(data = asia_map, color = "white", size = 0.2) +
  geom_sf(data = europe_map, color = "white", size = 0.2) +
  # ggrepel package prevents overlap
  geom_text_repel(data = africa_points, aes(x = X, y = Y, label = name),
                  size = 2.1, colour = "black", box.padding = 0, 
                  fontface = "bold") +
  ggtitle("Cumulative cases per million in WHO AFRO countries",
          subtitle = paste0("As at ", 
                            format.Date(max(df_country$Reporting_Date, na.rm = T), '%B %d, %Y'),
                            ", epidemiological week ", 
                            max(df_country$Epiweek, na.rm = TRUE))) +
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_line(linetype = "dashed"),
        axis.line.x.bottom = element_blank()) +
  scale_fill_brewer("Cases\nper million",type = "seq",
                    palette = "OrRd", direction = 1,
                    na.value = "lightgrey", aesthetics = "fill",
                    guide = "legend", drop = FALSE) +
  annotate(geom = "text", x = -0, y = -10, label = "Atlantic\nOcean",
           color = "#399fe3", size = 2, fontface = "italic") +
  annotate(geom = "text", x = 48, y = -6, label = "Indian\nOcean", 
           color = "#399fe3", size = 2, fontface = "italic") +
  labs(x = "longitude", y = "latitude",
       caption = "The boundaries and names used on this map do not imply \nthe expression of any opinion whatsoever concerning the legal \nstatus of territories.") +
  lims(x = c(-22, 56), y = c(-40, 38)) +
  theme(legend.position = c(0.2, 0.28) ,  
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6),
        plot.caption = element_text(size = 5, color = "gray50", hjust = 1),
        legend.key.size = unit(0.9,"line"))


########################################################
##                                                    ##
##      Map of deaths per million continent-wide      ##
##                                                    ##
########################################################

africa_map %>% 
  # add in case categories
  mutate(death_rate_cat = cut(`Deaths per million`,
                              breaks = breaks_deaths_per_million,
                              labels = labels_deaths_per_million,
                              right = F, dig.lab = 8)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = death_rate_cat),
          color = "white", size = 0.05) +
  geom_sf(data = asia_map, color = "white", size = 0.1) +
  geom_sf(data = europe_map, color = "white", size = 0.1) +
  geom_text_repel(data = africa_points, aes(x = X, y = Y, label = name),
                  size = 2.1, colour = "black", box.padding = 0, 
                  fontface = "bold") +
  ggtitle("Cumulative deaths per million in WHO AFRO countries",
          subtitle = paste0("As at ", 
                            format.Date(max(df_country$Reporting_Date, na.rm = T), '%B %d, %Y'),
                            ", epidemiological week ", 
                            max(df_country$Epiweek, na.rm = TRUE))) +
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_line(linetype = "dashed"),
        axis.line.x.bottom = element_blank()) +
  scale_fill_brewer("Deaths\nper million", type = "seq",
                    palette = "OrRd", direction = 1,
                    na.value = "lightgrey", aesthetics = "fill",
                    guide = "legend", drop = FALSE) +
  annotate(geom = "text", x = -0, y = -10, label = "Atlantic\nOcean",
           color = "#399fe3", size = 2, fontface = "italic") +
  annotate(geom = "text", x = 48, y = -6, label = "Indian\nOcean", 
           color = "#399fe3", size = 2, fontface = "italic") +
  labs(x = "longitude", y = "latitude",
       caption = "The boundaries and names used on this map do not imply \nthe expression of any opinion whatsoever concerning the legal \nstatus of territories.") +
  lims(x = c(-22, 56), y = c(-40, 38)) +
  theme(legend.position = c(0.2, 0.28) ,  
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6),
        plot.caption = element_text(size = 5, color = "gray50", hjust = 1),
        legend.key.size = unit(0.9,"line"))


#############################################################################
##                                                                         ##
##      Table of cumulative cases and deaths in all African countries      ##
##                                                                         ##
#############################################################################

country_tab_p <- all_country_tab %>%
  mutate(`Crude CFR` = round((100* Deaths/Cases), digits = 1),
         `Cases per million` = round(`Cases per million`, 1),
         `Deaths per million` = round(`Deaths per million`, 1)) %>% 
  left_join(country_info) %>% 
  # select(-c(iso_code, Region)) %>% 
  select(Country, Cases, Deaths, `Crude CFR`, Population, 
         `Cases per million`, `Deaths per million`) %>% 
  as_tibble()

# pander options
panderOptions('table.continues', '')
panderOptions('table.caption.prefix', '')
panderOptions('table.style', 'multiline')

# bold header
names(country_tab_p) <- pandoc.strong.return(names(country_tab_p))

# bold for my country
emphasize.strong.rows(which(country_tab_p == my_country, arr.ind = T)[1])

# build table
pander(country_tab_p, split.table = 1000, split.cells = 8)


##########################################################
##########################################################
##                                                      ##
##                                                      ##
##                  CODE FOR SECTION 2                  ##
##                                                      ##
##                  WHERE WE USE THE                    ##
##                                                      ##
##                    COUNTRY SPECIFIC                  ##
##                        LINELIST                      ##
##                                                      ##
##                                                      ##
##########################################################
##########################################################


################################################
##                                            ##
##      Loading country-specific linelist     ##
##                                            ##
################################################

# Importing clean DB (UTF-8 encoding)
df <- read.csv(paste0(clean_csv_path, "niger_clean.csv"), encoding = "UTF-8") %>%
  # removing additional information-less rows in the df
  filter(!is.na(report_date)) %>%
  # Transforming all dates properly
  mutate(report_date = as.Date(report_date),
         consultation_dateHF = as.Date(consultation_dateHF),
         Lab_datetaken = as.Date(Lab_datetaken),
         Lab_resdate = as.Date(Lab_resdate),
         patcourse_dateonset = as.Date(patcourse_dateonset),
         patcourse_datedeath = as.Date(patcourse_datedeath),
         patcourse_datedischarge = as.Date(patcourse_datedischarge),
         # logical variable = was case reported or no (necessary because of the missing dates that will be inserted)
         # we use report_date; if there isnt a date, the case cannot be counted
         # Note that reported cases are those that are in the linelist
         # Confirmed cases will be added later
         is_reported = case_when(!is.na(report_date) ~ 1,
                                 TRUE ~ 0),
         # Creating age_group variable
         age_group = cut(as.integer(patinfo_ageonset_years), 
                         breaks = c(-Inf, 50, Inf), labels = c("less than 50 y.o.","50+ y.o.")),
         # impute missing report_dates with date of consult, date lab taken, onset date, 
         # death date or discharge date or lab result dats in that order
         report_date = if_else(is.na(report_date), consultation_dateHF, report_date),
         report_date = if_else(is.na(report_date), Lab_datetaken, report_date),
         report_date = if_else(is.na(report_date), patcourse_dateonset, report_date),
         report_date = if_else(is.na(report_date), patcourse_datedeath, report_date),
         report_date = if_else(is.na(report_date), patcourse_datedischarge, report_date),
         report_date = if_else(is.na(report_date), Lab_resdate, report_date)) %>%
  # if report date is still NA, drop from analysis.
  filter(!is.na(report_date))


##################################################################################
##                                                                              ##
##      Cleaning patinfo_resadmin1 (region) variable using a csv dictionary     ##
##                                                                              ##
##################################################################################

# importing dictionary of correct/incorrect names 
# (basically listed the unique values of patinfo_resadmin1 and patinfo_resadmin2 
# and atributed the correct names that are present in the gadm file)
dict <- read.csv2("./Report/Scripts/Dictionaries/dict_niger.csv", encoding = "UTF-8")

# testing if the names in the cleanCSV are correct or not
test_incorrect_admin1 <- df$patinfo_resadmin1 %in% dict$incorrect[dict$admin_lvl == 1]

test_incorrect_admin2 <- df$patinfo_resadmin2 %in% dict$incorrect[dict$admin_lvl == 2]

# Loop to test individually if each patinfo_resadmin1 entry is correct or not
for(i in seq_along(df$patinfo_resadmin1)){
  if(test_incorrect_admin1[i] == TRUE) {
    index <- dict$incorrect %in% df$patinfo_resadmin1[i]
    df$resadmin1_correct[i] <- dict$correct[dict$admin_lvl == 1 & index == TRUE]
  }
  if(test_incorrect_admin2[i] == TRUE) {
    index <- dict$incorrect %in% df$patinfo_resadmin2[i]
    df$resadmin2_correct[i] <- dict$correct[dict$admin_lvl == 2 & index == TRUE]
  }
  
}


####################################################################################
##                                                                                ##
##      Inserting missing dates for all regions and creating epiweek variable     ##
##                                                                                ##
####################################################################################

# Expanding df with missing report dates for all regions
df_expanded <- df %>% 
  # complete to include all dates, even when case was not recorded 
  complete(report_date = seq.Date(min(report_date, na.rm = TRUE), 
                                  max(report_date, na.rm = TRUE), by = "day")) %>%
  # expand to include all dates in all regions
  expand(resadmin1_correct, report_date) %>%
  filter(!is.na(report_date))

# Joining with df
df <- df %>% 
  right_join(df_expanded) %>%
  # creating an epiweek variable 
  mutate(epiweek = MMWRweek(report_date)$MMWRweek)


##################################################################
##     Importing gpkg and extracting population from raster     ##
##################################################################

# Extracting population values from raster to gpkg files

# Reading gpkg with region polygons
df_gpkg <- readOGR(paste0("/work/data-platform/data/maps/", "niger_gadm36_NER.gpkg"),
                   layer = "gadm36_NER_1")

# Reading raster data
pop <- raster("./data/gpw-v4-population-count-rev11_2020_15_min_tif/gpw_v4_population_count_rev11_2020_15_min.tif")

# Reprojecting gpkg to wgs84
df_gpkg <- spTransform(df_gpkg, crs(pop))

# Checking if CRS match
crs(pop)
crs(df_gpkg)

# Extracting centroids
centroids_df <- data.frame(coordinates(df_gpkg)) %>%
  rename(X = X1,
         Y = X2)

# Inserting centroids into gpkg
df_gpkg <- cbind(df_gpkg, centroids_df)

# Creating a spatialDF of centroids to extract raster values
centroids <- SpatialPointsDataFrame(coords = centroids_df,
                                    data = data.frame(df_gpkg, centroids_df),
                                    proj4string = crs(df_gpkg))

# Checking if raster and polygons overlap, just in case
# plot(pop, ext = extent(df_gpkg))
# plot(df_gpkg, add = T)
# points(centroids)

# Extract all pop values in each polygons (should take a while)
pop_vals <- raster::extract(pop, df_gpkg)

# Mean population for each polygon
pop_mean <- unlist(lapply(pop_vals, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))

# Sum population for each polygon
pop_sum <- unlist(lapply(pop_vals, function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA ))

# Creating a data.frame with results
pop_data <- data.frame(NAME_1 = df_gpkg$NAME_1,
                       pop_mean = pop_mean,
                       pop_sum = pop_sum)


#######################################################################
##     Building a daily cases/deaths df for all figures and maps     ##
#######################################################################

# Creating new object with country linelist
df2 <- df  %>% 
  mutate(resadmin1_correct = case_when(is.na(resadmin1_correct) ~ "No Info",
                                       TRUE ~ resadmin1_correct))

# Calculate daily reported cases
reported_region <- df2 %>%
  # summarise the number of reported each day in each region
  group_by(resadmin1_correct, report_date) %>%
  summarise(reported_this_day = sum(is_reported, na.rm = T))

# Calculate daily confirmed cases
confirmed_region <-  df2 %>%
  # summarise the number of confirmed cases each day in each region
  group_by(resadmin1_correct, report_date) %>%
  summarise(confirmed_this_day = sum(is_reported[report_classif == "CONFIRMED"], na.rm = T))

# Calculate daily deaths
deaths_region <- df2 %>%
  # summarise the number of deaths each day in each region
  group_by(resadmin1_correct, report_date) %>%
  summarise(deaths_this_day = sum(is_reported[patcourse_status == "DEAD"], na.rm = T))

# Calculate daily discharges as above
discharges_region <- df2 %>% 
  # summarise the number of deaths each day in each region
  group_by(resadmin1_correct, report_date) %>%
  summarise(discharges_this_day = sum(is_reported[!is.na(patcourse_datedischarge)], na.rm = T))

# Combine reported cases, confirmed cases, deaths and discharges into a single df
df_daily <- inner_join(reported_region, confirmed_region,
                       by= c("report_date", "resadmin1_correct"))  %>% 
  right_join(deaths_region,
             by= c("report_date", "resadmin1_correct")) %>% 
  left_join(discharges_region,
            by= c("report_date", "resadmin1_correct")) %>% 
  # sort by date within each region 
  arrange(resadmin1_correct, report_date) %>% 
  # delete useless vars
  select(report_date, resadmin1_correct, reported_this_day, 
         confirmed_this_day, deaths_this_day, discharges_this_day) %>% 
  # arranging by regions and report dates (cumsum seems to work only when arranging or grouping;
  # in this case, it only worked correctly when arranging)
  arrange(resadmin1_correct, report_date) %>% 
  # Calculating cumulative totals
  mutate(epiweek = MMWRweek(report_date)$MMWRweek,
         cum_reported = cumsum(reported_this_day),
         cum_confirmed = cumsum(confirmed_this_day),
         cum_deaths = cumsum(deaths_this_day),
         cum_discharges = cumsum(discharges_this_day)) %>%
  # removing NA regions and dates (artifacts that may have been up to this step)
  filter(!is.na(resadmin1_correct) | is.na(report_date)) %>% 
  group_by(resadmin1_correct) %>%
  # calculating crude case fatality rate at each time point
  mutate(CFR = round(100 * cum_deaths/cum_confirmed, digits = 2)) %>% 
  # inserting population data
  left_join(pop_data, by = c("resadmin1_correct" = "NAME_1")) %>%
  # calculating incidence and mortality rates per 100,000 for maps
  mutate(incidence_reported = cum_reported / pop_sum * 100000,
         incidence_confirmed = cum_confirmed / pop_sum * 100000,
         mortality = cum_deaths / pop_sum * 100000) %>%
  # calculating log of each variable
  mutate(reported_log = log10(reported_this_day + 1),
         confirmed_log = log10(confirmed_this_day + 1),
         deaths_log = log10(deaths_this_day + 1),
         discharges_log = log10(discharges_this_day + 1),
         cum_reported_log = log10(cum_reported + 1),
         cum_confirmed_log = log10(cum_confirmed + 1),
         cum_deaths_log = log10(cum_deaths + 1),
         cum_discharges_log = log10(cum_discharges + 1),
         incidence_reported_log = log10(incidence_reported + 1),
         incidence_confirmed_log = log10(incidence_confirmed + 1),
         mortality_log = log10(mortality + 1)) %>%
  # calculating number and log of cases, deaths and discharges in the past week
  mutate(reported_past_7 = rollsum(x = reported_this_day, k = 7, align = "right",
                                   fill = na.fill(reported_this_day, 0)),
         confirmed_past_7 = rollsum(x = confirmed_this_day, k = 7, align = "right",
                                    fill = na.fill(confirmed_this_day, 0)),
         deaths_past_7 = rollsum(x = deaths_this_day, k = 7, align = "right",
                                 fill = na.fill(deaths_this_day, 0)),
         discharges_past_7 = rollsum(x = discharges_this_day, k = 7, align = "right",
                                     fill = na.fill(discharges_this_day, 0)),
         reported_past_7_log = log10(reported_past_7 + 1),
         confirmed_past_7_log = log10(confirmed_past_7 + 1),
         deaths_past_7_log = log10(deaths_past_7 + 1),
         discharges_past_7_log = log10(discharges_past_7 + 1)) %>%
  # calculating number and log of cases, deaths and discharges in the past two weeks
  mutate(reported_past_14 = rollsum(x = reported_this_day, k = 14, align = "right",
                                    fill = na.fill(reported_this_day, 0)),
         confirmed_past_14 = rollsum(x = confirmed_this_day, k = 14, align = "right",
                                     fill = na.fill(confirmed_this_day, 0)),
         deaths_past_14 = rollsum(x = deaths_this_day, k = 14, align = "right",
                                  fill = na.fill(deaths_this_day, 0)),
         discharges_past_14 = rollsum(x = discharges_this_day, k = 14, align = "right",
                                      fill = na.fill(discharges_this_day, 0)),
         reported_past_14_log = log10(reported_past_14 + 1),
         confirmed_past_14_log = log10(confirmed_past_14 + 1),
         deaths_past_14_log = log10(deaths_past_14 + 1),
         discharges_past_14_log = log10(discharges_past_14 + 1)) %>%
  # calculating number and log of cases, deaths and discharges in the 14 day of the previous two weeks
  mutate(reported_prev_14 = lag(n = 14, reported_past_14),
         confirmed_prev_14 = lag(n = 14, confirmed_past_14),
         deaths_prev_14 = lag(n = 14, deaths_past_14),
         discharges_prev_14 = lag(n = 14, discharges_past_14),
         reported_prev_14_log = log10(reported_prev_14 + 1),
         confirmed_prev_14_log = log10(confirmed_prev_14 + 1),
         deaths_prev_14_log = log10(deaths_prev_14 + 1),
         discharges_prev_14_log = log10(discharges_prev_14 + 1)) %>%
  # calculating trend and log cases, deaths and discharges in the past week
  mutate(reported_trend = rollmean(x = reported_this_day, k = 7, align = "right",  
                                   fill = na.fill(reported_this_day, NA)),
         confirmed_trend = rollmean(x = confirmed_this_day, k = 7, align = "right",  
                                    fill = na.fill(confirmed_this_day, NA)),
         deaths_trend =rollmean(x = deaths_this_day, k = 7, align = "right",  
                                fill = na.fill(deaths_this_day, NA)),
         discharges_trend =rollmean(x = discharges_this_day, k = 7, align = "right",  
                                    fill = na.fill(discharges_this_day, NA)),
         reported_trend_log = log10(reported_trend + 1),
         confirmed_trend_log = log10(confirmed_trend + 1),
         deaths_trend_log = log10(deaths_trend + 1),
         discharges_trend_log = log10(discharges_trend + 1))

# Combine into national plot
df_daily_national <- df_daily %>% 
  select(report_date, resadmin1_correct, reported_this_day, confirmed_this_day, 
         deaths_this_day, discharges_this_day) %>% 
  # pulling values from selected columns
  pivot_wider(id_cols = report_date, names_from = resadmin1_correct, 
              values_from = c(reported_this_day, confirmed_this_day, 
                              deaths_this_day, discharges_this_day))

# identifying which columns pertain to cases, deaths and discharges
# there should be a more elegant way to do this.
reported_cols <- which(!is.na(str_extract(names(df_daily_national), pattern = "reported")))
confirmed_cols <- which(!is.na(str_extract(names(df_daily_national), pattern = "confirmed")))
deaths_cols <- which(!is.na(str_extract(names(df_daily_national), pattern = "deaths")))
discharges_cols <- which(!is.na(str_extract(names(df_daily_national), pattern = "discharges")))

# sum across rows to generate national values
df_daily_national <- df_daily_national%>% 
  mutate(reported_this_day = rowSums(.[reported_cols]),
         confirmed_this_day = rowSums(.[confirmed_cols]),
         deaths_this_day = rowSums(.[deaths_cols]),
         discharges_this_day = rowSums(.[discharges_cols])) %>% 
  select(report_date, reported_this_day, confirmed_this_day, 
         deaths_this_day, discharges_this_day) %>% 
  # creating a cumulative sum, 7- and 14-day average of cases, deaths and discharges
  mutate(epiweek = MMWRweek(report_date)$MMWRweek,
         cum_reported = cumsum(reported_this_day),
         cum_confirmed = cumsum(confirmed_this_day),
         cum_deaths = cumsum(deaths_this_day),
         cum_discharges = cumsum(discharges_this_day),
         reported_7day_avg = rollmean(x = reported_this_day, k = 7, align = "right",  
                                      fill = na.fill(reported_this_day, "extend")),
         confirmed_7day_avg = rollmean(x = confirmed_this_day, k = 7, align = "right",  
                                       fill = na.fill(confirmed_this_day, "extend")),
         deaths_7day_avg = rollmean(x = deaths_this_day, k = 7, align = "right",  
                                    fill = na.fill(deaths_this_day, "extend")),
         discharges_7day_avg = rollmean(x = discharges_this_day, k = 7, align = "right",  
                                        fill = na.fill(discharges_this_day, "extend")),
         reported_14day_avg = rollmean(x = reported_this_day, k = 14, align = "right",  
                                       fill = na.fill(reported_this_day, "extend")),
         confirmed_14day_avg = rollmean(x = confirmed_this_day, k = 14, align = "right",  
                                        fill = na.fill(confirmed_this_day, "extend")),
         deaths_14day_avg = rollmean(x = deaths_this_day, k = 14, align = "right",  
                                     fill = na.fill(deaths_this_day, "extend")),
         discharges_14day_avg = rollmean(x = discharges_this_day, k = 14, align = "right",  
                                         fill = na.fill(discharges_this_day, "extend"))) 

count_of_cases_reported <- max(df_daily_national$cum_reported, na.rm = T)
count_of_cases_confirmed <- max(df_daily_national$cum_confirmed, na.rm = T)
count_of_deaths <- max(df_daily_national$cum_deaths, na.rm = T)
count_of_discharges <- max(df_daily_national$cum_discharges, na.rm = T)

current_crude_CFR <-  round(100 * max(df_daily_national$cum_deaths, na.rm = T) / 
                              max(df_daily_national$cum_confirmed, na.rm = T), 2)


#####################################################
##     Building an epiweek df for figures/maps     ##
#####################################################

# Similar to daily df, but grouped by epiweek
df_ew <- df_daily %>%
  group_by(resadmin1_correct, epiweek) %>%
  summarise(reported_this_week = sum(reported_this_day),
            confirmed_this_week = sum(confirmed_this_day),
            deaths_this_week = sum(deaths_this_day),
            discharges_this_week = sum(discharges_this_day)) %>%
  mutate(cum_reported = cumsum(reported_this_week),
         cum_confirmed = cumsum(confirmed_this_week),
         cum_deaths = cumsum(deaths_this_week),
         cum_discharges = cumsum(discharges_this_week)) %>%
  group_by(resadmin1_correct) %>%
  # Calculate crude CFR at each time point
  mutate(CFR = round(100 * cum_deaths/cum_confirmed, digits = 2)) %>% 
  left_join(pop_data, by = c("resadmin1_correct" = "NAME_1")) %>%
  # calculating incidence and mortality rates per 100,000 for maps
  mutate(incidence_reported = cum_reported / pop_sum * 100000,
         incidence_confirmed = cum_confirmed / pop_sum * 100000,
         mortality = cum_deaths / pop_sum * 100000) %>%
  # calculating log of each variable
  mutate(reported_log = log10(reported_this_week + 1),
         confirmed_log = log10(confirmed_this_week + 1),
         deaths_log = log10(deaths_this_week + 1),
         discharges_log = log10(discharges_this_week + 1),
         cum_reported_log = log10(cum_reported + 1),
         cum_confirmed_log = log10(cum_confirmed + 1),
         cum_deaths_log = log10(cum_deaths + 1),
         cum_discharges_log = log10(cum_discharges + 1),
         incidence_reported_log = log10(incidence_reported + 1),
         incidence_confirmed_log = log10(incidence_confirmed + 1),
         mortality_log = log10(mortality + 1)) %>%
  # calculating trend and log cases, deaths and discharges in the past week
  mutate(reported_trend = rollmean(x = reported_this_week, k = 7, align = "right",  
                                   fill = na.fill(reported_this_week, NA)),
         confirmed_trend = rollmean(x = confirmed_this_week, k = 7, align = "right",  
                                    fill = na.fill(confirmed_this_week, NA)),
         deaths_trend =rollmean(x = deaths_this_week, k = 7, align = "right",  
                                fill = na.fill(deaths_this_week, NA)),
         discharges_trend =rollmean(x = discharges_this_week, k = 7, align = "right",  
                                    fill = na.fill(discharges_this_week, NA)),
         reported_trend_log = log10(reported_trend + 1),
         confirmed_trend_log = log10(confirmed_trend + 1),
         deaths_trend_log = log10(deaths_trend + 1),
         discharges_trend_log = log10(discharges_trend + 1))


####################################################
##     Joining daily and epiweek df with gpkg     ##
####################################################

# Merging Niger daily DF and gpkg
df_gpkg_daily <- df_gpkg %>%
  # converting to sf, easier to make maps
  st_as_sf() %>%
  # joining daily df with gpkg
  full_join(df_daily, by = c("NAME_1" = "resadmin1_correct")) %>%
  # this part of the code is aimed to fill the regions without cases with data
  # this way plotting the maps wont show holes
  # replacing NA with zeros in all epidemiological variables 
  mutate_at(vars(reported_this_day:discharges_trend_log), ~replace_na(., 0)) %>%
  # inserting the max report_date in the regions with missing data
  mutate(report_date = if_else(is.na(report_date), max(na.omit(report_date)), report_date))

# Merging Niger daily EW and gpkg
df_gpkg_ew <- df_gpkg %>%
  st_as_sf() %>%
  full_join(df_ew, by = c("NAME_1" = "resadmin1_correct")) %>%
  # this part of the code is aimed to fill the regions without cases with data
  # this way plotting the maps wont show holes
  # replacing NA with zeros in all epidemiological variables 
  mutate_at(vars(reported_this_week:discharges_trend_log), ~replace_na(., 0)) %>%
  # inserting the max epiweek in the regions with missing data
  mutate(epiweek = if_else(is.na(epiweek), max(na.omit(epiweek)), epiweek))


##########################################################
##     Importing gpkg with spatial analyses results     ##
##########################################################

# Reading gpkg with cluster analysis (Local Moran) results at district level
df_cluster <- readOGR(paste0("/work/data-platform/data/spatial_analyses/Niger_Local_Moran.gpkg")) %>%
  st_as_sf() %>%
  # Recoding levels so that maps become easier to read: each level will be assigned to a color in the cluster analysis section
  mutate(cl = fct_recode(cl, 
                         "Non signif." = "0 non signif.",
                         "Hot spot" = "1 Hot spot",
                         "Doughnut" = "2 Doughnut",
                         "Cold spot" = "3 Cold spot",
                         "Diamond" = "4 Diamond"))


###########################################
##     Creating x-axis for epicurves     ##
###########################################

# Creating the axis label

# The following will need to be added to each ggplot to use these labels
# + scale_x_date(breaks = coveredsundays,
#               labels = epiweekanddate)

# Subset just the Sundays. Epiweeks start on Sunday, so we will need these to assign labels
daysinrange <- seq(min(na.omit(df2$report_date)),
                   max(na.omit(df2$report_date)),
                   by = "days")

coveredsundays <- daysinrange[lubridate::wday(daysinrange) == 1]

# Identify the epiweek that each sunday belongs to
epiweeks <- lubridate::epiweek(coveredsundays) 

# Extract a nicely formatted date for each Epiweek sunday start
epiweeksundaydate <- format.Date(coveredsundays, format = "%b %d")
epiweekanddate <-  paste("EW", epiweeks, "\n", "(",epiweeksundaydate, ")", sep = "")
# epiweekanddate is our final output
# check that all is well with : as.data.frame(coveredsundays,epiweekanddate)

coveredsundays <- rev(coveredsundays)
epiweekanddate <- rev(epiweekanddate)

# every other sunday. For tight plots
coveredsundays2 <- coveredsundays[c(T, F)] 
epiweekanddate2 <- epiweekanddate[c(T, F)]

# every third sunday. For really tight plots. Add aditional FALSES for tighter plots
coveredsundays3 <- coveredsundays[c(T,F,F)] 
epiweekanddate3 <- epiweekanddate[c(T,F,F)]

coveredsundays4 <- coveredsundays[c(T,F,F,F)] 
epiweekanddate4 <- epiweekanddate[c(T,F,F,F)]

coveredsundays5 <- coveredsundays[c(T,F,F,F,F)] 
epiweekanddate5 <- epiweekanddate[c(T,F,F,F,F)]


################################
################################
##                            ##
##      1 SUMMARY SECTION     ##
##                            ##
##       TABLES AND MAPS      ##
##                            ##
################################
################################


##################################################
##      Chart regions with cases and deaths     ##
##################################################

tab_regions_1 <- df_daily %>% 
  # take the latest date for each region
  group_by(resadmin1_correct) %>% 
  slice(which.max(report_date)) %>% 
  mutate(cum_reported_region = sum(cum_reported),
         cum_confirmed_region = sum(cum_confirmed),
         cum_deaths_region = sum(cum_deaths),
         Population = pop_sum) %>% 
  # Renaming for a more elegant presentation
  rename(Region = resadmin1_correct,
         `Reported cases` = cum_reported_region, 
         `Confirmed cases` = cum_confirmed, 
         Deaths = cum_deaths_region) %>% 
  mutate(`Crude CFR (%)` = round(100 * Deaths/`Confirmed cases`, digits = 1),
         Population = round(Population, 0),
         `Reported per 100,000` = round(`Reported cases`/Population * 100000, 2),
         `Confirmed per 100,000` = round(`Confirmed cases`/Population * 100000, 2),
         `Deaths per 100,000` = round(Deaths/Population * 100000, 2)) %>% 
  select(Region, `Reported cases`, `Confirmed cases`, Deaths, `Crude CFR (%)`, Population, `Reported per 100,000`, `Confirmed per 100,000`, `Deaths per 100,000`) %>% 
  # sort in descending order of cases
  arrange(-`Reported cases`) %>%
  ungroup()

# formattable plot
tab_epi <- formattable(tab_regions_1  %>% filter(Region != "No Info"), 
                       list(
                         `Reported cases` = color_bar2("orange"), 
                         `Confirmed cases` = color_bar2("orange"), 
                         `Deaths` = color_bar2("#ff8066"), 
                         Population = color_bar2("#6abda7"), 
                         #  `Crude CFR (%)` = color_tile("white", "red"), 
                         `Reported per 100,000` = color_bar2("orange"), 
                         `Confirmed per 100,000` = color_bar2("orange"), 
                         `Deaths per 100,000`= color_bar2("#ff8066")
                       ))

export_formattable(tab_epi, paste0(out_path, my_country, "_Tab_1.png"),
                   width = 1500, height = 1000)


tab_regions_2 <- df_daily %>% 
  # take the latest date for each region
  group_by(resadmin1_correct) %>% 
  slice(which.max(report_date)) %>% 
  ungroup() %>% 
  # calculate case growth
  mutate(reported_growth = round((reported_past_14 - reported_prev_14) / (reported_prev_14) * 100, 2), 
         # remove NA
         reported_growth = ifelse(!is.na(reported_growth), formattable::percent(reported_growth), 0), 
         confirmed_growth = round((confirmed_past_14 - confirmed_prev_14) / (confirmed_prev_14) * 100, 2), 
         confirmed_growth = ifelse(!is.na(confirmed_growth), formattable::percent(confirmed_growth), 0), 
         deaths_growth = round((deaths_past_14 - deaths_prev_14) / (deaths_prev_14) * 100, 2), 
         deaths_growth = ifelse(!is.na(deaths_growth), formattable::percent(deaths_growth), 0)) %>% 
  # presentable names
  transmute(Region = resadmin1_correct,
            `Total reported cases` = cum_reported, 
            `Reported cases 4-2 weeks ago` = reported_prev_14, 
            `Reported cases past 2 weeks` = reported_past_14, 
            `2-week % change in reported cases` = reported_growth,
            `Total confirmed cases` = cum_confirmed, 
            `Confirmed cases 4-2 weeks ago` = confirmed_prev_14, 
            `Confirmed cases past 2 weeks` = confirmed_past_14, 
            `2-week % change in confirmed cases` = confirmed_growth,
            `Total deaths` = cum_deaths, 
            `Deaths 4-2 weeks ago` = deaths_prev_14, 
            `Deaths past 2 weeks` = deaths_past_14, 
            `2-week % change in deaths` = deaths_growth) %>% 
  # remove mising info 
  filter(Region != "No Info") 

# create table
tab_growth <- formattable(tab_regions_2, 
                          list(
                            # reported cases color bar
                            `Total reported cases` = color_bar2("orange"), 
                            # height of bar should be mapped to both columns 3 and 4
                            formattable::area(col = 3:4) ~ color_bar2("#ffd9b8"), 
                            # % change. Use custom function
                            `2-week % change in reported cases` = growth_formatter, 
                            # confirmed cases
                            `Total confirmed cases` = color_bar2("orange"), 
                            formattable::area(col = 7:8) ~ color_bar2("#ffd9b8"), 
                            `2-week % change in confirmed cases` = growth_formatter, 
                            # Deaths
                            `Total deaths` = color_bar2("#ff8066"), 
                            formattable::area(col = 11:12) ~ color_bar2("#ffb1a1"), 
                            `2-week % change in deaths` = growth_formatter))

export_formattable(tab_growth, paste0(out_path, my_country, "_Tab_2.png"),
                   width = 1500, height = 1000)


#####################################################################
##      MAPS SECTION: SEVERAL THAT COULD SUIT BEST THE REPORT      ##
#####################################################################

############################################################
##      Region level maps: cumulative reported cases      ##
############################################################

ggplot(data = df_gpkg_ew[df_gpkg_ew$epiweek == max(df_gpkg_ew$epiweek, na.rm = TRUE), ]) +
  geom_sf(aes(fill = cum_reported)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(paste0(my_country), subtitle = paste0("Epidemiological week ", 
                                                max(df_gpkg_ew$epiweek, na.rm = TRUE))) +
  theme_bw() +
  scale_fill_distiller("Cumulative number of Covid-19\nreported cases",
                       type = "seq",
                       palette = "YlOrRd",
                       direction = 1,
                       na.value = "#FFFFB3",
                       aesthetics = "fill",
                       guide = "colourbar",
                       limits = c(0, max(df_gpkg_ew$cum_reported, na.rm = TRUE))) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  # country label
  geom_text(aes(x = X, y = Y - 0.1, label = NAME_1),
            size = 2.4, colour = "black", fontface = "bold") +
  labs(x = "", y = "")

ggsave(paste0(out_path, my_country, "_Map_CasesReported.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


############################################################
##      Region level maps: cumulative confirmed cases     ##
############################################################

ggplot(data = df_gpkg_ew[df_gpkg_ew$epiweek == max(df_gpkg_ew$epiweek, na.rm = TRUE), ]) +
  geom_sf(aes(fill = cum_confirmed)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(paste0(my_country), subtitle = paste0("Epidemiological week ", 
                                                max(df_gpkg_ew$epiweek, na.rm = TRUE))) +
  theme_bw() +
  scale_fill_distiller("Cumulative number of Covid-19\nconfirmed cases",
                       type = "seq",
                       palette = "YlOrRd",
                       direction = 1,
                       na.value = "#FFFFB3",
                       aesthetics = "fill",
                       guide = "colourbar",
                       limits = c(0, max(df_gpkg_ew$cum_confirmed, na.rm = TRUE))) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  # country label
  geom_text(aes(x = X, y = Y - 0.1, label = NAME_1),
            size = 2.4, colour = "black", fontface = "bold") +
  labs(x = "", y = "")

ggsave(paste0(out_path, my_country, "_Map_CasesConfirmed.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


####################################################
##      Region level maps: cumulative deaths      ##
####################################################

ggplot(data = df_gpkg_ew[df_gpkg_ew$epiweek == max(df_gpkg_ew$epiweek, na.rm = TRUE), ]) +
  geom_sf(aes(fill = cum_deaths)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(paste0(my_country), subtitle = paste0("Epidemiological week ", 
                                                max(df_gpkg_ew$epiweek, na.rm = TRUE))) +
  theme_bw() +
  scale_fill_distiller("Cumulative number of\nCovid-19 deaths",
                       type = "seq",
                       palette = "YlOrRd",
                       direction = 1,
                       na.value = "white",
                       aesthetics = "fill",
                       guide = "colourbar",
                       limits = c(0, max(df_gpkg_ew$cum_deaths, na.rm = TRUE))) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  # country label
  geom_text(aes(x = X, y = Y - 0.1, label = NAME_1),
            size = 2.4, colour = "black", fontface = "bold") +
  labs(x = "", y = "")

ggsave(paste0(out_path, my_country, "_Map_Deaths.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


##################################################################
##      Region level maps: incidence rate of reported cases     ##
##################################################################

ggplot(data = df_gpkg_ew[df_gpkg_ew$epiweek == max(df_gpkg_ew$epiweek, na.rm = TRUE), ]) +
  geom_sf(aes(fill = incidence_reported)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(paste0(my_country), subtitle = paste0("Epidemiological week ", 
                                                max(df_gpkg_ew$epiweek, na.rm = TRUE))) +
  theme_bw() +
  scale_fill_distiller("Covid-19 incidence rate\nper 100,000 inhabitants\n(reported cases)",
                       type = "seq",
                       palette = "YlOrRd",
                       direction = 1,
                       na.value = "white",
                       aesthetics = "fill",
                       guide = "colourbar",
                       limits = c(0, max(df_gpkg_ew$incidence_reported, na.rm = TRUE))) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  # country label
  geom_text(aes(x = X, y = Y - 0.1, label = NAME_1),
            size = 2.4, colour = "black", fontface = "bold") +
  labs(x = "", y = "")

ggsave(paste0(out_path, my_country, "_Map_Incidence_CasesReported.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


####################################################################
##      Region level maps: incidence rate of confirmed cases      ##
####################################################################

ggplot(data = df_gpkg_ew[df_gpkg_ew$epiweek == max(df_gpkg_ew$epiweek, na.rm = TRUE), ]) +
  geom_sf(aes(fill = incidence_confirmed)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(paste0(my_country), subtitle = paste0("Epidemiological week ", 
                                                max(df_gpkg_ew$epiweek, na.rm = TRUE))) +
  theme_bw() +
  scale_fill_distiller("Covid-19 incidence rate\nper 100,000 inhabitants\n(confirmed cases)",
                       type = "seq",
                       palette = "YlOrRd",
                       direction = 1,
                       na.value = "white",
                       aesthetics = "fill",
                       guide = "colourbar",
                       limits = c(0, max(df_gpkg_ew$incidence_confirmed, na.rm = TRUE))) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  # country label
  geom_text(aes(x = X, y = Y - 0.1, label = NAME_1),
            size = 2.4, colour = "black", fontface = "bold") +
  labs(x = "", y = "")

ggsave(paste0(out_path, my_country, "_Map_Incidence_CasesConfirmed.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


################################################
##      Region level maps: mortality rate     ##
################################################

ggplot(data = df_gpkg_ew[df_gpkg_ew$epiweek == max(df_gpkg_ew$epiweek, na.rm = TRUE), ]) +
  geom_sf(aes(fill = mortality)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(paste0(my_country), subtitle = paste0("Epidemiological week ", max(df_gpkg_ew$epiweek, na.rm = TRUE))) +
  theme_bw() +
  scale_fill_distiller("Covid-19 mortality rate \nper 100,000 inhabitants",
                       type = "seq",
                       palette = "YlOrRd",
                       direction = 1,
                       na.value = "white",
                       aesthetics = "fill",
                       guide = "colourbar",
                       limits = c(0, max(df_gpkg_ew$mortality, na.rm = TRUE))) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  # country label
  geom_text(aes(x = X, y = Y - 0.1, label = NAME_1),
            size = 2.4, colour = "black", fontface = "bold") +
  labs(x = "", y = "")

ggsave(paste0(out_path, my_country, "_Map_Mortality.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


###############################################################
##      Incidence of reported cases: past 14 and 28 days     ##
###############################################################

# Compositve map of incidence of reported cases in the past 14 and 28 days
for (howmuchlag in c(14, 0)) { 
  
  howmuchlag <- as.numeric(howmuchlag)  
  
  # slice the date desired
  past2 <- df_gpkg_daily %>% 
    group_by(NAME_1) %>% 
    slice(which.max(report_date) - howmuchlag) %>% 
    mutate(incidence_reported_past_14 = 100000 * reported_past_14 / pop_sum)
  
  # plot
  p  <- past2 %>% 
    ggplot() +
    geom_sf(aes(fill = incidence_reported_past_14),
            color = "gray20", size = 0.05) +
    # title iterates over epiweeks
    ggtitle(paste0(format.Date(as.Date(past2$report_date - 14) , format = "%b %d, %Y"),
                   " to ",
                   format.Date(past2$report_date, format = "%b %d, %Y"))) +
    # fill color is mapped to reported cases per 100,000 inhabitants
    scale_fill_distiller("Reported cases per 100,000",
                         type = "seq",
                         palette = "YlOrRd",
                         direction = 1,
                         na.value = "white",
                         aesthetics = "fill",
                         guide = "colourbar",
                         limits = c(0, max(past2$incidence_reported_past_14, na.rm = TRUE))) +
    # country label (anchored to centroid values), but pushed downward slightly to avoid overlap
    geom_text(aes(x = X - 0.3, y = Y + 0.3, label = NAME_1),
              size = 2.4, colour = "black", fontface = "bold") +
    labs(x = "", y = "") +
    # point mapped to cumulative cases
    geom_point(data = . %>% filter(reported_past_14 > 0), 
               aes(x = X, y = Y, size = reported_past_14), 
               shape = 21, color = alpha("dodgerblue3", 0.8), fill = alpha("dodgerblue3", 0.33), stroke = 1) +
    scale_size_continuous(name = "Reported cases past 14 days", range = c(3, 18), guide = FALSE) +
    # text mapped to count of cases
    geom_text(data = . %>% filter(reported_past_14 > 0), 
              aes(x = X - 0.3, y = Y, label = reported_past_14),
              color = alpha("black", 1), size = 2.4, fontface = "bold")  +
    theme(legend.position = "bottom", 
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype = "dashed"),
          axis.line.x.bottom = element_blank(), 
          legend.title = element_text(size = 6), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()) +
    labs(caption = "Circles track the absolute number of reported cases in each Region")
  
  # output two plots
  futurename <- paste0("plot", howmuchlag)
  assign(futurename, p)
  
}

ggsave(paste0(out_path, my_country, "_Map_past_14_28_days.png"), plot = grid.arrange(plot14, plot0, ncol = 2), 
       width = 35, height = 20, units = "cm", dpi = 600)


################################################################
##      Incidence of confirmed cases: past 14 and 28 days     ##
################################################################

# Compositve map of incidence of confirmed cases in the past 14 and 28 days
for (howmuchlag in c(14, 0)) { 
  
  howmuchlag <- as.numeric(howmuchlag)  
  
  # slice the date desired
  past2 <- df_gpkg_daily %>% 
    group_by(NAME_1) %>% 
    slice(which.max(report_date) - howmuchlag) %>% 
    mutate(incidence_confirmed_past_14 = 100000 * confirmed_past_14 / pop_sum)
  
  # plot
  p  <- past2 %>% 
    ggplot() +
    geom_sf(aes(fill = incidence_confirmed_past_14),
            color = "gray20", size = 0.05) +
    # title iterates over epiweeks
    ggtitle(paste0(format.Date(as.Date(past2$report_date - 14) , format = "%b %d, %Y"),
                   " to ",
                   format.Date(past2$report_date, format = "%b %d, %Y"))) +
    # fill color is mapped to confirmed cases per 100,000 inhabitants
    scale_fill_distiller("Confirmed cases per 100,000",
                         type = "seq",
                         palette = "YlOrRd",
                         direction = 1,
                         na.value = "white",
                         aesthetics = "fill",
                         guide = "colourbar",
                         limits = c(0, max(past2$incidence_confirmed_past_14, na.rm = TRUE))) +
    # country label (anchored to centroid values), but pushed downward slightly to avoid overlap
    geom_text(aes(x = X - 0.3, y = Y + 0.3, label = NAME_1),
              size = 2.4, colour = "black", fontface = "bold") +
    labs(x = "", y = "") +
    # point mapped to cumulative cases
    geom_point(data = . %>% filter(confirmed_past_14 > 0), 
               aes(x = X, y = Y, size = confirmed_past_14), 
               shape = 21, color = alpha("dodgerblue3", 0.8), fill = alpha("dodgerblue3", 0.33), stroke = 1) +
    scale_size_continuous(name = "Confirmed cases past 14 days", range = c(3, 18), guide = FALSE) +
    # text mapped to count of cases
    geom_text(data = . %>% filter(confirmed_past_14 > 0), 
              aes(x = X - 0.3, y = Y, label = confirmed_past_14),
              color = alpha("black", 1), size = 2.4, fontface = "bold")  +
    theme(legend.position = "bottom", 
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype = "dashed"),
          axis.line.x.bottom = element_blank(), 
          legend.title = element_text(size = 6), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()) +
    labs(caption = "Circles track the absolute number of confirmed cases in each Region")
  
  # output two plots
  futurename <- paste0("plot", howmuchlag)
  assign(futurename, p)
  
}

ggsave(paste0(out_path, my_country, "_Map_past_14_28_days.png"), plot = grid.arrange(plot14, plot0, ncol = 2), 
       width = 35, height = 20, units = "cm", dpi = 600)


####################################################
##      District level maps: cluster analysis     ##
####################################################

# exploratory map with log transformed incidence
ggplot() +
  geom_sf(data = st_as_sf(df_gpkg), fill = "white") +
  geom_sf(data = df_cluster, aes(fill = log_incidence)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(paste0(my_country), subtitle = "Incidence rate (log transformed)") +
  theme_bw() +
  scale_fill_distiller("Covid-19 incidence rate \nper 100,000 inhabitants\n(log transformed)",
                       type = "seq",
                       palette = "YlOrRd",
                       direction = 1,
                       na.value = "white",
                       aesthetics = "fill",
                       guide = "colourbar",
                       limits = c(0, max(df_cluster$log_incidence, na.rm = TRUE))) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave(paste0(out_path, my_country, "_Cluster_exploratory.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# cluster analysis
ggplot() +
  geom_sf(data = st_as_sf(df_gpkg), fill = "white") +
  geom_sf(data = df_cluster, aes(fill = cl)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(paste0(my_country), subtitle = "Cluster analysis") +
  theme_bw() +
  scale_fill_manual("Clusters of Covid-19",
                    # Assigning colors to specific levels of LISA results
                    breaks = c("Non signif.", "Hot spot", "Doughnut", "Cold spot", "Diamond"),
                    values = c("gray90", "indianred1", "plum1", "skyblue1", "red")) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave(paste0(out_path, my_country, "_Cluster_LISA.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


##############################################################
##############################################################
##                                                          ##
##      2 ALL CASES AND DEATHS DISAGGREGATED BY REGION      ##
##                                                          ##
##          EPICURVES WITH MOVING AVERAGE SECTION           ##
##                                                          ##
##                    AND FACETTED PLOTS                    ##
##                                                          ##
##############################################################
##############################################################


#########################################################################
##      Daily absolute and relative reported cases plot per region     ##
##                      and summary statistics                         ##
#########################################################################

# Filtering provinces with low number of reported cases
provinces_most_reported <- df_daily %>%
  group_by(resadmin1_correct) %>% 
  # Discovering and ranking which provinces have more cases
  summarise(reported_this_day = sum(reported_this_day)) %>%
  arrange(-reported_this_day) %>% 
  # Selecting only top four (can be changed)
  slice(1:4) %>% 
  select(resadmin1_correct) %>% 
  unique() %>% 
  pull() %>% 
  as.character()

# daily absolute reported cases plot per region
df_daily %>% 
  ungroup() %>%
  select(report_date, resadmin1_correct, reported_this_day) %>% 
  mutate(resadmin1_correct =
           # Maintaining only the 5 provinces with highest number of cases, 
           # others are summed and recoded
           case_when(!resadmin1_correct %in% provinces_most_reported 
                     | resadmin1_correct == "NA" ~ "Other",
                     TRUE ~ as.character(resadmin1_correct)),
         # 7 days moving average for reported cases
         reported_7day_avg = rollmean(x = reported_this_day, k = 7, align = "right",  
                                      fill = na.fill(reported_this_day, 0)),
         reported_14day_avg = rollmean(x = reported_this_day, k = 14, align = "right",  
                                       fill = na.fill(reported_this_day, 0)),
         resadmin1_correct = as.factor(resadmin1_correct),
         resadmin1_correct = fct_relevel(resadmin1_correct, "Other", after = Inf)) %>% 
  ggplot() +
  # stacked bar graph
  geom_bar(aes(x = report_date, y = reported_this_day, fill = resadmin1_correct), 
           stat = "identity", position = "stack", colour = alpha("white", 0.05)) +
  scale_fill_manual(values = my_palette) +
  # Moving Average
  geom_line(data = df_daily_national  %>% 
              # creating a variable with moving average; needed to plot 7 and 14 days mov. avg. lines
              gather(key = "moving_avg", value = "reported_avg",
                     c("reported_7day_avg", "reported_14day_avg")) %>%
              select(report_date, reported_avg, moving_avg) %>%
              filter(moving_avg == "reported_7day_avg" | moving_avg == "reported_14day_avg") %>%
              mutate(moving_avg = recode_factor(moving_avg, 
                                                "reported_7day_avg" = "7-day \nrolling avg",
                                                "reported_14day_avg" = "14-day \nrolling avg")), 
            aes(x = report_date, y = reported_avg, linetype = moving_avg)) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE) - 1, 
                          max(df_daily$report_date, na.rm = TRUE))) +
  ggtitle(paste("Daily COVID-19 reported cases across regions of", my_country),
          subtitle = paste0("(National cumulative total as at ",
                            format.Date(max(df_daily_national$report_date), "%b %d"), 
                            ": ",
                            max(df_daily_national$cum_reported, na.rm = T),
                            " reported cases)")) +
  labs(x = "", y = "Absolute number of \n daily reported cases") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8))

ggsave(paste0(out_path, my_country, "_Epicurve_Abs_CasesReported.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


# daily relative reported cases plot per region
df_daily %>% 
  ungroup() %>%
  select(report_date, resadmin1_correct, reported_this_day) %>% 
  # what prop of total cases reported in a day are in a specific region?
  group_by(report_date) %>% 
  mutate(reported_prop =  100 * reported_this_day/ sum(reported_this_day),
         # Maintaining only the 4 provinces with highest number of cases, 
         # others are summed and recoded
         resadmin1_correct = case_when(!resadmin1_correct %in% provinces_most_reported 
                                       | resadmin1_correct == "NA" ~ "All others",
                                       TRUE ~ as.character(resadmin1_correct)),
         # push "All others" to the last level
         resadmin1_correct = as.factor(resadmin1_correct),
         resadmin1_correct = fct_relevel(resadmin1_correct, "All others", after = Inf)) %>% 
  # we need to add up the case counts for the newly created "Other" factor levels
  # we do this with the summarise function
  group_by(resadmin1_correct, report_date) %>% 
  summarise(reported_this_day = sum(reported_this_day),
            reported_prop = sum(reported_prop)) %>%
  group_by(resadmin1_correct) %>% 
  filter(max(reported_this_day) > 0) %>% 
  ggplot() +
  # stacked bar graph
  geom_bar(aes(x = report_date, y = reported_prop, fill = resadmin1_correct), 
           stat = "identity", position = "stack", colour = alpha("white", 0.05), size = 0.1) +
  scale_fill_manual(values = my_palette) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE), 
                          max(df_daily$report_date, na.rm = TRUE))) +
  ggtitle(paste("Daily COVID-19 reported cases across regions of", my_country, 
                "(relative proportions)"),
          subtitle = paste0("(National cumulative total as at ",
                            format.Date(max(df_daily_national$report_date), "%b %d"), 
                            ": ",
                            max(df_daily_national$cum_reported, na.rm = T),
                            " reported cases)")) +
  labs(x = "", y = "Percentage of \n daily reported cases") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(1, "line"))

ggsave(paste0(out_path, my_country, "_Epicurve_Rel_CasesReported.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


# summary statistics for reported cases
pander(df_daily %>% 
         ungroup() %>%
         select(report_date, resadmin1_correct, reported_this_day) %>% 
         mutate(resadmin1_correct = as.factor(resadmin1_correct),
                resadmin1_correct = fct_relevel(resadmin1_correct, "No Info", after = Inf)) %>% 
         group_by(resadmin1_correct) %>%
         summarise(`Reported cases (mean)` = mean(reported_this_day, na.rm = TRUE),
                   `Reported cases (median)` = median(reported_this_day, na.rm = TRUE),
                   `Reported cases (min)` = min(reported_this_day, na.rm = TRUE),
                   `Reported cases (max)` = max(reported_this_day, na.rm = TRUE),
                   `Reported cases (std. dev)` = sd(reported_this_day, na.rm = TRUE),
                   `Reported cases (total)` = sum(reported_this_day, na.rm = TRUE)))


###########################################################################
##      Daily absolute and relative confirmed cases plot per region      ##
##                      and summary statistics                           ##
###########################################################################

# Filtering provinces with low number of confirmed cases
provinces_most_confirmed <- df_daily %>%
  group_by(resadmin1_correct) %>% 
  # Discovering and ranking which provinces have more cases
  summarise(confirmed_this_day = sum(confirmed_this_day)) %>%
  arrange(-confirmed_this_day) %>% 
  # Selecting only top four (can be changed)
  slice(1:4) %>% 
  select(resadmin1_correct) %>% 
  unique() %>% 
  pull() %>% 
  as.character()

# daily absolute confirmed cases plot per region
df_daily %>% 
  ungroup() %>%
  select(report_date, resadmin1_correct, confirmed_this_day) %>% 
  mutate(resadmin1_correct =
           # Maintaining only the 5 provinces with highest number of cases, 
           # others are summed and recoded
           case_when(!resadmin1_correct %in% provinces_most_confirmed 
                     | resadmin1_correct == "NA" ~ "Other",
                     TRUE ~ as.character(resadmin1_correct)),
         # 7 days moving average for confirmed cases
         confirmed_7day_avg = rollmean(x = confirmed_this_day, k = 7, align = "right",  
                                       fill = na.fill(confirmed_this_day, 0)),
         confirmed_14day_avg = rollmean(x = confirmed_this_day, k = 14, align = "right",  
                                        fill = na.fill(confirmed_this_day, 0)),
         resadmin1_correct = as.factor(resadmin1_correct),
         resadmin1_correct = fct_relevel(resadmin1_correct, "Other", after = Inf)) %>% 
  ggplot() +
  # stacked bar graph
  geom_bar(aes(x = report_date, y = confirmed_this_day, fill = resadmin1_correct), 
           stat = "identity", position = "stack", colour = alpha("white", 0.05)) +
  scale_fill_manual(values = my_palette) +
  # Moving Average
  geom_line(data = df_daily_national  %>% 
              # creating a variable with moving average; needed to plot 7 and 14 days mov. avg. lines
              gather(key = "moving_avg", value = "confirmed_avg",
                     c("confirmed_7day_avg", "confirmed_14day_avg")) %>%
              select(report_date, confirmed_avg, moving_avg) %>%
              filter(moving_avg == "confirmed_7day_avg" | moving_avg == "confirmed_14day_avg") %>%
              mutate(moving_avg = recode_factor(moving_avg, 
                                                "confirmed_7day_avg" = "7-day \nrolling avg",
                                                "confirmed_14day_avg" = "14-day \nrolling avg")), 
            aes(x = report_date, y = confirmed_avg, linetype = moving_avg)) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE), 
                          max(df_daily$report_date, na.rm = TRUE))) +
  ggtitle(paste("Daily COVID-19 confirmed cases across regions of", my_country),
          subtitle = paste0("(National cumulative total as at ",
                            format.Date(max(df_daily_national$report_date), "%b %d"), 
                            ": ",
                            max(df_daily_national$cum_confirmed, na.rm = T),
                            " confirmed cases)")) +
  labs(x = "", y = "Absolute number of \n daily confirmed cases") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8))

ggsave(paste0(out_path, my_country, "_Epicurve_Abs_CasesConfirmed.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


# daily relative confirmed cases plot per region
df_daily %>% 
  ungroup() %>%
  select(report_date, resadmin1_correct, confirmed_this_day) %>% 
  # what prop of total cases confirmed in a day are in a specific region?
  group_by(report_date) %>% 
  mutate(confirmed_prop =  100 * confirmed_this_day/ sum(confirmed_this_day)) %>% 
  # Maintaining only the 4 provinces with highest number of cases, 
  # others are summed and recoded
  mutate(resadmin1_correct = case_when(!resadmin1_correct %in% provinces_most_confirmed 
                                       | resadmin1_correct == "NA" ~ "All others",
                                       TRUE ~ as.character(resadmin1_correct))) %>%
  # push "All others" to the last level
  mutate(resadmin1_correct = as.factor(resadmin1_correct),
         resadmin1_correct = fct_relevel(resadmin1_correct, "All others", after = Inf)) %>% 
  # we need to add up the case counts for the newly created "Other" factor levels
  # we do this with the summarise function
  group_by(resadmin1_correct, report_date) %>% 
  summarise(confirmed_this_day = sum(confirmed_this_day),
            confirmed_prop = sum(confirmed_prop)) %>%
  group_by(resadmin1_correct) %>% 
  filter(max(confirmed_this_day) > 0) %>% 
  ggplot() +
  # stacked bar graph
  geom_bar(aes(x = report_date, y = confirmed_prop, fill = resadmin1_correct), 
           stat = "identity", position = "stack", colour = alpha("white", 0.05), size = 0.1) +
  scale_fill_manual(values = my_palette) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE), 
                          max(df_daily$report_date, na.rm = TRUE))) +
  ggtitle(paste("Daily COVID-19 confirmed cases across regions of", my_country, 
                "(relative proportions)"),
          subtitle = paste0("(National cumulative total as at ",
                            format.Date(max(df_daily_national$report_date), "%b %d"), 
                            ": ",
                            max(df_daily_national$cum_confirmed, na.rm = T),
                            " confirmed cases)")) +
  labs(x = "", y = "Percentage of \n daily confirmed cases") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(1, "line"))

ggsave(paste0(out_path, my_country, "_Epicurve_Rel_CasesConfirmed.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# summary statistics for confirmed cases
pander(df_daily %>% 
         ungroup() %>%
         select(report_date, resadmin1_correct, confirmed_this_day) %>% 
         mutate(resadmin1_correct = as.factor(resadmin1_correct),
                resadmin1_correct = fct_relevel(resadmin1_correct, "No Info", after = Inf)) %>% 
         group_by(resadmin1_correct) %>%
         summarise(`Confirmed cases (mean)` = mean(confirmed_this_day, na.rm = TRUE),
                   `Confirmed cases (median)` = median(confirmed_this_day, na.rm = TRUE),
                   `Confirmed cases (min)` = min(confirmed_this_day, na.rm = TRUE),
                   `Confirmed cases (max)` = max(confirmed_this_day, na.rm = TRUE),
                   `Confirmed cases (std. dev)` = sd(confirmed_this_day, na.rm = TRUE),
                   `Confirmed cases (total)` = sum(confirmed_this_day, na.rm = TRUE)))


##################################################################
##      Daily absolute and relative deaths plot per region      ##
##                      and summary statistics                  ##
##################################################################

# Filtering provinces with low number of deaths
provinces_most_deaths <- df_daily %>%
  group_by(resadmin1_correct) %>% 
  # Discovering and ranking which provinces have more cases
  summarise(deaths_this_day = sum(deaths_this_day)) %>%
  arrange(-deaths_this_day) %>% 
  # Selecting only top four (can be changed)
  slice(1:4) %>% 
  select(resadmin1_correct) %>% 
  unique() %>% 
  pull() %>% 
  as.character()

# daily absolute deaths plot per region
df_daily %>% 
  ungroup() %>%
  select(report_date, resadmin1_correct, deaths_this_day) %>% 
  mutate(resadmin1_correct =
           # Maintaining only the 5 provinces with highest number of cases, 
           # others are summed and recoded
           case_when(!resadmin1_correct %in% provinces_most_deaths 
                     | resadmin1_correct == "NA" ~ "Other",
                     TRUE ~ as.character(resadmin1_correct)),
         # 7 days moving average for deaths
         deaths_7day_avg = rollmean(x = deaths_this_day, k = 7, align = "right",  
                                    fill = na.fill(deaths_this_day, 0)),
         deaths_14day_avg = rollmean(x = deaths_this_day, k = 14, align = "right",  
                                     fill = na.fill(deaths_this_day, 0)),
         resadmin1_correct = as.factor(resadmin1_correct),
         resadmin1_correct = fct_relevel(resadmin1_correct, "Other", after = Inf)) %>% 
  ggplot() +
  # stacked bar graph
  geom_bar(aes(x = report_date, y = deaths_this_day, fill = resadmin1_correct), 
           stat = "identity", position = "stack", colour = alpha("white", 0.05)) +
  scale_fill_manual(values = my_palette) +
  # Moving Average
  geom_line(data = df_daily_national  %>% 
              # creating a variable with moving average; needed to plot 7 and 14 days mov. avg. lines
              gather(key = "moving_avg", value = "deaths_avg",
                     c("deaths_7day_avg", "deaths_14day_avg")) %>%
              select(report_date, deaths_avg, moving_avg) %>%
              filter(moving_avg == "deaths_7day_avg" | moving_avg == "deaths_14day_avg") %>%
              mutate(moving_avg = recode_factor(moving_avg, 
                                                "deaths_7day_avg" = "7-day \nrolling avg",
                                                "deaths_14day_avg" = "14-day \nrolling avg")), 
            aes(x = report_date, y = deaths_avg, linetype = moving_avg)) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE), 
                          max(df_daily$report_date, na.rm = TRUE))) +
  ggtitle(paste("Daily COVID-19 deaths across regions of", my_country),
          subtitle = paste0("(National cumulative total as at ",
                            format.Date(max(df_daily_national$report_date), "%b %d"), 
                            ": ",
                            max(df_daily_national$cum_deaths, na.rm = T),
                            " deaths)")) +
  labs(x = "", y = "Absolute number of \n daily deaths") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8))

ggsave(paste0(out_path, my_country, "_Epicurve_Abs_Deaths.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


# daily relative deaths plot per region
df_daily %>% 
  ungroup() %>%
  select(report_date, resadmin1_correct, deaths_this_day) %>% 
  # what prop of total cases deaths in a day are in a specific region?
  group_by(report_date) %>% 
  mutate(deaths_prop =  100 * deaths_this_day/ sum(deaths_this_day)) %>% 
  # Maintaining only the 4 provinces with highest number of cases, 
  # others are summed and recoded
  mutate(resadmin1_correct = case_when(!resadmin1_correct %in% provinces_most_deaths 
                                       | resadmin1_correct == "NA" ~ "All others",
                                       TRUE ~ as.character(resadmin1_correct))) %>%
  # push "All others" to the last level
  mutate(resadmin1_correct = as.factor(resadmin1_correct),
         resadmin1_correct = fct_relevel(resadmin1_correct, "All others", after = Inf)) %>% 
  # we need to add up the case counts for the newly created "Other" factor levels
  # we do this with the summarise function
  group_by(resadmin1_correct, report_date) %>% 
  summarise(deaths_this_day = sum(deaths_this_day),
            deaths_prop = sum(deaths_prop)) %>%
  group_by(resadmin1_correct) %>% 
  filter(max(deaths_this_day) > 0) %>% 
  ggplot() +
  # stacked bar graph
  geom_bar(aes(x = report_date, y = deaths_prop, fill = resadmin1_correct), 
           stat = "identity", position = "stack", colour = alpha("white", 0.05), size = 0.1) +
  scale_fill_manual(values = my_palette) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE), 
                          max(df_daily$report_date, na.rm = TRUE))) +
  ggtitle(paste("Daily COVID-19 deaths across regions of", my_country, 
                "(relative proportions)"),
          subtitle = paste0("(National cumulative total as at ",
                            format.Date(max(df_daily_national$report_date), "%b %d"), 
                            ": ",
                            max(df_daily_national$cum_deaths, na.rm = T),
                            " deaths)")) +
  labs(x = "", y = "Percentage of \n daily deaths") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(1, "line"))

ggsave(paste0(out_path, my_country, "_Epicurve_Rel_Deaths.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# summary statistics for deaths
pander(df_daily %>% 
         ungroup() %>%
         select(report_date, resadmin1_correct, deaths_this_day) %>% 
         mutate(resadmin1_correct = as.factor(resadmin1_correct),
                resadmin1_correct = fct_relevel(resadmin1_correct, "No Info", after = Inf)) %>% 
         group_by(resadmin1_correct) %>%
         summarise(`Deaths (mean)` = mean(deaths_this_day, na.rm = TRUE),
                   `Deaths (median)` = median(deaths_this_day, na.rm = TRUE),
                   `Deaths (min)` = min(deaths_this_day, na.rm = TRUE),
                   `Deaths (max)` = max(deaths_this_day, na.rm = TRUE),
                   `Deaths (std. dev)` = sd(deaths_this_day, na.rm = TRUE),
                   `Deaths (total)` = sum(deaths_this_day, na.rm = TRUE)))


#########################################################
##      Daily absolute discharges plot per region      ##
##                and summary statistics               ##
#########################################################

# Filtering provinces with low number of discharges
provinces_most_discharges <- df_daily %>%
  group_by(resadmin1_correct) %>% 
  # Discovering and ranking which provinces have more cases
  summarise(discharges_this_day = sum(discharges_this_day)) %>%
  arrange(-discharges_this_day) %>% 
  # Selecting only top four (can be changed)
  slice(1:4) %>% 
  select(resadmin1_correct) %>% 
  unique() %>% 
  pull() %>% 
  as.character()

# daily absolute discharges plot per region
df_daily %>% 
  ungroup() %>%
  select(report_date, resadmin1_correct, discharges_this_day) %>% 
  mutate(resadmin1_correct =
           # Maintaining only the 5 provinces with highest number of discharges, 
           # others are summed and recoded
           case_when(!resadmin1_correct %in% provinces_most_discharges 
                     | resadmin1_correct == "NA" ~ "Other",
                     TRUE ~ as.character(resadmin1_correct)),
         # 7 days moving average for discharges
         discharges_7day_avg = rollmean(x = discharges_this_day, k = 7, align = "right",  
                                        fill = na.fill(discharges_this_day, 0)),
         discharges_14day_avg = rollmean(x = discharges_this_day, k = 14, align = "right",  
                                         fill = na.fill(discharges_this_day, 0)),
         resadmin1_correct = as.factor(resadmin1_correct),
         resadmin1_correct = fct_relevel(resadmin1_correct, "Other", after = Inf)) %>% 
  ggplot() +
  # stacked bar graph
  geom_bar(aes(x = report_date, y = discharges_this_day, fill = resadmin1_correct), 
           stat = "identity", position = "stack", colour = alpha("white", 0.05)) +
  scale_fill_manual(values = my_palette) +
  # Moving Average
  geom_line(data = df_daily_national  %>% 
              # creating a variable with moving average; needed to plot 7 and 14 days mov. avg. lines
              gather(key = "moving_avg", value = "discharges_avg",
                     c("discharges_7day_avg", "discharges_14day_avg")) %>%
              select(report_date, discharges_avg, moving_avg) %>%
              filter(moving_avg == "discharges_7day_avg" | moving_avg == "discharges_14day_avg") %>%
              mutate(moving_avg = recode_factor(moving_avg, 
                                                "discharges_7day_avg" = "7-day \nrolling avg",
                                                "discharges_14day_avg" = "14-day \nrolling avg")), 
            aes(x = report_date, y = discharges_avg, linetype = moving_avg)) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE), 
                          max(df_daily$report_date, na.rm = TRUE))) +
  ggtitle(paste("Daily COVID-19 discharges across regions of", my_country),
          subtitle = paste0("(National cumulative total as at ",
                            format.Date(max(df_daily_national$report_date), "%b %d"), 
                            ": ",
                            max(df_daily_national$cum_discharges, na.rm = T),
                            " discharges)")) +
  labs(x = "", y = "Absolute number of \n daily discharges") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8))

ggsave(paste0(out_path, my_country, "_Epicurve_Abs_Discharges.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


# daily relative discharges plot per region
df_daily %>% 
  ungroup() %>%
  select(report_date, resadmin1_correct, discharges_this_day) %>% 
  # what prop of total cases discharges in a day are in a specific region?
  group_by(report_date) %>% 
  mutate(discharges_prop =  100 * discharges_this_day/ sum(discharges_this_day)) %>% 
  # Maintaining only the 4 provinces with highest number of cases, 
  # others are summed and recoded
  mutate(resadmin1_correct = case_when(!resadmin1_correct %in% provinces_most_discharges 
                                       | resadmin1_correct == "NA" ~ "All others",
                                       TRUE ~ as.character(resadmin1_correct))) %>%
  # push "All others" to the last level
  mutate(resadmin1_correct = as.factor(resadmin1_correct),
         resadmin1_correct = fct_relevel(resadmin1_correct, "All others", after = Inf)) %>% 
  # we need to add up the case counts for the newly created "Other" factor levels
  # we do this with the summarise function
  group_by(resadmin1_correct, report_date) %>% 
  summarise(discharges_this_day = sum(discharges_this_day),
            discharges_prop = sum(discharges_prop)) %>%
  group_by(resadmin1_correct) %>% 
  filter(max(discharges_this_day) > 0) %>% 
  ggplot() +
  # stacked bar graph
  geom_bar(aes(x = report_date, y = discharges_prop, fill = resadmin1_correct), 
           stat = "identity", position = "stack", colour = alpha("white", 0.05), size = 0.1) +
  scale_fill_manual(values = my_palette) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE), 
                          max(df_daily$report_date, na.rm = TRUE))) +
  ggtitle(paste("Daily COVID-19 discharges across regions of", my_country, 
                "(relative proportions)"),
          subtitle = paste0("(National cumulative total as at ",
                            format.Date(max(df_daily_national$report_date), "%b %d"), 
                            ": ",
                            max(df_daily_national$cum_discharges, na.rm = T),
                            " discharges)")) +
  labs(x = "", y = "Percentage of \n daily discharges") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(1, "line"))

ggsave(paste0(out_path, my_country, "_Epicurve_Rel_Discharges.png"), plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# summary statistics for discharges
pander(df_daily %>% 
         ungroup() %>%
         select(report_date, resadmin1_correct, discharges_this_day) %>% 
         mutate(resadmin1_correct = as.factor(resadmin1_correct),
                resadmin1_correct = fct_relevel(resadmin1_correct, "No Info", after = Inf)) %>% 
         group_by(resadmin1_correct) %>%
         summarise(`Discharges (mean)` = mean(discharges_this_day, na.rm = TRUE),
                   `Discharges (median)` = median(discharges_this_day, na.rm = TRUE),
                   `Discharges (min)` = min(discharges_this_day, na.rm = TRUE),
                   `Discharges (max)` = max(discharges_this_day, na.rm = TRUE),
                   `Discharges (std. dev)` = sd(discharges_this_day, na.rm = TRUE),
                   `Discharges (total)` = sum(discharges_this_day, na.rm = TRUE)))


#######################################
##      AUTOMATIC FACETTED PLOT      ##
#######################################

#################################################
##      Daily reported cases per region facet  ##
#################################################

# there are six elements to this plot:
# 1. a bar chart that shows the log of daily cases for each region
# 
# 2. a red geom_line that tracks the seven day average of cases. This metric was already built in the epicurve df above
# 
# 3. A geom_point that is anchored at x= with the maximum cases, y = cases that day (log)
# 
# 4. A geom_label that is also anchored at the same place, which pastes:
#                 a. the date of maximum cases
#                 b. the number of cases that day
# Note: 3 and 4 are created in a separate df, called max_tag_reported or other variants
# the geom_point and geom_label geoms thus use a separate df than the other geoms
#
# 5. A geom_label which pastes the total number of cases at the top right 
# (it is anchored to the actual values of the plot in order to get it stay there)
#
# 6. A geom_label which pastes the past 14 days of cases
# Note: number 6 is also built in a separate data frame. 
# (if you have an idea for doing this in a single pipe chain, please change it!!)
# Here, we left_join it with the main epicurve while plotting
# 
# Because some countries have a high number of regions, we are locking the facet number to a
# max of 12 facets per plot. This is done using facet_wrap_paginate() from ggforce package.
# 
# Plotting time takes a while, especially for countries with high number of regions (because
# of the loop structure) so be patient.


# create a new data frame to house the aesthetics for the labels
max_tag_reported <- df_daily %>%
  # paste region and cases at day with max number of cases. Needed for plot
  group_by(resadmin1_correct) %>% 
  # plural
  mutate(label_max_cases = 
           if_else(reported_this_day == max(reported_this_day, na.rm=T) & 
                     max(reported_this_day, na.rm=T) > 1,
                   paste(as.character(reported_this_day), "reported cases,\n", 
                         format.Date(report_date, format = "%b %d" )), NA_character_) )%>% 
  # singular 
  mutate(label_max_cases = 
           if_else(reported_this_day == max(reported_this_day, na.rm=T) & 
                     max(reported_this_day, na.rm=T) <= 1,
                   paste(as.character(reported_this_day), "case,\n", 
                         format.Date(report_date, format = "%b %d" )),
                   label_max_cases) )%>% 
  mutate(label_max_point = if_else(reported_this_day == max(reported_this_day, na.rm=T),
                                   reported_this_day , NaN) )%>%
  filter(!is.na(label_max_cases)) %>% 
  # if there are multiple days with maximum cases, keep only the latest one
  top_n(n = 1, wt = report_date) %>% 
  ungroup() %>% 
  # label regions with lot of cases. Will user later to prevent overlap with heading
  mutate(cases_high = if_else(cum_reported > quantile(cum_reported, 0.9), "high", "low")) %>%
  # removing all regions that do not have reported cases
  filter(cum_reported > 0)

# for the past 14 days of cases tag
tag_14d_reported <-  df_daily %>%
  group_by(resadmin1_correct) %>%
  top_n(1, wt = report_date) %>% 
  select(resadmin1_correct, reported_past_14) %>% 
  transmute(most_recent_reported_past_14 = paste("Total in the past 14 days:", 
                                                 reported_past_14))

# The number of unique regions will define the number of plots
n_plots_reported <- ceiling(length(unique(df_daily$resadmin1_correct)) / 9)

# Number of columns for facet plot
ncol_reported <- ifelse(length(unique(max_tag_reported$resadmin1_correct)) > 3, 3, 
                        length(unique(max_tag_reported$resadmin1_correct)))

# Daily reported cases plot region facet
# this is a loop to create separate plots if total regions exceed 12, so that visualization is maintained. It might take a while to run all figures.
for (i in seq_len(n_plots_reported)) {
  print(
    df_daily %>% 
      group_by(resadmin1_correct) %>% 
      # keep only regions with at last one case
      filter(max(cum_reported) > 0) %>% 
      # build a label for total cases and last 14 
      mutate(label_total_cases = paste("Total cases:", 
                                       as.character(max(cum_reported, na.rm = T)))) %>%
      ungroup() %>% 
      # add in tag for 14 day cases
      left_join(tag_14d_reported) %>% 
      # plot
      ggplot() +
      # Pushing "No info" to the last level
      facet_wrap_paginate(~ fct_relevel(resadmin1_correct, "No Info", after = Inf), 
                          # Making the number of facets automatic (so that countries with a high or low number 
                          # of regions can be visualized)
                          ncol = ncol_reported, 
                          nrow = ceiling(length(unique(max_tag_reported$resadmin1_correct)) / n_plots_reported / ncol_reported), 
                          page = i) +
      # bar chart shows log-transformed daily cases
      geom_bar(aes(x = report_date, y = reported_log ), stat = "identity", 
               colour = alpha("white", 0.05), fill = alpha("red", 0.5)) +
      # trend line shows log-transformed seven-day-avg cases. Includes arrow
      geom_line(aes(x = report_date, y = reported_trend_log),
                colour = alpha("black", 0.8), size=.55) +
      # first and second labels are anchored at first date, max reported (top left)
      geom_label(aes(x = min(report_date, na.rm = T), 
                     y = max(reported_log, na.rm = T), 
                     label = paste0(label_total_cases, "\n", most_recent_reported_past_14)),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.8),
                 hjust = 0, nudge_x = 0.1, label.size = NA, lineheight = 1) +
      # third label is anchored at the maximum daily case count for the region (log transformed)
      # but displays the non-transformed count of daily cases + date
      # we plot two different labels to avoid overlap with the facet header
      # high cases get vjust of 1 and low cases get vjust of 0
      geom_label(data = subset(max_tag_reported, cases_high == "high"), 
                 aes(x = report_date, y = reported_log, label = label_max_cases),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.7),
                 hjust = 1, vjust = 1, label.size = NA, lineheight = 1) + 
      geom_label(data = subset(max_tag_reported, cases_high == "low"), 
                 aes(x = report_date, y = reported_log, label = label_max_cases),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.7),
                 hjust = 1, vjust = 0, label.size = NA, lineheight = 1) + 
      # point is also anchored at the maximum daily case count for the region (log transformed)
      geom_point(data = max_tag_reported, 
                 aes(x = report_date, y = reported_log),
                 na.rm = TRUE, colour = alpha("#ff4c22", 0.9),
                 shape = 16, size = 1.2) +
      # expand the limits to make space for the labels
      scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                         labels = c("0", "10", "100", "1000", "10000"),
                         limits = c(0, max(df_daily$reported_log) * 1.2)) +
      scale_x_date(breaks = coveredsundays5,
                   labels = epiweekanddate5,
                   limits = c(min(df_daily$report_date, na.rm = TRUE), 
                              max(df_daily$report_date, na.rm = TRUE))) +
      ggtitle(paste("Daily reported cases for each region in", my_country)) +
      labs(x = "", y = "Cases reported (log-transformed)", 
           subtitle = "Single day peak, total cumulative cases and cases in the past 14 days are indicated. Scale is log-transformed",
           caption = "Red lines show seven-day average.\nAny regions not shown have had no reported cases") +
      theme_bw() +
      theme(strip.background = element_rect(fill = "dodgerblue4" , colour = "transparent"),
            strip.text = element_text(size = 8, colour = "white"),
            axis.text.x = element_text(size = 6.5),
            axis.text.y = element_text(size = 6))
  )
  
  ggsave(paste0(out_path, my_country, "_Facet_CasesReported_", i, ".png"), 
         plot = last_plot(), 
         width = 35, height = 20, units = "cm", dpi = 600)
}


######################################################
##      Daily confirmed cases per region facet      ##
######################################################

# create a new data frame to house the aesthetics for the labels
max_tag_confirmed <- df_daily %>%
  # paste region and cases at day with max number of cases. Needed for plot
  group_by(resadmin1_correct) %>% 
  # plural
  mutate(label_max_cases = 
           if_else(confirmed_this_day == max(confirmed_this_day, na.rm=T) & 
                     max(confirmed_this_day, na.rm=T) > 1,
                   paste(as.character(confirmed_this_day), "confirmed cases,\n", 
                         format.Date(report_date, format = "%b %d" )), NA_character_) )%>% 
  # singular 
  mutate(label_max_cases = 
           if_else(confirmed_this_day == max(confirmed_this_day, na.rm=T) & 
                     max(confirmed_this_day, na.rm=T) <= 1,
                   paste(as.character(confirmed_this_day), "case,\n", 
                         format.Date(report_date, format = "%b %d" )),
                   label_max_cases) )%>% 
  mutate(label_max_point = if_else(confirmed_this_day == max(confirmed_this_day, na.rm=T),
                                   confirmed_this_day , NaN) )%>%
  filter(!is.na(label_max_cases)) %>% 
  # if there are multiple days with maximum cases, keep only the latest one
  top_n(n = 1, wt = report_date) %>% 
  ungroup() %>% 
  # label regions with lot of cases. Will user later to prevent overlap with heading
  mutate(cases_high = if_else(cum_confirmed > quantile(cum_confirmed, 0.9), "high", "low")) %>%
  # removing all regions that do not have reported cases
  filter(cum_confirmed > 0)

# for the past 14 days of cases tag
tag_14d_confirmed <- df_daily %>%
  group_by(resadmin1_correct) %>%
  top_n(1, wt = report_date) %>% 
  select(resadmin1_correct, confirmed_past_14) %>% 
  transmute(most_recent_confirmed_past_14 = paste("Total in the past 14 days:", 
                                                  confirmed_past_14))

# The number of unique regions will define the number of plots
n_plots_confirmed <- ceiling(length(unique(max_tag_confirmed$resadmin1_correct)) / 9)

# Number of columns for facet plot
ncol_confirmed <- ifelse(length(unique(max_tag_confirmed$resadmin1_correct)) > 3, 3, 
                         length(unique(max_tag_confirmed$resadmin1_correct)))

# Daily confirmed cases plot region facet
# this is a loop to create separate plots if total regions exceed 12, so that visualization is maintained. It might take a while to run all figures.
for (i in seq_len(n_plots_confirmed)) {
  print(
    df_daily %>% 
      group_by(resadmin1_correct) %>% 
      # keep only regions with at last one case
      filter(max(cum_confirmed) > 0) %>% 
      # build a label for total cases and last 14 
      mutate(label_total_cases = paste("Total cases:", 
                                       as.character(max(cum_confirmed, na.rm = T)))) %>%
      ungroup() %>% 
      # add in tag for 14 day cases
      left_join(tag_14d_confirmed) %>% 
      # plot
      ggplot() +
      # Pushing "No info" to the last level
      facet_wrap_paginate(~ fct_relevel(resadmin1_correct, "No Info", after = Inf), 
                          # Making the number of facets automatic (so that countries with a high or low number 
                          # of regions can be visualized)
                          ncol = ncol_confirmed, 
                          nrow = ceiling(length(unique(max_tag_confirmed$resadmin1_correct)) / n_plots_confirmed / ncol_confirmed), 
                          page = i) +
      # bar chart shows log-transformed daily cases
      geom_bar(aes(x = report_date, y = confirmed_log ), stat = "identity", 
               colour = alpha("white", 0.05), fill = alpha("red", 0.5)) +
      # trend line shows log-transformed seven-day-avg cases. Includes arrow
      geom_line(aes(x = report_date, y = confirmed_trend_log),
                colour = alpha("black", 0.8), size=.55) +
      # first and second labels are anchored at first date, max confirmed * 1.22 (top right)
      geom_label(aes(x = min(report_date, na.rm = T), 
                     y = max(confirmed_log, na.rm = T), 
                     label = paste0(label_total_cases, "\n", most_recent_confirmed_past_14)),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.8),
                 hjust = 0, nudge_x = 0.1, label.size = NA, lineheight = 1) +
      # third label is anchored at the maximum daily case count for the region (log transformed)
      # but displays the non-transformed count of daily cases + date
      # we plot two different labels to avoid overlap with the facet header
      # high cases get vjust of 1 and low cases get vjust of 0
      geom_label(data = subset(max_tag_confirmed, cases_high == "high"), 
                 aes(x = report_date, y = confirmed_log, label = label_max_cases),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.7),
                 hjust = 1, vjust = 1, label.size = NA, lineheight = 1) + 
      geom_label(data = subset(max_tag_confirmed, cases_high == "low"), 
                 aes(x = report_date, y = confirmed_log, label = label_max_cases),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.7),
                 hjust = 1, vjust = 0, label.size = NA, lineheight = 1) + 
      # point is also anchored at the maximum daily case count for the region (log transformed)
      geom_point(data = max_tag_confirmed, 
                 aes(x = report_date, y = confirmed_log),
                 na.rm = TRUE, colour = alpha("#ff4c22", 0.9),
                 shape = 16, size = 1.2) +
      # expand the limits to make space for the labels
      scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                         labels = c("0", "10", "100", "1000", "10000"),
                         limits = c(0, max(df_daily$confirmed_log) * 1.2)) +
      scale_x_date(breaks = coveredsundays5,
                   labels = epiweekanddate5,
                   limits = c(min(df_daily$report_date, na.rm = TRUE), 
                              max(df_daily$report_date, na.rm = TRUE))) +
      ggtitle(paste("Daily confirmed cases for each region in", my_country)) +
      labs(x = "", y = "Cases confirmed (log-transformed)", 
           subtitle = "Single day peak, total cumulative cases and cases in the past 14 days are indicated. Scale is log-transformed",
           caption = "Red lines show seven-day average.\nAny regions not shown have had no confirmed cases") +
      theme_bw() +
      theme(strip.background = element_rect(fill = "dodgerblue4" , colour = "transparent"),
            strip.text = element_text(size = 8, colour = "white"),
            axis.text.x = element_text(size = 6.5),
            axis.text.y = element_text(size = 6))
  )
  
  ggsave(paste0(out_path, my_country, "_Facet_CasesConfirmed_", i, ".png"), 
         plot = last_plot(), 
         width = 35, height = 20, units = "cm", dpi = 600)
}


############################################
##      Daily deaths per region facet     ##
############################################

# create a new data frame to house the aesthetics for the labels
max_tag_deaths <- df_daily %>%
  # paste region and deaths at day with max number of deaths. Needed for plot
  group_by(resadmin1_correct) %>% 
  # plural
  mutate(label_max_deaths = 
           if_else(deaths_this_day == max(deaths_this_day, na.rm=T) & 
                     max(deaths_this_day, na.rm=T) > 1,
                   paste(as.character(deaths_this_day), "deaths,\n", 
                         format.Date(report_date, format = "%b %d" )), NA_character_) )%>% 
  # singular 
  mutate(label_max_deaths = 
           if_else(deaths_this_day == max(deaths_this_day, na.rm=T) & 
                     max(deaths_this_day, na.rm=T) <= 1,
                   paste(as.character(deaths_this_day), "death,\n", 
                         format.Date(report_date, format = "%b %d" )),
                   label_max_deaths) )%>% 
  mutate(label_max_point = if_else(deaths_this_day == max(deaths_this_day, na.rm=T),
                                   deaths_this_day , NaN) )%>%
  filter(!is.na(label_max_deaths)) %>% 
  # if there are multiple days with maximum deaths, keep only the latest one
  top_n(n = 1, wt = report_date) %>% 
  ungroup() %>% 
  # label regions with lot of deaths. Will user later to prevent overlap with heading
  mutate(deaths_high = if_else(cum_deaths > quantile(cum_deaths, 0.9), "high", "low")) %>%
  # removing all regions that do not have reported deaths
  filter(cum_deaths > 0)

# for the past 14 days of deaths tag
tag_14d_deaths <-  df_daily %>%
  group_by(resadmin1_correct) %>%
  top_n(1, wt = report_date) %>% 
  select(resadmin1_correct, deaths_past_14) %>% 
  transmute(most_recent_deaths_past_14 = paste("Total in the past 14 days:", 
                                               deaths_past_14))

# The number of unique regions will define the number of plots
n_plots_deaths <- ceiling(length(unique(max_tag_deaths$resadmin1_correct)) / 9)

# Number of columns for facet plot
ncol_deaths <- ifelse(length(unique(max_tag_deaths$resadmin1_correct)) > 3, 3, 
                      length(unique(max_tag_deaths$resadmin1_correct)))

# Daily deaths plot region facet
# this is a loop to create separate plots if total regions exceed 12, so that visualization is maintained. It might take a while to run all figures.
for (i in seq_len(n_plots_deaths)) {
  print(
    df_daily %>% 
      group_by(resadmin1_correct) %>% 
      # keep only regions with at last one death
      filter(max(cum_deaths) > 0) %>% 
      # build a label for total deaths and last 14 
      mutate(label_total_deaths = paste("Total deaths:", 
                                        as.character(max(cum_deaths, na.rm = T)))) %>%
      ungroup() %>% 
      # add in tag for 14 day deaths
      left_join(tag_14d_deaths) %>% 
      # plot
      ggplot() +
      # Pushing "No info" to the last level
      facet_wrap_paginate(~ fct_relevel(resadmin1_correct, "No Info", after = Inf), 
                          # Making the number of facets automatic (so that countries with a high or low number 
                          # of regions can be visualized)
                          ncol = ncol_deaths, 
                          nrow = ceiling(length(unique(max_tag_deaths$resadmin1_correct)) / n_plots_deaths / ncol_deaths), 
                          page = i) +
      # bar chart shows log-transformed daily deaths
      geom_bar(aes(x = report_date, y = deaths_log ), stat = "identity", 
               colour = alpha("white", 0.05), fill = alpha("red", 0.5)) +
      # trend line shows log-transformed seven-day-avg deaths. Includes arrow
      geom_line(aes(x = report_date, y = deaths_trend_log),
                colour = alpha("black", 0.8), size=.55) +
      # first and second labels are anchored at first date, max deaths * 1.22 (top right)
      geom_label(aes(x = min(report_date, na.rm = T), 
                     y = max(deaths_log, na.rm = T), 
                     label = paste0(label_total_deaths, "\n", most_recent_deaths_past_14)),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.8),
                 hjust = 0, nudge_x = 0.1, label.size = NA, lineheight = 1) +
      # third label is anchored at the maximum daily death count for the region (log transformed)
      # but displays the non-transformed count of daily deaths + date
      # we plot two different labels to avoid overlap with the facet header
      # high deaths get vjust of 1 and low deaths get vjust of 0
      geom_label(data = subset(max_tag_deaths, deaths_high == "high"), 
                 aes(x = report_date, y = deaths_log, label = label_max_deaths),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.7),
                 hjust = 1, vjust = 1, label.size = NA, lineheight = 1) + 
      geom_label(data = subset(max_tag_deaths, deaths_high == "low"), 
                 aes(x = report_date, y = deaths_log, label = label_max_deaths),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.7),
                 hjust = 1, vjust = 0, label.size = NA, lineheight = 1) + 
      # point is also anchored at the maximum daily death count for the region (log transformed)
      geom_point(data = max_tag_deaths, 
                 aes(x = report_date, y = deaths_log),
                 na.rm = TRUE, colour = alpha("#ff4c22", 0.9),
                 shape = 16, size = 1.2) +
      # expand the limits to make space for the labels
      scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                         labels = c("0", "10", "100", "1000", "10000"),
                         limits = c(0, max(df_daily$deaths_log) * 1.2)) +
      scale_x_date(breaks = coveredsundays5,
                   labels = epiweekanddate5,
                   limits = c(min(df_daily$report_date, na.rm = TRUE), 
                              max(df_daily$report_date, na.rm = TRUE))) +
      ggtitle(paste("Daily deaths for each region in", my_country)) +
      labs(x = "", y = "Deaths (log-transformed)", 
           subtitle = "Single day peak, total cumulative deaths and deaths in the past 14 days are indicated. Scale is log-transformed",
           caption = "Red lines show seven-day average.\nAny regions not shown have had no deaths") +
      theme_bw() +
      theme(strip.background = element_rect(fill = "dodgerblue4" , colour = "transparent"),
            strip.text = element_text(size = 8, colour = "white"),
            axis.text.x = element_text(size = 6.5),
            axis.text.y = element_text(size = 6))
  )
  
  ggsave(paste0(out_path, my_country, "_Facet_Deaths_", i, ".png"), 
         plot = last_plot(), 
         width = 35, height = 20, units = "cm", dpi = 600)
}


#############################################
##      Daily discharges per region facet  ##
#############################################

# create a new data frame to house the aesthetics for the labels
max_tag_discharges <- df_daily %>%
  # paste region and discharges at day with max number of discharges. Needed for plot
  group_by(resadmin1_correct) %>% 
  # plural
  mutate(label_max_discharges = 
           if_else(discharges_this_day == max(discharges_this_day, na.rm=T) & 
                     max(discharges_this_day, na.rm=T) > 1,
                   paste(as.character(discharges_this_day), "discharges,\n", 
                         format.Date(report_date, format = "%b %d" )), NA_character_) )%>% 
  # singular 
  mutate(label_max_discharges = 
           if_else(discharges_this_day == max(discharges_this_day, na.rm=T) & 
                     max(discharges_this_day, na.rm=T) <= 1,
                   paste(as.character(discharges_this_day), "discharge,\n", 
                         format.Date(report_date, format = "%b %d" )),
                   label_max_discharges) )%>% 
  mutate(label_max_point = if_else(discharges_this_day == max(discharges_this_day, na.rm=T),
                                   discharges_this_day , NaN) )%>%
  filter(!is.na(label_max_discharges)) %>% 
  # if there are multiple days with maximum discharges, keep only the latest one
  top_n(n = 1, wt = report_date) %>% 
  ungroup() %>% 
  # label regions with lot of discharges. Will user later to prevent overlap with heading
  mutate(discharges_high = if_else(cum_discharges > quantile(cum_discharges, 0.9), "high", "low")) %>%
  # removing all regions that do not have reported discharges
  filter(cum_discharges > 0)

# for the past 14 days of discharges tag
tag_14d_discharges <-  df_daily %>%
  group_by(resadmin1_correct) %>%
  top_n(1, wt = report_date) %>% 
  select(resadmin1_correct, discharges_past_14) %>% 
  transmute(most_recent_discharges_past_14 = paste("Total in the past 14 days:", 
                                                   discharges_past_14))

# The number of unique regions will define the number of plots
n_plots_discharges <- ceiling(length(unique(max_tag_discharges$resadmin1_correct)) / 12)

# Number of columns for facet plot
ncol_discharges <- ifelse(length(unique(max_tag_discharges$resadmin1_correct)) > 3, 3, 
                          length(unique(max_tag_discharges$resadmin1_correct)))

# Daily discharges plot region facet
# this is a loop to create separate plots if total regions exceed 12, so that visualization is maintained. It might take a while to run all figures.
for (i in seq_len(n_plots_discharges)) {
  print(
    df_daily %>% 
      group_by(resadmin1_correct) %>% 
      # keep only regions with at last one discharge
      filter(max(cum_discharges) > 0) %>% 
      # build a label for total discharges and last 14 
      mutate(label_total_discharges = paste("Total discharges:", 
                                            as.character(max(cum_discharges, na.rm = T)))) %>%
      ungroup() %>% 
      # add in tag for 14 day discharges
      left_join(tag_14d_discharges) %>% 
      # plot
      ggplot() +
      # 12 facets per plot up to maximum number of regions, pushing "No info" to the last level
      facet_wrap_paginate(~ fct_relevel(resadmin1_correct, "No Info", after = Inf), 
                          # Making the number of facets automatic (so that countries with a high or low number 
                          # of regions can be visualized)
                          ncol = ncol_discharges, 
                          nrow = ceiling(length(unique(mmax_tag_discharges$resadmin1_correct)) / n_plots_discharges / ncol_discharges), 
                          page = i) +
      # bar chart shows log-transformed daily discharges
      geom_bar(aes(x = report_date, y = discharges_log ), stat = "identity", 
               colour = alpha("white", 0.05), fill = alpha("red", 0.5)) +
      # trend line shows log-transformed seven-day-avg discharges. Includes arrow
      geom_line(aes(x = report_date, y = discharges_trend_log),
                colour = alpha("black", 0.8), size=.55) +
      # first and second labels are anchored at first date, max discharges * 1.22 (top right)
      geom_label(aes(x = min(report_date, na.rm = T), 
                     y = max(discharges_log, na.rm = T), 
                     label = paste0(label_total_discharges, "\n", most_recent_discharges_past_14)),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.8),
                 hjust = 0, nudge_x = 0.1, label.size = NA, lineheight = 1) +
      # third label is anchored at the maximum daily discharge count for the region (log transformed)
      # but displays the non-transformed count of daily discharges + date
      # we plot two different labels to avoid overlap with the facet header
      # high discharges get vjust of 1 and low discharges get vjust of 0
      geom_label(data = subset(max_tag_discharges, discharges_high == "high"), 
                 aes(x = report_date, y = discharges_log, label = label_max_discharges),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.7),
                 hjust = 1, vjust = 1, label.size = NA, lineheight = 1) + 
      geom_label(data = subset(max_tag_discharges, discharges_high == "low"), 
                 aes(x = report_date, y = discharges_log, label = label_max_discharges),
                 na.rm = TRUE, size = 2.2, fill = alpha("white", 0.5),
                 fontface = "plain", colour = alpha("black", 0.7),
                 hjust = 1, vjust = 0, label.size = NA, lineheight = 1) + 
      # point is also anchored at the maximum daily discharge count for the region (log transformed)
      geom_point(data = max_tag_discharges, 
                 aes(x = report_date, y = discharges_log),
                 na.rm = TRUE, colour = alpha("#ff4c22", 0.9),
                 shape = 16, size = 1.2) +
      # expand the limits to make space for the labels
      scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                         labels = c("0", "10", "100", "1000", "10000"),
                         limits = c(0, max(df_daily$discharges_log) * 1.2)) +
      scale_x_date(breaks = coveredsundays5,
                   labels = epiweekanddate5,
                   limits = c(min(df_daily$report_date, na.rm = TRUE), 
                              max(df_daily$report_date, na.rm = TRUE))) +
      ggtitle(paste("Daily discharges for each region in", my_country)) +
      labs(x = "", y = "discharges (log-transformed)", 
           subtitle = "Single day peak, total cumulative discharges and discharges in the past 14 days are indicated. Scale is log-transformed",
           caption = "Red lines show seven-day average.\nAny regions not shown have had no discharges") +
      theme_bw() +
      theme(strip.background = element_rect(fill = "dodgerblue4" , colour = "transparent"),
            strip.text = element_text(size = 8, colour = "white"),
            axis.text.x = element_text(size = 6.5),
            axis.text.y = element_text(size = 6))
  )
  
  ggsave(paste0(out_path, my_country, "_Facet_discharges_", i, ".png"), 
         plot = last_plot(), 
         width = 35, height = 20, units = "cm", dpi = 600)
}


######################################################
######################################################
##                                                  ##
##      3 CASES PER REGION PER EPIWEEK, DETAIL      ##
##                                                  ##
##                PIVOT TABLES SECTION              ##
##                                                  ##
######################################################
######################################################

#############################################
##      Pivot table of reported cases      ##
#############################################

# pivot reported cases epicurve into presentable table
df_daily_tab_reported <- df_daily %>%
  group_by(resadmin1_correct, epiweek) %>% 
  top_n(1, wt = report_date) %>% 
  # pivoting table
  pivot_wider(id_cols = resadmin1_correct, names_from = epiweek, values_from = reported_past_7) %>% 
  ungroup() %>% 
  # removing No Info region
  filter(resadmin1_correct != "No Info") %>% 
  # joining population data
  left_join(df_daily %>% select(resadmin1_correct, pop_sum) %>% summarise(population = unique(pop_sum))) %>% 
  # selecting only important variables to present
  select(resadmin1_correct,  population, everything()) %>% 
  rename(Region = resadmin1_correct , Population = population) %>% 
  mutate(Population = comma(Population, 0))

# extract epiweeks to name from table
ew_label <- df %>% 
  tabyl(epiweek) %>% 
  select(epiweek) %>% 
  pull() %>% 
  as.numeric()

# label the last day of each week ( a saturday)
how_many <- length(ew_label)
sunday_start <- format.Date(MMWRweek2Date(rep(2020, how_many), ew_label, 1),  "%b/%d")
saturday_end <- format.Date(MMWRweek2Date(rep(2020, how_many), ew_label, 7),  "%b/%d")

# paste saturday and sunday as range 
new_week_names <- paste0("Week ", ew_label, "\n", sunday_start, "\n to ", saturday_end)

# replace names
names(df_daily_tab_reported)[-c(1, 2)] <- new_week_names

# create table
tab_reported <- formattable(df_daily_tab_reported, 
                            list(formattable::area(col = 1:ncol(df_daily_tab_reported)) ~ color_tile_small_text("white", "orange"), 
                                 # just make the text small
                                 formattable::area(col = 1:2) ~ color_tile_small_text_bold("white", "white")))

# reduce table header size
names(tab_reported) <- make_header_small(names(tab_reported))

# exporting the table also prints it
export_formattable(tab_reported, paste0(out_path, my_country, "_Tab_reported.png"),
                   width = 1800, height = 1000)


##############################################
##      Pivot table of confirmed cases      ##
##############################################

# pivot confirmed cases epicurve into presentable table
df_daily_tab_confirmed <- df_daily %>%
  group_by(resadmin1_correct, epiweek) %>% 
  top_n(1, wt = report_date) %>% 
  # pivoting table
  pivot_wider(id_cols = resadmin1_correct, names_from = epiweek, values_from = confirmed_past_7) %>% 
  ungroup() %>% 
  # removing No Info region
  filter(resadmin1_correct != "No Info") %>% 
  # joining population data
  left_join(df_daily %>% select(resadmin1_correct, pop_sum) %>% summarise(population = unique(pop_sum))) %>% 
  # selecting only important variables to present
  select(resadmin1_correct,  population, everything()) %>% 
  rename(Region = resadmin1_correct , Population = population) %>% 
  mutate(Population = comma(Population, 0))

# replace names
names(df_daily_tab_confirmed)[-c(1, 2)] <- new_week_names

# create table
tab_confirmed <- formattable(df_daily_tab_confirmed, 
                             list(formattable::area(col = 1:ncol(df_daily_tab_confirmed)) ~ color_tile_small_text("white", "orange"), 
                                  # just make the text small
                                  formattable::area(col = 1:2) ~ color_tile_small_text_bold("white", "white")))

# reduce table header size
names(tab_confirmed) <- make_header_small(names(tab_confirmed))

# exporting the table also prints it
export_formattable(tab_confirmed, paste0(out_path, my_country, "_Tab_confirmed.png"),
                   width = 1800, height = 1000)


#####################################
##      Pivot table of deaths      ##
#####################################

# pivot deaths epicurve into presentable table
df_daily_tab_deaths <- df_daily %>%
  group_by(resadmin1_correct, epiweek) %>% 
  top_n(1, wt = report_date) %>% 
  # pivoting table
  pivot_wider(id_cols = resadmin1_correct, names_from = epiweek, values_from = deaths_past_7) %>% 
  ungroup() %>% 
  # removing No Info region
  filter(resadmin1_correct != "No Info") %>% 
  # joining population data
  left_join(df_daily %>% select(resadmin1_correct, pop_sum) %>% summarise(population = unique(pop_sum))) %>% 
  # selecting only important variables to present
  select(resadmin1_correct,  population, everything()) %>% 
  rename(Region = resadmin1_correct , Population = population) %>% 
  mutate(Population = comma(Population, 0))

# replace names
names(df_daily_tab_deaths)[-c(1, 2)] <- new_week_names

# create table
tab_deaths <- formattable(df_daily_tab_deaths, 
                          list(formattable::area(col = 1:ncol(df_daily_tab_deaths)) ~ color_tile_small_text("white", "orange"), 
                               # just make the text small
                               formattable::area(col = 1:2) ~ color_tile_small_text_bold("white", "white")))

# reduce table header size
names(tab_deaths) <- make_header_small(names(tab_deaths))

# exporting the table also prints it
export_formattable(tab_deaths, paste0(out_path, my_country, "_Tab_deaths.png"),
                   width = 1800, height = 1000)


##########################################################
##      Pivot table of incidence of reported cases      ##
##########################################################

# pivot deaths epicurve into presentable table
df_daily_tab_incidence_reported <- df_daily %>%
  mutate(incidence_reported_past_7 = round(reported_past_7 / pop_sum * 100000, 2)) %>%
  group_by(resadmin1_correct, epiweek) %>% 
  slice(which.max(report_date)) %>% 
  # pivoting table
  pivot_wider(id_cols = resadmin1_correct, names_from = epiweek, values_from = incidence_reported_past_7) %>% 
  ungroup() %>% 
  # removing No Info region
  filter(resadmin1_correct != "No Info") %>% 
  # joining population data
  left_join(df_daily %>% select(resadmin1_correct, pop_sum) %>% summarise(population = unique(pop_sum))) %>% 
  # selecting only important variables to present
  select(resadmin1_correct,  population, everything()) %>% 
  rename(Region = resadmin1_correct , Population = population) %>% 
  mutate(Population = comma(Population, 0))

# replace names
names(df_daily_tab_incidence_reported)[-c(1, 2)] <- new_week_names

# create table
tab_incidence <- formattable(df_daily_tab_incidence_reported, 
                             list(formattable::area(col = 1:ncol(df_daily_tab_incidence_reported)) ~ color_tile_small_text("white", "orange"), 
                                  # just make the text small
                                  formattable::area(col = 1:2) ~ color_tile_small_text_bold("white", "white")))

# reduce table header size
names(tab_incidence) <- make_header_small(names(tab_incidence))

# exporting the table also prints it
export_formattable(tab_incidence, paste0(out_path, my_country, "_Tab_incidence.png"),
                   width = 1800, height = 1000)


########################################
##      Pivot table of mortality      ##
########################################

# pivot deaths epicurve into presentable table
df_daily_tab_mortality_reported <- df_daily %>%
  mutate(mortality_past_7 = round(deaths_past_7 / pop_sum * 100000, 2)) %>%
  group_by(resadmin1_correct, epiweek) %>% 
  slice(which.max(report_date)) %>% 
  # pivoting table
  pivot_wider(id_cols = resadmin1_correct, names_from = epiweek, values_from = mortality_past_7) %>% 
  ungroup() %>% 
  # removing No Info region
  filter(resadmin1_correct != "No Info") %>% 
  # joining population data
  left_join(df_daily %>% select(resadmin1_correct, pop_sum) %>% summarise(population = unique(pop_sum))) %>% 
  # selecting only important variables to present
  select(resadmin1_correct,  population, everything()) %>% 
  rename(Region = resadmin1_correct , Population = population) %>% 
  mutate(Population = comma(Population, 0))

# replace names
names(df_daily_tab_mortality_reported)[-c(1, 2)] <- new_week_names

# create table
tab_mortality <- formattable(df_daily_tab_mortality_reported, 
                             list(formattable::area(col = 1:ncol(df_daily_tab_mortality_reported)) ~ color_tile_small_text("white", "orange"), 
                                  # just make the text small
                                  formattable::area(col = 1:2) ~ color_tile_small_text_bold("white", "white")))

# reduce table header size
names(tab_mortality) <- make_header_small(names(tab_mortality))

# exporting the table also prints it
export_formattable(tab_mortality, paste0(out_path, my_country, "_Tab_mortality.png"),
                   width = 1800, height = 1000)


########################################################
########################################################
##                                                    ##
##      4 CUMULATIVE CASES AND DEATHS PER REGION      ##
##                                                    ##
##                TIME SERIES SECTION                 ##
##                                                    ##
########################################################
########################################################


####################################################
##      Time Series of cumulative reported cases  ##
####################################################

# select the top 4 regions with the higher count of cumulative reported cases
reported_to_label <- df_daily %>% 
  group_by(resadmin1_correct) %>% 
  top_n(1, wt = report_date) %>% 
  ungroup() %>% 
  filter(cum_reported > 0) %>% 
  # selecting only top four regions
  top_n(n = 4, wt = cum_reported) %>% 
  select(resadmin1_correct) %>% 
  pull()

# subset df according to needed labels
reported_comparisons <- df_daily %>% 
  filter(resadmin1_correct %in% reported_to_label) %>% 
  # paste resadmin1_correct and reported cases at maxdate. Needed for plot
  group_by(resadmin1_correct) %>% 
  mutate(label_max_cases = if_else(report_date == max(report_date, na.rm=T),
                                   paste0(as.character(resadmin1_correct), "\n",
                                          max(cum_reported, na.rm = T), " reported cases\n",
                                          "(", max(round(incidence_reported, 2)), " per 100,000.)"),
                                   NA_character_))

# plot time series
df_daily %>% 
  select(resadmin1_correct, report_date, cum_reported) %>% 
  ggplot() +
  geom_line(aes(x = report_date, y = cum_reported, group = resadmin1_correct),
            colour = alpha("black", 0.6), size = 0.4) +
  geom_line(data = reported_comparisons,
            aes(x = report_date, y = cum_reported, colour = resadmin1_correct),
            size = 1) +
  geom_label_repel(data = reported_comparisons,
                   aes(x = report_date, y = cum_reported, 
                       colour = resadmin1_correct, label = label_max_cases),
                   na.rm = TRUE, size = 2.8, fill = alpha("white", 0.8),
                   fontface = "bold", min.segment.length = 0) +
  scale_color_manual(values= my_palette) +
  ggtitle(paste("Reported cases across regions in", my_country), 
          subtitle = "Top four regions highlighted") +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE), 
                          max(df_daily$report_date, na.rm = TRUE)))+
  labs(x="", y ="Reported cases", 
       caption = "Reported cases and incidence per 100,000 are indicated, the latter in parentheses") +
  theme(legend.position = "none")

ggsave(paste0(out_path, my_country, "_TimeSeries_CasesReported.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


#####################################################
##      Time Series of cumulative confirmed cases  ##
#####################################################

# select the top 4 regions with the higher count of cumulative confirmed cases
confirmed_to_label <- df_daily %>% 
  group_by(resadmin1_correct) %>% 
  top_n(1, wt = report_date) %>% 
  ungroup() %>% 
  filter(cum_confirmed > 0) %>% 
  # selecting only top four regions
  top_n(n = 4, wt = cum_confirmed) %>% 
  select(resadmin1_correct) %>% 
  pull()

# subset df according to needed labels
confirmed_comparisons <- df_daily %>% 
  filter(resadmin1_correct %in% confirmed_to_label) %>% 
  # paste resadmin1_correct and confirmed cases at maxdate. Needed for plot
  group_by(resadmin1_correct) %>% 
  mutate(label_max_cases = if_else(report_date == max(report_date, na.rm=T),
                                   paste0(as.character(resadmin1_correct), "\n",
                                          max(cum_confirmed, na.rm = T), " confirmed cases\n",
                                          "(", max(round(incidence_confirmed, 2)), " per 100,000.)"),
                                   NA_character_))

# plot time series
df_daily %>% 
  select(resadmin1_correct, report_date, cum_confirmed) %>% 
  ggplot() +
  geom_line(aes(x = report_date, y = cum_confirmed, group = resadmin1_correct),
            colour = alpha("black", 0.6), size = 0.4) +
  geom_line(data = confirmed_comparisons,
            aes(x = report_date, y = cum_confirmed, colour = resadmin1_correct),
            size = 1) +
  geom_label_repel(data = confirmed_comparisons,
                   aes(x = report_date, y = cum_confirmed, 
                       colour = resadmin1_correct, label = label_max_cases),
                   na.rm = TRUE, size = 2.8, fill = alpha("white", 0.8),
                   fontface = "bold", min.segment.length = 0) +
  scale_color_manual(values= my_palette) +
  ggtitle(paste("Confirmed cases across regions in", my_country), 
          subtitle = "Top four regions highlighted") +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE), 
                          max(df_daily$report_date, na.rm = TRUE)))+
  labs(x="", y ="Confirmed cases", 
       caption = "Confirmed cases and incidence per 100,000 are indicated, the latter in parentheses") +
  theme(legend.position = "none")

ggsave(paste0(out_path, my_country, "_TimeSeries_CasesConfirmed.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)



############################################
##      Time Series of cumulative deaths  ##
############################################

# select the top 4 regions with the higher count of cumulative deaths
deaths_to_label <- df_daily %>% 
  group_by(resadmin1_correct) %>% 
  top_n(1, wt = report_date) %>% 
  ungroup() %>% 
  filter(cum_deaths > 0) %>% 
  # selecting only top four regions
  top_n(n = 4, wt = cum_deaths) %>% 
  select(resadmin1_correct) %>% 
  pull()

# subset df according to needed labels
deaths_comparisons <- df_daily %>% 
  filter(resadmin1_correct %in% deaths_to_label) %>% 
  # paste resadmin1_correct and deaths at maxdate. Needed for plot
  group_by(resadmin1_correct) %>% 
  mutate(label_max_cases = if_else(report_date == max(report_date, na.rm=T),
                                   paste0(as.character(resadmin1_correct), "\n",
                                          max(cum_deaths, na.rm = T), " deaths\n",
                                          "(", max(round(mortality, 2)), " per 100,000.)"),
                                   NA_character_))

# plot time series
df_daily %>% 
  select(resadmin1_correct, report_date, cum_deaths) %>% 
  ggplot() +
  geom_line(aes(x = report_date, y = cum_deaths, group = resadmin1_correct),
            colour = alpha("black", 0.6), size = 0.4) +
  geom_line(data = deaths_comparisons,
            aes(x = report_date, y = cum_deaths, colour = resadmin1_correct),
            size = 1) +
  geom_label_repel(data = deaths_comparisons,
                   aes(x = report_date, y = cum_deaths, 
                       colour = resadmin1_correct, label = label_max_cases),
                   na.rm = TRUE, size = 2.8, fill = alpha("white", 0.8),
                   fontface = "bold", min.segment.length = 0) +
  scale_color_manual(values= my_palette) +
  ggtitle(paste("Deaths across regions in", my_country), 
          subtitle = "Top four regions highlighted") +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3,
               limits = c(min(df_daily$report_date, na.rm = TRUE), 
                          max(df_daily$report_date, na.rm = TRUE)))+
  labs(x="", y ="Deaths", 
       caption = "Deaths and mortality per 100,000 are indicated, the latter in parentheses") +
  theme(legend.position = "none")

ggsave(paste0(out_path, my_country, "_TimeSeries_Deaths.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


######################################################
######################################################
##                                                  ##
##      5 NATIONAL WEEKLY GROWTH RATE OVER TIME     ##
##                                                  ##
##              GROWTH RATES SECTION                ##
##                                                  ##
######################################################
######################################################

############################################
##      Growth rate of reported cases     ##
############################################

# growth rate of reported cases df
growth_rate_tab_reported <- df_daily_national %>% 
  mutate(reported_past_7 = rollsum(reported_this_day, 7, 
                                   fill = na.fill(reported_this_day, 0)),
         epiweek = MMWRweek(report_date)$MMWRweek) %>% 
  select(report_date, reported_past_7, epiweek) %>% 
  group_by(epiweek) %>% 
  # take the last day of each epiweek
  slice(which.max(report_date)) %>% 
  ungroup() %>% 
  # Cases in the past week vs cases two weeks ago
  mutate(diff_cases = reported_past_7 - lag(reported_past_7, 1), 
         week_growth = diff_cases / lag(reported_past_7, 1),
         week_growth_perc = 100 * week_growth, 
         # formula to convert weeklygrowth to daily growth equivalent
         growth = (((week_growth + 1) ^ (1/7)) - 1), 
         growth_perc = growth * 100)

# growth rate of reported cases plot
ggplot(growth_rate_tab_reported) +
  # line tracking the past 14 day changes.
  geom_line(aes(x = report_date, y = week_growth_perc), color = alpha("deepskyblue", 0.5), 
            size = 1) +
  geom_point(aes(x = report_date, y = week_growth_perc), color = alpha("deepskyblue", 1), 
             size = 1.5) +
  coord_cartesian(ylim = c(-100, 500)) +
  # shade area above line red
  geom_area(aes(y = Inf, x = report_date), 
            fill = alpha("#f79f57", 0.3)) +
  # share area below line green
  geom_area(aes(y = - Inf, x = report_date), 
            fill = alpha("#56bfa3", 0.3)) +
  ggtitle(paste0("Week-on-week growth rate of new COVID-19 reported cases in ", my_country), 
          subtitle = "Growth rate is the % change in new cases in the past week relative to the cases in the previous week") +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3)+
  labs(y = "Average daily growth rate (%) each week", x = "") +
  labs(caption = "Falls in growth rate at the end of the epicurve should be interpreted with caution. \nThis drop is often caused by case reporting lag.")

ggsave(paste0(out_path, my_country, "_GrowthRate_CasesReported.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# growth rate of reported cases: national table
growth_rate_tab_reported_prep  <- growth_rate_tab_reported %>% 
  select(epiweek, report_date, reported_past_7, week_growth_perc) %>%
  # replace either with 0, or with the percent formatted value
  mutate(week_growth_perc = ifelse(!is.na(week_growth_perc), week_growth_perc, 0)) %>% 
  mutate(week_growth_perc = round(week_growth_perc, 1)) %>% 
  mutate(report_date = format.Date(report_date, "%b %d")) %>% 
  rename(Epiweek = epiweek, 
         `Last day of week` = report_date, 
         `Reported cases in past week` = reported_past_7, 
         `% Change from prior week` = week_growth_perc)

growth_rate_formattab_reported <- formattable(growth_rate_tab_reported_prep, 
                                              list(`Reported cases in past week` = color_bar2("orange"), 
                                                   `% Change from prior week` = growth_formatter))

# export table
export_formattable(growth_rate_formattab_reported, paste0(out_path, my_country, "_Tab_GrowthRate_CasesReported.png"), 
                   width = 1000, height = 1000)


#############################################
##      Growth rate of confirmed cases     ##
#############################################

# growth rate of confirmed cases df
growth_rate_tab_confirmed <- df_daily_national %>% 
  mutate(confirmed_past_7 = rollsum(confirmed_this_day, 7, 
                                    fill = na.fill(confirmed_this_day, 0)),
         epiweek = MMWRweek(report_date)$MMWRweek) %>% 
  select(report_date, confirmed_past_7, epiweek) %>% 
  group_by(epiweek) %>% 
  # take the last day of each epiweek
  slice(which.max(report_date)) %>% 
  ungroup() %>% 
  # Cases in the past week vs cases two weeks ago
  mutate(diff_cases = confirmed_past_7 - lag(confirmed_past_7, 1), 
         week_growth = diff_cases / lag(confirmed_past_7, 1),
         week_growth_perc = 100 * week_growth, 
         # formula to convert weeklygrowth to daily growth equivalent
         growth = (((week_growth + 1) ^ (1/7)) - 1), 
         growth_perc = growth * 100)

# growth rate of confirmed cases plot
ggplot(growth_rate_tab_confirmed) +
  # line tracking the past 14 day changes.
  geom_line(aes(x = report_date, y = week_growth_perc), color = alpha("deepskyblue", 0.5), 
            size = 1) +
  geom_point(aes(x = report_date, y = week_growth_perc), color = alpha("deepskyblue", 1), 
             size = 1.5) +
  coord_cartesian(ylim = c(-100, 500)) +
  # shade area above line red
  geom_area(aes(y = Inf, x = report_date), 
            fill = alpha("#f79f57", 0.3)) +
  # share area below line green
  geom_area(aes(y = - Inf, x = report_date), 
            fill = alpha("#56bfa3", 0.3)) +
  ggtitle(paste0("Week-on-week growth rate of new COVID-19 confirmed cases in ", my_country), 
          subtitle = "Growth rate is the % change in new cases in the past week relative to the cases in the previous week") +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3)+
  labs(y = "Average daily growth rate (%) each week", x = "") +
  labs(caption = "Falls in growth rate at the end of the epicurve should be interpreted with caution. \nThis drop is often caused by case reporting lag.")

ggsave(paste0(out_path, my_country, "_GrowthRate_CasesConfirmed.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# growth rate of confirmed cases: national table
growth_rate_tab_confirmed_prep  <- growth_rate_tab_confirmed %>% 
  select(epiweek, report_date, confirmed_past_7, week_growth_perc) %>%
  # replace either with 0, or with the percent formatted value
  mutate(week_growth_perc = ifelse(!is.na(week_growth_perc), week_growth_perc, 0)) %>% 
  mutate(week_growth_perc = round(week_growth_perc, 1)) %>% 
  mutate(report_date = format.Date(report_date, "%b %d")) %>% 
  rename(Epiweek = epiweek, 
         `Last day of week` = report_date, 
         `Confirmed cases in past week` = confirmed_past_7, 
         `% Change from prior week` = week_growth_perc)

growth_rate_formattab_confirmed <- formattable(growth_rate_tab_confirmed_prep, 
                                               list(`Confirmed cases in past week` = color_bar2("orange"), 
                                                    `% Change from prior week` = growth_formatter))

# export table
export_formattable(growth_rate_formattab_confirmed, paste0(out_path, my_country, "_Tab_GrowthRate_CasesConfirmed.png"), 
                   width = 1000, height = 1000)


####################################
##      Growth rate of deaths     ##
####################################

# growth rate of deaths df
growth_rate_tab_deaths <- df_daily_national %>% 
  mutate(deaths_past_7 = rollsum(deaths_this_day, 7, 
                                 fill = na.fill(deaths_this_day, 0)),
         epiweek = MMWRweek(report_date)$MMWRweek) %>% 
  select(report_date, deaths_past_7, epiweek) %>% 
  group_by(epiweek) %>% 
  # take the last day of each epiweek
  slice(which.max(report_date)) %>% 
  ungroup() %>% 
  # Cases in the past week vs cases two weeks ago
  mutate(diff_cases = deaths_past_7 - lag(deaths_past_7, 1), 
         week_growth = diff_cases / lag(deaths_past_7, 1),
         week_growth_perc = 100 * week_growth, 
         # formula to convert weeklygrowth to daily growth equivalent
         growth = (((week_growth + 1) ^ (1/7)) - 1), 
         growth_perc = growth * 100)

# growth rate of deaths plot
ggplot(growth_rate_tab_deaths) +
  # line tracking the past 14 day changes.
  geom_line(aes(x = report_date, y = week_growth_perc), color = alpha("deepskyblue", 0.5), 
            size = 1) +
  geom_point(aes(x = report_date, y = week_growth_perc), color = alpha("deepskyblue", 1), 
             size = 1.5) +
  coord_cartesian(ylim = c(-100, 500)) +
  # shade area above line red
  geom_area(aes(y = Inf, x = report_date), 
            fill = alpha("#f79f57", 0.3)) +
  # share area below line green
  geom_area(aes(y = - Inf, x = report_date), 
            fill = alpha("#56bfa3", 0.3)) +
  ggtitle(paste0("Week-on-week growth rate of new COVID-19 deaths in ", my_country), 
          subtitle = "Growth rate is the % change in new cases in the past week relative to the cases in the previous week") +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3)+
  labs(y = "Average daily growth rate (%) each week", x = "") +
  labs(caption = "Falls in growth rate at the end of the epicurve should be interpreted with caution. \nThis drop is often caused by case reporting lag.")

ggsave(paste0(out_path, my_country, "_GrowthRate_Deaths.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# growth rate of deaths: national table
growth_rate_tab_deaths_prep  <- growth_rate_tab_deaths %>% 
  select(epiweek, report_date, deaths_past_7, week_growth_perc) %>%
  # replace either with 0, or with the percent formatted value
  mutate(week_growth_perc = ifelse(!is.na(week_growth_perc), week_growth_perc, 0)) %>% 
  mutate(week_growth_perc = round(week_growth_perc, 1)) %>% 
  mutate(report_date = format.Date(report_date, "%b %d")) %>% 
  rename(Epiweek = epiweek, 
         `Last day of week` = report_date, 
         `Deaths in past week` = deaths_past_7, 
         `% Change from prior week` = week_growth_perc)

growth_rate_formattab_deaths <- formattable(growth_rate_tab_deaths_prep, 
                                            list(`Deaths in past week` = color_bar2("orange"), 
                                                 `% Change from prior week` = growth_formatter))

# export table
export_formattable(growth_rate_formattab_deaths, paste0(out_path, my_country, "_Tab_GrowthRate_Deaths.png"), 
                   width = 1000, height = 1000)


##########################################################
##########################################################
##                                                      ##
##      6 DISTRIBUTION OF CASES BY ORIGIN OVER TIME     ##
##                                                      ##
##                TIME SERIES SECTION                   ##
##                                                      ##
##########################################################
##########################################################

###########################################################
##      Epicurve of reported cases by travel history     ##
###########################################################

# Expand dates per travel history category
all_dates_origin <- df %>%
  filter(!is.na(expo_travel)) %>%
  filter(is_reported == 1) %>%
  complete(report_date = seq.Date(min(report_date, na.rm = TRUE), 
                                  max(report_date, na.rm = TRUE), by = "day")) %>%
  tidyr::expand(report_date, expo_travel) %>%
  # logical variable needed to exclude from case count
  mutate(is_reported = 0)

# epicurve per travel history
df_origin_reported <- df %>%
  filter(!is.na(expo_travel)) %>%
  filter(is_reported == 1) %>%
  # add in new rows so that all dates are covered
  bind_rows(all_dates_origin) %>%
  # selecting only needed variables
  select(report_date, expo_travel, is_reported) %>%
  group_by(expo_travel, report_date) %>%
  summarise(reported_this_day = sum(is_reported, na.rm = T)) %>%
  group_by(report_date) %>%
  mutate(reported_prop = round((reported_this_day/ sum(reported_this_day) * 100), 1)) %>%
  # cases past week. To smooth out the curve
  # slightly less honest to the data. But yields better insights.
  group_by(expo_travel) %>%
  mutate(reported_trend = rollmean(x = reported_this_day, k = 7, align = "right",  
                                   fill = na.fill(reported_this_day, NA))) %>%
  group_by(report_date) %>%
  mutate(past_week_prop = round((reported_trend/ sum(reported_trend) * 100), 1))

# plot absolute reported cases vs travel history
df_origin_reported %>%
  filter(!is.na(expo_travel)) %>%
  ggplot() +
  geom_bar(aes(x = report_date, y = reported_this_day, fill = expo_travel), 
           stat = "identity", position = "stack", colour = alpha("white", .5), size = 0.1) +
  scale_fill_manual(values = c("#f19777", "#d94711"), labels = c("No recent travel", "Recent travel")) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3) +
  labs(x = "", y = "Absolute number of reported cases") +
  theme(legend.title = element_blank()) +
  ggtitle("Classification of new reported cases by travel history over time",
          "Absolute number of daily reported cases")   +
  theme(plot.caption = element_text(size = 8, color = "gray50", hjust = 1))

ggsave(paste0(out_path, my_country, "_TravelHistory_Abs_CasesReported.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# plot relative reported cases vs travel history
df_origin_reported %>%
  filter(!is.na(expo_travel)) %>%
  ggplot() +
  geom_bar(aes(x = report_date, y = past_week_prop, fill = expo_travel), 
           stat = "identity", position = "stack", colour = alpha("white", .5 ), size = 0.1) +
  scale_fill_manual(values = c("#f19777", "#d94711"), labels = c("No recent travel", "Recent travel")) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3) +
  labs(x = "", y = "% of reported cases", caption = "Note: Values are a smoothed rolling average of the past seven days")  +
  theme(legend.title = element_blank()) +
  labs(caption = paste0("*Cumulative total:\nRecent travel - ",
                        sum(df_origin_reported$reported_this_day[which(df_origin_reported$expo_travel == "Y")]),
                        " reported cases, ",
                        round(100 * sum(df_origin_reported$reported_this_day[which(df_origin_reported$expo_travel == "Y")])/
                                sum(df_origin_reported$reported_this_day), 1 ),
                        "% of total",
                        "\nNo recent travel - ",
                        sum(df_origin_reported$reported_this_day[which(df_origin_reported$expo_travel == "N")]),
                        " cases, ",
                        round(100 * sum(df_origin_reported$reported_this_day[which(df_origin_reported$expo_travel == "N")])/
                                sum(df_origin_reported$reported_this_day), 1 ),
                        "% of total"))  +
  theme( plot.caption = element_text(size = 8, color = "gray50", hjust = 1)) +
  ggtitle("Classification new COVID-19 reported cases over time (relative, smoothed)")

ggsave(paste0(out_path, my_country, "_TravelHistory_Rel_CasesReported.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# summary statistics for reported cases vs travel history 
pander(df_origin_reported %>% 
         ungroup() %>%
         select(report_date, expo_travel, reported_this_day) %>% 
         filter(!is.na(expo_travel)) %>%
         group_by(expo_travel) %>%
         summarise(`Reported cases (mean)` = mean(reported_this_day, na.rm = TRUE),
                   `Reported cases (median)` = median(reported_this_day, na.rm = TRUE),
                   `Reported cases (min)` = min(reported_this_day, na.rm = TRUE),
                   `Reported cases (max)` = max(reported_this_day, na.rm = TRUE),
                   `Reported cases (std. dev)` = sd(reported_this_day, na.rm = TRUE),
                   `Reported cases (total)` = sum(reported_this_day, na.rm = TRUE)))


############################################################
##      Epicurve of confirmed cases by travel history     ##
############################################################

# epicurve per travel history
df_origin_confirmed <- df %>%
  filter(!is.na(expo_travel)) %>%
  filter(report_classif == "CONFIRMED") %>%
  # add in new rows so that all dates are covered
  bind_rows(all_dates_origin) %>%
  # selecting only needed variables
  select(report_date, expo_travel, is_reported) %>%
  group_by(expo_travel, report_date) %>%
  summarise(confirmed_this_day = sum(is_reported, na.rm = T)) %>%
  group_by(report_date) %>%
  mutate(confirmed_prop = round((confirmed_this_day/ sum(confirmed_this_day) * 100), 1)) %>%
  # cases past week. To smooth out the curve
  # slightly less honest to the data. But yields better insights.
  group_by(expo_travel) %>%
  mutate(confirmed_trend = rollmean(x = confirmed_this_day, k = 7, align = "right",  
                                    fill = na.fill(confirmed_this_day, NA))) %>%
  group_by(report_date) %>%
  mutate(past_week_prop = round((confirmed_trend/ sum(confirmed_trend) * 100), 1))

# plot absolute confirmed cases vs travel history
df_origin_confirmed %>%
  filter(!is.na(expo_travel)) %>%
  ggplot() +
  geom_bar(aes(x = report_date, y = confirmed_this_day, fill = expo_travel), 
           stat = "identity", position = "stack", colour = alpha("white", .5), size = 0.1) +
  scale_fill_manual(values = c("#f19777", "#d94711"), labels = c("No recent travel", "Recent travel")) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3) +
  labs(x = "", y = "% of confirmed cases") +
  theme(legend.title = element_blank()) +
  ggtitle("Classification of new confirmed cases by travel history over time",
          "Absolute number of daily confirmed cases")   +
  theme(plot.caption = element_text(size = 8, color = "gray50", hjust = 1))

ggsave(paste0(out_path, my_country, "_TravelHistory_Abs_CasesConfirmed.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# plot relative confirmed cases vs travel history
df_origin_confirmed %>%
  filter(!is.na(expo_travel)) %>%
  ggplot() +
  geom_bar(aes(x = report_date, y = past_week_prop, fill = expo_travel), 
           stat = "identity", position = "stack", colour = alpha("white", .5 ), size = 0.1) +
  scale_fill_manual(values = c("#f19777", "#d94711"), labels = c("No recent travel", "Recent travel")) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3) +
  labs(x = "", y = "% of confirmed cases", caption = "Note: Values are a smoothed rolling average of the past seven days")  +
  theme(legend.title = element_blank()) +
  labs(caption = paste0("*Cumulative total:\nRecent travel - ",
                        sum(df_origin_confirmed$confirmed_this_day[which(df_origin_confirmed$expo_travel == "Y")]),
                        " confirmed cases, ",
                        round(100 * sum(df_origin_confirmed$confirmed_this_day[which(df_origin_confirmed$expo_travel == "Y")])/
                                sum(df_origin_confirmed$confirmed_this_day), 1 ),
                        "% of total",
                        "\nNo recent travel - ",
                        sum(df_origin_confirmed$confirmed_this_day[which(df_origin_confirmed$expo_travel == "N")]),
                        " cases, ",
                        round(100 * sum(df_origin_confirmed$confirmed_this_day[which(df_origin_confirmed$expo_travel == "N")])/
                                sum(df_origin_confirmed$confirmed_this_day), 1 ),
                        "% of total"))  +
  theme( plot.caption = element_text(size = 8, color = "gray50", hjust = 1)) +
  ggtitle("Classification new COVID-19 confirmed cases over time (relative, smoothed)")

ggsave(paste0(out_path, my_country, "_TravelHistory_Rel_CasesConfirmed.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# summary statistics for confirmed cases vs travel history 
pander(df_origin_confirmed %>% 
         ungroup() %>%
         select(report_date, expo_travel, confirmed_this_day) %>% 
         filter(!is.na(expo_travel)) %>%
         group_by(expo_travel) %>%
         summarise(`Confirmed cases (mean)` = mean(confirmed_this_day, na.rm = TRUE),
                   `Confirmed cases (median)` = median(confirmed_this_day, na.rm = TRUE),
                   `Confirmed cases (min)` = min(confirmed_this_day, na.rm = TRUE),
                   `Confirmed cases (max)` = max(confirmed_this_day, na.rm = TRUE),
                   `Confirmed cases (std. dev)` = sd(confirmed_this_day, na.rm = TRUE),
                   `Confirmed cases (total)` = sum(confirmed_this_day, na.rm = TRUE)))


###################################################
##      Epicurve of deaths by travel history     ##
###################################################

# epicurve per travel history
df_origin_deaths <- df %>%
  filter(!is.na(expo_travel)) %>%
  filter(patcourse_status == "DEAD") %>%
  # add in new rows so that all dates are covered
  bind_rows(all_dates_origin) %>%
  # selecting only needed variables
  select(report_date, expo_travel, is_reported) %>%
  group_by(expo_travel, report_date) %>%
  summarise(deaths_this_day = sum(is_reported, na.rm = T)) %>%
  group_by(report_date) %>%
  mutate(deaths_prop = round((deaths_this_day/ sum(deaths_this_day) * 100), 1)) %>%
  # cases past week. To smooth out the curve
  # slightly less honest to the data. But yields better insights.
  group_by(expo_travel) %>%
  mutate(deaths_trend = rollmean(x = deaths_this_day, k = 7, align = "right",  
                                 fill = na.fill(deaths_this_day, NA))) %>%
  group_by(report_date) %>%
  mutate(past_week_prop = round((deaths_trend/ sum(deaths_trend) * 100), 1))

# plot absolute deaths vs travel history
df_origin_deaths %>%
  filter(!is.na(expo_travel)) %>%
  ggplot() +
  geom_bar(aes(x = report_date, y = deaths_this_day, fill = expo_travel), 
           stat = "identity", position = "stack", colour = alpha("white", .5), size = 0.1) +
  scale_fill_manual(values = c("#f19777", "#d94711"), labels = c("No recent travel", "Recent travel")) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3) +
  labs(x = "", y = "% of deaths") +
  theme(legend.title = element_blank()) +
  ggtitle("Classification of new deaths by travel history over time",
          "Absolute number of daily deaths")   +
  theme(plot.caption = element_text(size = 8, color = "gray50", hjust = 1))

ggsave(paste0(out_path, my_country, "_TravelHistory_Abs_Deaths.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# plot relative deaths vs travel history
df_origin_deaths %>%
  filter(!is.na(expo_travel)) %>%
  ggplot() +
  geom_bar(aes(x = report_date, y = past_week_prop, fill = expo_travel), 
           stat = "identity", position = "stack", colour = alpha("white", .5 ), size = 0.1) +
  scale_fill_manual(values = c("#f19777", "#d94711"), labels = c("No recent travel", "Recent travel")) +
  scale_x_date(breaks = coveredsundays3,
               labels = epiweekanddate3) +
  labs(x = "", y = "% of deaths", caption = "Note: Values are a smoothed rolling average of the past seven days")  +
  theme(legend.title = element_blank()) +
  labs(caption = paste0("*Cumulative total:\nRecent travel - ",
                        sum(df_origin_deaths$deaths_this_day[which(df_origin_deaths$expo_travel == "Y")]),
                        " deaths, ",
                        round(100 * sum(df_origin_deaths$deaths_this_day[which(df_origin_deaths$expo_travel == "Y")])/
                                sum(df_origin_deaths$deaths_this_day), 1 ),
                        "% of total",
                        "\nNo recent travel - ",
                        sum(df_origin_deaths$deaths_this_day[which(df_origin_deaths$expo_travel == "N")]),
                        " cases, ",
                        round(100 * sum(df_origin_deaths$deaths_this_day[which(df_origin_deaths$expo_travel == "N")])/
                                sum(df_origin_deaths$deaths_this_day), 1 ),
                        "% of total"))  +
  theme( plot.caption = element_text(size = 8, color = "gray50", hjust = 1)) +
  ggtitle("Classification new COVID-19 deaths over time (relative, smoothed)")

ggsave(paste0(out_path, my_country, "_TravelHistory_Rel_Deaths.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# summary statistics for confirmed cases vs travel history 
pander(df_origin_deaths %>% 
         ungroup() %>%
         select(report_date, expo_travel, deaths_this_day) %>% 
         filter(!is.na(expo_travel)) %>%
         group_by(expo_travel) %>%
         summarise(`Deaths (mean)` = mean(deaths_this_day, na.rm = TRUE),
                   `Deaths (median)` = median(deaths_this_day, na.rm = TRUE),
                   `Deaths (min)` = min(deaths_this_day, na.rm = TRUE),
                   `Deaths (max)` = max(deaths_this_day, na.rm = TRUE),
                   `Deaths (std. dev)` = sd(deaths_this_day, na.rm = TRUE),
                   `Deaths (total)` = sum(deaths_this_day, na.rm = TRUE)))


###############################################
###############################################
##                                           ##
##      7 AGE-SEX DISTRIBUTION OF CASES      ##
##                                           ##
##            PYRAMID PLOTS SECTION          ##
##                                           ##
###############################################
###############################################

##########################################
##      Age-sex distribution graphs     ##
##########################################

df_age_sex <- df %>%
  # filtering out individuals with missing sex or age
  filter(!is.na(patinfo_sex) & !is.na(patinfo_ageonset_years)) %>%
  # creating age categories
  mutate(age_group = cut(as.numeric(patinfo_ageonset_years), breaks = c(0, 5, 9, 19, 29, 39, 49, 59, 69, 79, Inf),
                         labels = c("<5", "5-9", "10-19", "20-29", "30-39", "40-49",
                                    "50-59", "60-69", "70-79", ">80"), right = TRUE)) %>% 
  # for each age group and sex, sum the number of cases and the number of deaths
  group_by(age_group, patinfo_sex) %>%
  summarise(reported = sum(is_reported, na.rm = TRUE),
            confirmed = sum(is_reported[report_classif == "CONFIRMED"], na.rm = TRUE),
            deaths = sum(is_reported[patcourse_status == "DEAD"], na.rm = TRUE)) %>%
  filter(!is.na(age_group)) %>%
  ungroup()

# long format for the stacked bar chart.
# (Note that we will still use the regular wide format above for the text labels)
# for the stack, we need a long format
df_age_sex_long <-  df_age_sex %>% 
  # subtract out deaths from reported count to get CASES ALONE 
  # needed since we're going to build a STACKED bar chart.
  mutate(reported_alone = reported - deaths,
         confirmed_alone = confirmed - deaths) %>% 
  pivot_longer(names_to = "classification", cols = c(reported_alone, confirmed_alone, deaths)) %>% 
  mutate(classification = fct_relevel(classification, c("deaths", "reported_alone", "confirmed_alone")))


# age-sex pyramid plot of reported cases
ggplot() +
  # males
  geom_bar(data = subset(df_age_sex_long %>% filter(classification != "confirmed_alone", 
                                                    patinfo_sex == "M")),
           aes(x = age_group, y = value, fill = classification),
           stat = "identity", position = "stack", color = "white", width = 1,
           size = 0.1) +
  # add a scale We don't want a legend for cases only for deaths, so we pass only a single value to the breaks argument
  scale_fill_manual(breaks = c("deaths"), values = c("red", "steelblue3")) +
  # ggnewscale allows us to fill males and females with a slightly different color for cases
  ggnewscale::new_scale_fill() +
  # females
  geom_bar(data = subset(df_age_sex_long %>% filter(classification != "confirmed_alone", 
                                                    patinfo_sex == "F")),
           # negative values for the women (because it's on the left side of the plot)
           aes(x = age_group, y = -value, fill = classification),
           stat = "identity", position = "stack", color = "white", width = 1,
           size = 0) +
  scale_fill_manual(breaks = c("deaths"), values = c("red", "steelblue1")) +
  # CFR labels for males and females (bold font)
  geom_label(data = subset(df_age_sex %>% filter(patinfo_sex == "M") %>% select(-confirmed)), 
             aes(x = age_group, y = reported, label = paste0("CFR: ", round(100* deaths/reported, 1), "%")),
             size = 2.7,  fontface = "bold", colour = "dodgerblue4",
             hjust = 0, vjust = 0.2, label.size = 0, fill = "transparent") + 
  geom_label(data = subset(df_age_sex %>% filter(patinfo_sex == "F") %>% select(-confirmed)), 
             aes(x = age_group, y =-reported, label = paste0("CFR: ", round(100* deaths/reported, 1), "%")),
             size = 2.7,  fontface = "bold", colour = "dodgerblue3",
             hjust = 1, vjust = 0.2, label.size = 0, fill = "transparent") + 
  # Case and death count labels (plain font)
  geom_label(data = subset(df_age_sex %>% filter(patinfo_sex == "M") %>% select(-confirmed)), 
             aes(x = age_group, y = reported, label = paste0("(", deaths, " of ", reported, ")")),
             size = 2.5,  fontface = "plain", colour = alpha("dodgerblue4", 0.8),
             hjust = 0, vjust = 1, label.size = 0, fill = "transparent") + 
  geom_label(data = subset(df_age_sex %>% filter(patinfo_sex == "F") %>% select(-confirmed)), 
             aes(x = age_group, y = -reported, label = paste0("(", deaths, " of ", reported, ")")),
             size = 2.5,  fontface = "plain", colour = alpha("dodgerblue3", 0.8),
             hjust = 1, vjust = 1, label.size = 0, fill = "transparent") + 
  # Female and male labels, location of labels are on the upper corners for best visualization
  # and to prevent manual formatting of the plot
  annotate("text", x = Inf, y = -Inf, hjust = 0, vjust = 1,
           label = "Female", size = 4, fontface = "bold", colour = "dodgerblue3") +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = "Male", size = 4 , fontface = "bold", colour = "dodgerblue4" ) +
  labs(x = "Age group", y = "Reported cases and deaths", 
       caption = "Deaths are shown in red, but barely visible for most categories") +
  ggtitle("Age-sex distribution of all COVID-19 reported cases and deaths",
          subtitle = "Crude case-fatality rate, deaths and reported cases are indicated") +
  theme(legend.title = element_blank(),
        legend.position = "bottom") + 
  # expand the scale so that text labels are not cut off
  scale_y_continuous(limits = range(c(df_age_sex_long$value[df_age_sex_long$classification != "confirmed_alone"] * 1.2,
                                      -df_age_sex_long$value[df_age_sex_long$classification != "confirmed_alone"] * 1.2 ),
                                    na.rm=T), labels = abs) +
  coord_flip()

ggsave(paste0(out_path, my_country, "_Pyramid_CasesReported.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# age-sex pyramid plot of confirmed cases
ggplot() +
  # males
  geom_bar(data = subset(df_age_sex_long %>% filter(classification != "reported_alone"), 
                         patinfo_sex == "M"),
           aes(x = age_group, y = value, fill = classification),
           stat = "identity", position = "stack", color = "white", width = 1,
           size = 0.1) +
  # add a scale We don't want a legend for cases only for deaths, so we pass only a single value to the breaks argument
  scale_fill_manual(breaks = c("deaths"), values = c("red", "steelblue3")) +
  # ggnewscale allows us to fill males and females with a slightly different color for cases
  ggnewscale::new_scale_fill() +
  # females
  geom_bar(data = subset(df_age_sex_long %>% filter(classification != "reported_alone"), 
                         patinfo_sex == "F"),
           # negative values for the women (because it's on the left side of the plot)
           aes(x = age_group, y = -value, fill = classification),
           stat = "identity", position = "stack", color = "white", width = 1,
           size = 0) +
  scale_fill_manual(breaks = c("deaths"), values = c("red", "steelblue1")) +
  # CFR labels for males and females (bold font)
  geom_label(data = subset(df_age_sex %>% filter(patinfo_sex == "M") %>% select(-reported)), 
             aes(x = age_group, y = confirmed, label = paste0("CFR: ", round(100* deaths/confirmed, 1), "%")),
             size = 2.7,  fontface = "bold", colour = "dodgerblue4",
             hjust = 0, vjust = 0.2, label.size = 0, fill = "transparent") + 
  geom_label(data = subset(df_age_sex %>% filter(patinfo_sex == "F") %>% select(-reported)), 
             aes(x = age_group, y =-confirmed, label = paste0("CFR: ", round(100* deaths/confirmed, 1), "%")),
             size = 2.7,  fontface = "bold", colour = "dodgerblue3",
             hjust = 1, vjust = 0.2, label.size = 0, fill = "transparent") + 
  # Case and death count labels (plain font)
  geom_label(data = subset(df_age_sex %>% filter(patinfo_sex == "M") %>% select(-reported)), 
             aes(x = age_group, y = confirmed, label = paste0("(", deaths, " of ", confirmed, ")")),
             size = 2.5,  fontface = "plain", colour = alpha("dodgerblue4", 0.8),
             hjust = 0, vjust = 1, label.size = 0, fill = "transparent") + 
  geom_label(data = subset(df_age_sex %>% filter(patinfo_sex == "F") %>% select(-reported)), 
             aes(x = age_group, y = -confirmed, label = paste0("(", deaths, " of ", confirmed, ")")),
             size = 2.5,  fontface = "plain", colour = alpha("dodgerblue3", 0.8),
             hjust = 1, vjust = 1, label.size = 0, fill = "transparent") + 
  # Female and male labels, location of labels are on the upper corners for best visualization
  # and to prevent manual formatting of the plot
  annotate("text", x = Inf, y = -Inf, hjust = 0, vjust = 1,
           label = "Female", size = 4, fontface = "bold", colour = "dodgerblue3") +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = "Male", size = 4 , fontface = "bold", colour = "dodgerblue4" ) +
  labs(x = "Age group", y = "Confirmed cases and deaths", 
       caption = "Deaths are shown in red, but barely visible for most categories") +
  ggtitle("Age-sex distribution of all COVID-19 confirmed cases and deaths",
          subtitle = "Crude case-fatality rate, deaths and confirmed cases are indicated") +
  theme(legend.title = element_blank(),
        legend.position = "bottom") + 
  # expand the scale so that text labels are not cut off
  scale_y_continuous(limits = range(c(df_age_sex_long$value[df_age_sex_long$classification != "reported_alone"] * 1.2,
                                      -df_age_sex_long$value[df_age_sex_long$classification != "reported_alone"] * 1.2 ),
                                    na.rm=T), labels = abs) +
  coord_flip()

ggsave(paste0(out_path, my_country, "_Pyramid_CasesConfirmed.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


###############################
###############################
##                           ##
##      8 COMORBITIES        ##
##                           ##
##          SECTION          ##
##                           ##
###############################
###############################

###############################
##      Comorbities plot     ##
###############################

df_comorb <- df %>% 
  filter(patcourse_admit == "Y") %>% 
  group_by(Comcond_preexist) %>% 
  count() %>% 
  uncount(weights = n) %>% 
  # cleaning the diverse coding of the Comcond_preexist variable
  # standardizing into title case
  mutate(condition = str_to_title(Comcond_preexist)) %>% 
  # splitting comma separated observations into a vector
  # unnesting vectors that were separated above
  mutate(condition = strsplit(as.character(condition), ",")) %>% 
  unnest(condition) %>% 
  # splitting semicolon separated observations into a vector
  # unnesting vectors that were separated above
  mutate(condition = strsplit(as.character(condition), ";")) %>% 
  unnest(condition) %>% 
  # removing blank spaces (artifacts of splitting and unnesting)
  mutate(condition = trimws(condition)) %>% 
  group_by(condition) %>% 
  count() %>%
  # arranging by descending order
  arrange(-n) %>% 
  # filtering out 
  filter(condition != "Non" & !is.na(condition)) %>% 
  filter(n > 1)


df_comorb %>% 
  ggplot() +
  geom_bar(aes(x = reorder(condition, n), y = n), fill = "dodgerblue4", width = 0.8, stat = "identity") + 
  geom_text(aes(x = reorder(condition, n) , y = n, label = n) , hjust = -0.2, color = "dodgerblue4") + 
  scale_y_continuous(expand = expansion(c(mult = c(0, 0.2)))) +
  coord_flip() +
  labs(x = "Condition", y = "Count") +
  ggtitle(paste0("Most common comorbidities reported \nby hospitalised patients in ", my_country)) + 
  theme(axis.text.y = element_text(size = 9), 
        plot.title.position = "plot")

ggsave(paste0(out_path, my_country, "_Comorbidities.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


######################################
######################################
##                                  ##
##      9 TESTING TURNAROUND        ##
##                                  ##
##        BOX PLOT SECTION          ##
##                                  ##
######################################
######################################

#########################################################
##      Time from lab collection until lab result      ##
#########################################################

# time from test taken to result output
df$timetoresult <- as.numeric(df$Lab_resdate - df$Lab_datetaken)

# case cutoff for regions
count_cutoff <- 10

toofew <- df %>% 
  group_by(resadmin1_correct) %>% 
  summarise(non_na_count = sum(!is.na(timetoresult))) %>% 
  filter(non_na_count < count_cutoff) %>% 
  select(resadmin1_correct) %>% 
  pull() %>% 
  as.character()

# separate data frame to calculate median times per region
time_lab_df <- df %>% 
  group_by(resadmin1_correct) %>% 
  # filter out impossible numbers (more than 4 months)
  filter(timetoresult > 0 & timetoresult < 120) %>% 
  # bundle into one 
  mutate(resadmin1_correct = case_when(resadmin1_correct %in% toofew |
                                         resadmin1_correct == "No Info" ~ "All others",
                                       TRUE ~ as.character(resadmin1_correct))) %>% 
  mutate(resadmin1_correct = as.factor(resadmin1_correct)) %>% 
  mutate(resadmin1_correct = fct_relevel(resadmin1_correct, "All others", after = Inf)) %>% 
  summarise(mean = mean(timetoresult, na.rm = T), 
            sd = sd(timetoresult, na.rm = T), 
            median = median(timetoresult, na.rm = T), 
            minimum = min(timetoresult, na.rm = T), 
            maximum = max(timetoresult, na.rm = T), 
            howmany = sum(is_reported, na.rm = T)) %>% 
  filter(!is.na(resadmin1_correct)) %>%
  ungroup()

# boxplot of time from test taken to result output
df %>% 
  left_join(time_lab_df) %>% 
  # filter out impossible numbers
  filter(!is.na(timetoresult)) %>%
  # remove a few negative values (mistakes)
  filter(timetoresult > 0 & timetoresult < 100) %>% 
  # bundle into one 
  mutate(resadmin1_correct = case_when(resadmin1_correct %in% toofew |
                                         resadmin1_correct == "No Info" ~ "All others",
                                       TRUE ~ as.character(resadmin1_correct))) %>%
  mutate(resadmin1_correct = as.factor(resadmin1_correct)) %>%
  mutate(resadmin1_correct = fct_relevel(resadmin1_correct, "All others", after = Inf)) %>%
  ggplot() +
  # geom point slightly jittered
  geom_point(aes(x = resadmin1_correct, y = timetoresult, color = resadmin1_correct),
             position = position_jitter(width = 0.1, height = 0.1), size = 0.3, alpha = 0.15) + 
  # box plot beside the points
  geom_boxplot(aes(x = resadmin1_correct, y = timetoresult, 
                   color = resadmin1_correct, fill = resadmin1_correct),
               width = .18, outlier.shape = NA, alpha = 0.5, position = position_nudge(x = 0.2)) +
  # geom text pastes median value
  geom_text(data = time_lab_df, aes(x = resadmin1_correct, color = resadmin1_correct,
                                    y = median, label = paste0(median,
                                                               "\nday(s)", "\n", 
                                                               "n = ", howmany)),
            size = 2.4, position = position_nudge(x = 0.32), hjust = 0, lineheight = 1) +
  scale_fill_manual(values = my_palette) +
  scale_color_manual(values = my_palette) +
  ggtitle(paste("Time between collection and result \n for lab tests in each region of ", 
                my_country),
          subtitle = "Individual data points, boxplots, and median values are indicated") +
  theme(legend.position = "none") +
  labs(x = "Region", y = "Days between lab collection and lab result") +
  coord_cartesian(ylim = c(0, 5)) +
  # expand scale slightly so the last geom_text isn't cut off
  scale_x_discrete(expand = c(0, 1.05)) + 
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1))

ggsave(paste0(out_path, my_country, "_Time_CollResult.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)

# table with descriptive statistics
tab_time_lab <- time_lab_df %>%
  filter(!is.na(resadmin1_correct)) %>%
  mutate(mean = round(mean, 2),
         sd = round(sd, 2)) %>%
  rename(Region = resadmin1_correct,
         `Mean n of days` = mean,
         `Standard Deviation` = sd,
         `Median n of days` = median,
         `Min. n of days` = minimum,
         `Max. n of days` = maximum,
         N = howmany) %>%
  formattable()

export_formattable(tab_time_lab, paste0(out_path, my_country, "_Tab_Time_CollResult.png"),
                   width = 1500, height = 1000)

# summary statistics for testing turn-around
pander(df %>% 
         left_join(time_lab_df) %>% 
         group_by(resadmin1_correct) %>% 
         # filter out impossible numbers (more than 4 months)
         filter(timetoresult > 0 & timetoresult < 120) %>% 
         # bundle into one 
         mutate(resadmin1_correct = case_when(is.na(resadmin1_correct) ~ "No Info",
                                              TRUE ~ resadmin1_correct),
                resadmin1_correct = as.factor(resadmin1_correct)) %>% 
         summarise(`Days between collection and result (mean)` = mean(timetoresult, na.rm = T), 
                   `Days between collection and result (median)` = median(timetoresult, na.rm = T), 
                   `Days between collection and result (min)` = min(timetoresult, na.rm = T), 
                   `Days between collection and result (max)` = max(timetoresult, na.rm = T), 
                   `Days between collection and result (std. dev)` = sd(timetoresult, na.rm = T)))


#######################################################
#######################################################
##                                                   ##
##      10 SYMPTOM ONSET TO REPORT DATE DELAY        ##
##                                                   ##
##                BOX PLOT SECTION                   ##
##                                                   ##
#######################################################
#######################################################

#####################################################
##      Time from symptom onset until report      ##
#####################################################

# time from test taken to result output
df$timetoconsult <- as.numeric(df$report_date - df$patcourse_dateonset)

# case cutoff for regions
count_cutoff <- 5

toofew <- df %>% 
  group_by(resadmin1_correct) %>% 
  # filter out impossible numbers
  filter(timetoconsult > 0 & timetoconsult < 100) %>% 
  summarise(non_na_count = sum(!is.na(timetoconsult))) %>% 
  filter(non_na_count < count_cutoff) %>% 
  select(resadmin1_correct) %>% 
  pull() %>% 
  as.character()

# separate data frame to calculate median times per region
time_consult_df <- df %>% 
  # filter out impossible numbers
  filter(timetoconsult > 0 & timetoconsult < 100) %>% 
  group_by(resadmin1_correct) %>% 
  mutate(resadmin1_correct = case_when(resadmin1_correct %in% toofew |
                                         resadmin1_correct == "No Info" ~ "All others",
                                       TRUE ~ as.character(resadmin1_correct))) %>% 
  mutate(resadmin1_correct = as.factor(resadmin1_correct)) %>% 
  mutate(resadmin1_correct = fct_relevel(resadmin1_correct, "All others", after = Inf)) %>% 
  summarise(mean = mean(timetoconsult, na.rm = T), 
            sd = sd(timetoconsult, na.rm = T), 
            median = median(timetoconsult, na.rm = T), 
            minimum = min(timetoconsult, na.rm = T), 
            maximum = max(timetoconsult, na.rm = T), 
            howmany = sum(is_reported, na.rm = T)) %>% 
  filter(!is.na(resadmin1_correct)) %>%
  ungroup()

# boxplots of time from test taken to result output
df %>% 
  left_join(time_consult_df) %>% 
  filter(!is.na(timetoconsult)) %>%
  # remove a few negative values (mistakes)
  filter(timetoconsult > 0 & timetoconsult < 100) %>% 
  mutate(resadmin1_correct = case_when(resadmin1_correct %in% toofew |
                                         resadmin1_correct == "No Info" ~ "All others",
                                       TRUE ~ as.character(resadmin1_correct))) %>% 
  mutate(resadmin1_correct = as.factor(resadmin1_correct)) %>% 
  mutate(resadmin1_correct = fct_relevel(resadmin1_correct, "All others", after = Inf)) %>% 
  ggplot(aes(x = resadmin1_correct, y = timetoconsult, 
             fill = resadmin1_correct, color = resadmin1_correct)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), size = 0.3, alpha = 0.3) +
  geom_boxplot(width = .18, outlier.shape = NA, alpha = 0.5, position = position_nudge(x = 0.2)) +
  geom_text(data = time_consult_df, aes(x = resadmin1_correct, color = resadmin1_correct,
                                        y = median,
                                        label = paste0(median, "\nday(s)", "\n", 
                                                       "n=", howmany)), 
            size = 2.4, position = position_nudge(x = 0.32),
            hjust = 0, lineheight = 1) +
  scale_fill_manual(values = my_palette) +
  scale_color_manual(values = my_palette) +
  ggtitle(paste("Time between symptom onset and reporting date \nin each region of", my_country),
          subtitle= "(Box plots, individual data points and median values are indicated)") +
  theme(legend.position = "none") +
  labs(x = "Region", y = "Days between symptom onset and report date") +
  coord_cartesian(ylim = c(0, 30)) +
  # expand scale slightly so the last geom_text isn't cut off
  scale_x_discrete(expand = c(0, 1.05))+ 
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1))

ggsave(paste0(out_path, my_country, "_Time_OnsetReport.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


# table with descriptive statistics
tab_time_consult <- time_consult_df %>%
  filter(!is.na(resadmin1_correct)) %>%
  mutate(mean = round(mean, 2),
         sd = round(sd, 2)) %>%
  rename(Region = resadmin1_correct,
         `Mean n of days` = mean,
         `Standard Deviation` = sd,
         `Median n of days` = median,
         `Min. n of days` = minimum,
         `Max. n of days` = maximum,
         N = howmany) %>%
  formattable()

export_formattable(tab_time_consult, paste0(out_path, my_country, "_Tab_Time_OnsetReport.png"),
                   width = 1500, height = 1000)

# summary statistics for sympton onset report delay
pander(df %>% 
         left_join(time_consult_df) %>% 
         group_by(resadmin1_correct) %>% 
         # remove a few negative values (mistakes)
         filter(timetoconsult > 0 & timetoconsult < 100) %>% 
         # bundle into one 
         mutate(resadmin1_correct = case_when(is.na(resadmin1_correct) ~ "No Info",
                                              TRUE ~ resadmin1_correct),
                resadmin1_correct = as.factor(resadmin1_correct)) %>% 
         summarise(`Days between collection and result (mean)` = mean(timetoconsult, na.rm = T), 
                   `Days between collection and result (median)` = median(timetoconsult, na.rm = T), 
                   `Days between collection and result (min)` = min(timetoconsult, na.rm = T), 
                   `Days between collection and result (max)` = max(timetoconsult, na.rm = T), 
                   `Days between collection and result (std. dev)` = sd(timetoconsult, na.rm = T)))


################################
################################
##                            ##
##      11 TRAVEL HISTORY     ##
##                            ##
##      BAR PLOT SECTION      ##
##                            ##
################################
################################

########################################################
##      Recent travel history of confirmed cases      ##
########################################################

# Variable is not standardized, so final results might not be as good as we would expect
# We tried to standardize country names so that we at least make figure somehow better

travel_cutoff <- 2

toofew <- df %>% 
  # standardizing country names to title case (tends to reduce total number of countries)
  mutate(expo_travel_country = str_to_title(expo_travel_country)) %>%
  group_by(expo_travel_country) %>% 
  summarise(count = sum(is_reported[report_classif == "CONFIRMED"])) %>% 
  filter(count < travel_cutoff) %>% 
  select(expo_travel_country) %>% 
  pull() %>% 
  as.character()

df %>% 
  filter(!is.na(expo_travel_country)) %>% 
  # standardizing country names to upper case (reduces total number of countries)
  mutate(expo_travel_country = str_to_title(expo_travel_country)) %>%
  group_by(expo_travel_country) %>% 
  summarise(count = sum(is_reported[report_classif == "CONFIRMED"])) %>% 
  mutate(expo_travel_country = case_when(expo_travel_country %in% toofew |
                                           expo_travel_country == "Unknown" ~ "All others",
                                         TRUE~ as.character(expo_travel_country)))%>% 
  mutate(expo_travel_country = as.factor(expo_travel_country)) %>% 
  filter(expo_travel_country != "All others") %>% 
  # recount sum for other category
  filter(!is.na(expo_travel_country)) %>% 
  group_by(expo_travel_country) %>% 
  summarise(count = sum(count)) %>% 
  mutate(expo_travel_country = fct_reorder(expo_travel_country, count)) %>% 
  mutate(expo_travel_country = fct_relevel(expo_travel_country, "All others", after = Inf)) %>% 
  filter(count > 0) %>%
  ggplot() +
  geom_bar(aes(x = expo_travel_country, y = count), stat = "identity", fill = "#56B4E9",
           width = 0.95) +
  geom_text(aes(x = expo_travel_country, y = count, label = paste(" ", count)), hjust = 0,
            size = 3.4,  colour = "#3758a6") +
  labs(x = "Country visited", y = "Count of positive cases") +
  theme(legend.position = "none") +
  ggtitle(paste0("Countries most commonly visited by recent returnees to ", 
                 my_country, 
                 " \nwho tested positive for SARS-CoV-2")) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 9, colour = "black")) +
  labs(caption = paste0("Overall, ", 
                        nrow(df[which(df$expo_travel=="Y"), ]), 
                        " individuals who tested positive had recent international travel history.",
                        "\nThis is ",
                        round(
                          100 * nrow(df[which(df$expo_travel=="Y"), ]) / 
                            nrow(df[which(df$expo_travel=="Y" | df$expo_travel=="N"), ]),
                          0),
                        "% of the total number that responded to this question."))

ggsave(paste0(out_path, my_country, "_TravelHistory.png"), 
       plot = last_plot(), 
       width = 35, height = 20, units = "cm", dpi = 600)


###############################################################################
###############################################################################
##                                                                           ##
##      12 REPORTED CASE DISAGGREGATION FOR EACH REGION OF NIGER, DETAIL     ##
##                                                                           ##
##                      CONTINENTAL OVERVIEW                                 ##
##                                                                           ##
##                  TIME SERIES AND MAPS SECTION                             ##
##                                                                           ##
###############################################################################
###############################################################################


############################################################################################
############################################################################################
##                                                                                        ##
## This section produces a detailed analysis for all regions of the country using a loop  ##
##                                                                                        ##
## All analyses in this section uses !!!!!!REPORTED CASES!!!!!!                           ##
##                                                                                        ##
############################################################################################
############################################################################################


############################################################################
##      Details of several epidemiological variables for each Region      ##
############################################################################

# set case cutoff. Below this number of cases, region will be not be outputted
case_cutoff <- 10

# define cutoff for regions
# set to 1 to capture regions with any cases
regions_to_show <- df_daily %>% 
  group_by(resadmin1_correct) %>% 
  filter(max(na.omit(cum_reported)) > case_cutoff) %>% 
  select(resadmin1_correct) %>% 
  unique() %>% 
  pull() %>% 
  as.character()


# This is a time consuming loog to produce all tables and plots for each region of my_country

for (my_region in regions_to_show) {
  
  ######################################
  ##      Subseting df per region     ##
  ######################################
  
  df_epi_region <- df_daily %>%
    filter(resadmin1_correct == my_region)
  
  df_region <- df %>% 
    filter(resadmin1_correct == my_region)
  
  # calculating key statistics per region
  count_of_reported_cases_region <- max(df_epi_region$cum_reported, na.rm = TRUE)
  count_of_confirmed_cases_region <- max(df_epi_region$cum_confirmed, na.rm = TRUE)
  count_of_deaths_region <- max(df_epi_region$cum_deaths, na.rm = TRUE)
  current_crude_CFR_reg_region <-  round(100 * max(df_epi_region$cum_deaths, na.rm = TRUE) / 
                                           max(df_epi_region$cum_reported, na.rm = T), 2)
  
  # pasting section title
  cat(" \n# ", my_region, ": ", 
      count_of_reported_cases_region, " reported cases, ", 
      count_of_confirmed_cases_region, " confirmed cases, ", 
      count_of_deaths_region, " death(s)", sep = "")
  
  
  #######################
  ##     epiweeks      ##
  #######################
  
  # extract epiweeks to name for the region in question
  ew_to_label <- df %>% 
    filter(is_reported == 1 & resadmin1_correct == my_region) %>%
    tabyl(epiweek) %>% 
    select(epiweek) %>%
    # complete missing weeks in the range
    complete(epiweek = full_seq(epiweek, 1)) %>%  
    pull() %>% 
    as.numeric()
  
  # label the last day of each week (a saturday)
  howmany <- length(ew_to_label)
  sunday_start<- format.Date(MMWRweek2Date(rep(2020, howmany), ew_to_label, 1), "%m.%d")
  saturday_end <- format.Date(MMWRweek2Date(rep(2020, howmany), ew_to_label, 7), "%m.%d")
  
  # paste saturday and sunday as range
  new_week_names <- paste0("Week ", ew_to_label, "\n", sunday_start, "\n to ", saturday_end)
  
  
  ############################
  ##      Treemap plot      ##
  ############################
  
  gender_section <- df %>%
    select(patinfo_sex, is_reported, resadmin1_correct) %>%
    filter(is_reported == 1 & resadmin1_correct == my_region) %>%
    # convert to character, slightly easier to work with
    mutate(patinfo_sex = as.character(patinfo_sex)) %>%
    # NA to No info
    mutate(patinfo_sex = ifelse(is.na(patinfo_sex), "No info", patinfo_sex)) %>%
    # add in a row for each factor level in case they do not exist. 
    # Needed so as not to break plotting code
    add_row(patinfo_sex = "No info", is_reported = 0) %>%
    add_row(patinfo_sex = "M", is_reported = 0) %>%
    add_row(patinfo_sex = "F", is_reported = 0) %>%
    # summarise
    group_by(patinfo_sex, .drop = FALSE) %>%
    summarise(howmany = sum(is_reported), .groups = "drop") %>%
    # convert to factor and create custom factor order
    mutate(patinfo_sex = fct_relevel(patinfo_sex, "M", "F", "No info")) %>%
    # arrange in that order as well
    arrange(patinfo_sex, levels = c("M", "F", "No info")) %>%
    # columns to allow merging with other sections
    rename(value = patinfo_sex) %>%
    mutate(classif = "Sex") %>% 
    select(classif, everything()) %>% 
    ungroup()
  
  
  occup_section <- df %>%
    select(patinfo_occus, is_reported, resadmin1_correct) %>%
    filter(is_reported == 1 & resadmin1_correct == my_region) %>%
    # convert to character, slightly easier to work with
    mutate(patinfo_occus = as.character(patinfo_occus)) %>%
    # NA to No info
    mutate(patinfo_occus = ifelse(is.na(patinfo_occus), "No info", patinfo_occus)) %>%
    # add in a row for each factor level in case they do not exist. 
    # Needed so as not to break plotting code
    add_row(patinfo_occus = "No info", is_reported = 0) %>%
    add_row(patinfo_occus = "Y", is_reported = 0) %>%
    add_row(patinfo_occus = "N", is_reported = 0) %>%
    # summarise
    group_by(patinfo_occus, .drop = FALSE) %>%
    summarise(howmany = sum(is_reported), .groups = "drop") %>%
    mutate(patinfo_occus = case_when(patinfo_occus == "Y" ~ "Yes",
                                     patinfo_occus == "N" ~ "No",
                                     TRUE ~ as.character(patinfo_occus))) %>%
    # convert to factor and create custom factor order
    mutate(patinfo_occus = fct_relevel(patinfo_occus, "Yes", "No", "No info")) %>%
    # arrange in that order as well
    arrange(patinfo_occus, levels = c("Yes", "No", "No info")) %>%
    # columns to allow merging with other sections
    rename(value = patinfo_occus) %>%
    mutate(classif = "HCW Status") %>% 
    select(classif, everything()) %>% 
    ungroup()
  
  
  travel_section <- df %>%
    select(expo_travel, is_reported, resadmin1_correct) %>%
    filter(is_reported == 1 & resadmin1_correct == my_region) %>%
    # convert to character, slightly easier to work with
    mutate(expo_travel = as.character(expo_travel)) %>%
    # NA to No info
    mutate(expo_travel = ifelse(is.na(expo_travel), "No info", expo_travel)) %>%
    # add in a row for each factor level in case they do not exist. 
    # Needed so as not to break plotting code
    add_row(expo_travel = "No info", is_reported = 0) %>%
    add_row(expo_travel = "Y", is_reported = 0) %>%
    add_row(expo_travel = "N", is_reported = 0) %>%
    group_by(expo_travel , .drop = FALSE) %>%
    # summarise
    summarise(howmany = sum(is_reported), .groups = "drop") %>%
    mutate(expo_travel = case_when(expo_travel == "Y"~ "Yes",
                                   expo_travel == "N"~ "No",
                                   TRUE ~ as.character(expo_travel))) %>%
    # convert to factor and create custom factor order
    mutate(expo_travel = fct_relevel(expo_travel, "Yes" , "No", "No info")) %>%
    # arrange in that order as well
    arrange(expo_travel, levels = c("Yes", "No", "No info")) %>%
    # columns to allow merging with other sections
    rename(value = expo_travel) %>%
    mutate(classif = "Recent travel")%>% 
    select(classif, everything())%>% 
    ungroup()
  
  
  age_section <- df %>%
    select(patinfo_ageonset_years, is_reported, resadmin1_correct) %>%
    filter(is_reported == 1 & resadmin1_correct == my_region) %>%
    mutate(age_group = cut(patinfo_ageonset_years, breaks = c(0,20,40,60,120),
                           labels = c("< 20","20-39", "40-59", "> 60"))) %>%
    # convert to char. slightly wieldier
    mutate(age_group = as.character(age_group)) %>%
    mutate(age_group = ifelse(is.na(age_group), "No info", age_group)) %>%
    # add in a row for each factor level in case  they do not exist. Needed so as not to break plotting code
    add_row(age_group = "No info", is_reported = 0) %>%
    add_row(age_group = "< 20", is_reported = 0) %>%
    add_row(age_group = "20-39", is_reported = 0) %>%
    add_row(age_group = "40-59", is_reported = 0) %>%
    add_row(age_group = "> 60", is_reported = 0) %>%
    # summarise
    group_by(age_group , .drop = FALSE) %>%
    summarise(howmany = sum(is_reported), .groups = "drop") %>%
    # convert to factor and create custom factor order
    mutate(age_group = fct_relevel(age_group, "< 20","20-39", "40-59", "> 60", "No info")) %>%
    # arrange in that order as well
    arrange(age_group, levels = c("< 20","20-39", "40-59", "> 60", "No info")) %>%
    # columns to allow merging with other sections
    rename(value = age_group) %>%
    mutate(classif = "Age group") %>% 
    select(classif, everything())%>% 
    ungroup()
  
  # merge into single factor table
  factor_table_1 <- bind_rows(gender_section, occup_section, travel_section, age_section) %>%
    # NAs to 0
    mutate(howmany = ifelse(is.na(howmany), 0, howmany)) %>%
    group_by(classif) %>%
    # percent column for labels
    mutate(percent =round(100 * howmany/sum(howmany), 1)) %>% 
    ungroup()
  
  factorpalette  <- c("#408199", "#A3D6D6", "gray80", # teal shades for male/female
                      "#5bb57c", "#a3de90", "gray80", # greens for HCW status
                      "#f09a73","#f5c2ae", "gray80", # oranges for recent travel history
                      "#E1DEF1", "#ADA5D1", "#9389d9", "#7269b3", # purples for age categories
                      "gray80") 
  
  # combine factor table with colors then make modifications
  factor_table_2 <- bind_cols(factor_table_1, factorpalette = factorpalette) %>%
    mutate(classif = as.factor(classif),
           factorpalette = as.factor(factorpalette)) %>%
    ungroup()
  
  
  # plot
  treemap <- factor_table_2 %>%
    ggplot(aes(subgroup = classif)) +
    geom_treemap(aes(area = howmany), fill = factorpalette) +
    geom_treemap_subgroup_border(aes(area = howmany), colour = "white", size = 1) +
    geom_treemap_text(aes(area = howmany, label = value), fontface = "italic",
                      colour = alpha("white", 0.7), place = "centre",
                      grow = FALSE, reflow = TRUE) +
    #labels for count and proportion
    geom_treemap_text(aes(area = howmany, label = paste0("n = ", howmany, " (", percent, "%)")),
                      fontface = "bold", colour = alpha("black", 0.8), size = 7,
                      place = "bottom", padding.y = unit(1.2, "mm"), 
                      grow = FALSE, reflow = TRUE, min.size = 3) +
    geom_treemap_subgroup_text(aes(area = howmany), place = "top", 
                               grow = FALSE, alpha = 0.23, colour = "gray40") +
    ggtitle(paste("Breakdown of all COVID-19 reported cases in", my_region)) + 
    labs(caption = "Each color group equals 100% of cases.\nHCW = Health care worker. Recent travel = past 14 days") +
    theme(plot.title = element_text(size = 10))
  
  ggsave(paste0(out_path, my_country, "_Regional_Reported_", my_region, "_Treemap", ".png"), 
         plot = treemap, 
         width = 35, height = 20, units = "cm", dpi = 600)
  
  
  ####################################
  ##      Daily reported cases      ##
  ####################################
  
  p_reported_cases <- df_epi_region %>% 
    select(report_date, reported_this_day) %>% 
    mutate(reported_7day_avg = rollmean(x = reported_this_day, k = 7, align = "right",  
                                        fill = na.fill(reported_this_day, 0))) %>% 
    ggplot() +
    geom_bar(aes(x = report_date, y = reported_this_day), stat = "identity", 
             fill = "#1B9E77", colour = alpha("white", 0.05)) +
    geom_line(aes(x = report_date, y = reported_7day_avg, lty = "7-day \nrolling average")) +
    scale_x_date(breaks = coveredsundays3,
                 labels = epiweekanddate3,
                 limits = c(min(df_epi_region$report_date, na.rm = TRUE), 
                            max(df_epi_region$report_date, na.rm = TRUE)))+
    labs(x = "", y = "Reported cases") +
    ggtitle(paste0("Reported cases. Total: ",  max(df_epi_region$cum_reported, na.rm = TRUE))) +
    theme(legend.title = element_blank(),
          legend.position = c(0.3, 0.7),
          legend.background = element_rect(fill = alpha("white", 0.3)), 
          plot.title = element_text(size = 10), 
          axis.text = element_text(size = 6))
  
  ggsave(paste0(out_path, my_country, "_Regional_Reported_", my_region, "_Epicurve_CasesReported", ".png"), 
         plot = p_reported_cases, 
         width = 35, height = 20, units = "cm", dpi = 600)
  
  
  ############################
  ##      Daily deaths      ##
  ############################
  
  if (max(df_epi_region$cum_deaths) < 1){ 
    p_deaths <- text_grob(paste0("No recorded deaths in ", my_region))
  } else {
    p_deaths <- df_epi_region %>% 
      filter(resadmin1_correct == my_region) %>% 
      select(report_date, deaths_this_day) %>% 
      mutate(deaths_7day_avg = rollmean(x = deaths_this_day, k = 7, align = "right",  
                                        fill = na.fill(deaths_this_day, 0))) %>% 
      ggplot() +
      geom_bar(aes(x = report_date, y = deaths_this_day), stat = "identity", 
               fill = "#D95F02", colour = alpha("white", 0.05)) +
      geom_line(aes(x = report_date, y = deaths_7day_avg, lty = "7-day rolling average")) +
      scale_x_date(breaks = coveredsundays3,
                   labels = epiweekanddate3,
                   limits = c(min(df_epi_region$report_date, na.rm = TRUE),
                              max(df_epi_region$report_date, na.rm = TRUE)))+
      labs(x = "", y = "Deaths") +
      ggtitle(paste0("Deaths. Total: ", max(df_epi_region$cum_deaths, na.rm = TRUE))) +
      theme(legend.title = element_blank(),
            legend.position = "none" ,
            legend.background = element_rect(fill = alpha("white", 0.3)), 
            plot.title = element_text(size = 10), 
            axis.text = element_text(size = 6))
  }
  
  ggsave(paste0(out_path, my_country, "_Regional_Reported_", my_region, "_Epicurve_Deaths", ".png"), 
         plot = p_deaths, 
         width = 35, height = 20, units = "cm", dpi = 600)
  
  
  p_composite <- grid.arrange(treemap, p_reported_cases, p_deaths, 
                              layout_matrix = rbind(c(1,2), 
                                                    c(1,3)))
  
  ggsave(paste0(out_path, my_country, "_Regional_Reported_", my_region, "_CompositePlot", ".png"), 
         plot = p_composite, 
         width = 35, height = 20, units = "cm", dpi = 600)
  
  
  cat("  \n", 
      "Below are the cases reported in", my_region, 
      "each epiweek since the first case was reported.",
      "Each table is disaggregated by a different factor.",
      "  \n")
  
  cat("\\vspace{12pt}")
  
  
  #####################################
  ##      Reported cases by sex      ##
  #####################################
  
  # BUild epicurve per sex for only the specified region
  # Expand dates per sex category
  df_dates_per_sex <- df %>% 
    filter(is_reported == 1 & resadmin1_correct == my_region) %>% 
    complete(report_date = seq.Date(min(report_date), max(report_date), by = "day")) %>%
    tidyr::expand(report_date, patinfo_sex) %>% 
    # logical variable needed to exclude from case count
    mutate(is_reported = 0) %>% 
    mutate(patinfo_sex = ifelse(is.na(patinfo_sex), "No info", patinfo_sex))
  
  # epicurve per sex group
  df_sex <- df %>%  
    filter(is_reported == 1 & resadmin1_correct == my_region) %>% 
    # add in new rows so that all dates are covered
    bind_rows(df_dates_per_sex) %>%
    select(report_date, patinfo_sex, is_reported) %>%
    group_by(patinfo_sex, report_date) %>%
    summarise(reported_this_day = sum(is_reported, na.rm = TRUE)) %>% 
    group_by(patinfo_sex) %>% 
    # cases past week
    mutate(reported_past_week = rollsum(x = reported_this_day, k = 7, align = "right",
                                        fill = na.fill(reported_this_day, 0))) %>%
    ungroup() %>% 
    mutate(patinfo_sex = ifelse(is.na(patinfo_sex), "No info", patinfo_sex),
           epiweek = lubridate::epiweek(report_date)) %>% 
    # then take last day of each epiweek
    group_by(patinfo_sex, epiweek) %>% 
    slice(which.max(report_date)) %>% 
    # remove if empty  
    group_by(patinfo_sex) %>% 
    filter(max(reported_this_day) > 0) %>% 
    # take only the highest value if there is a clash 
    # (If there are any NAs there will be a clash. Please fix this code it is bad)
    group_by(patinfo_sex, epiweek) %>% 
    filter(reported_this_day == max(reported_this_day)) %>% 
    ungroup() %>% 
    # pivot
    pivot_wider(id_cols = patinfo_sex, names_from = epiweek, values_from = reported_this_day) %>% 
    ungroup() %>% 
    rename(`Sex  ` = patinfo_sex)
  
  # replace names
  names(df_sex)[-c(1)] <- new_week_names
  
  # create table
  df_sex <- formattable(df_sex, 
                        list(formattable::area(col = 2:ncol(df_sex)) ~ 
                               color_tile_small_text("white", "#408199"), 
                             # just make the text small
                             formattable::area(col = 1) ~ 
                               color_tile_small_text_bold("white", "white")))
  
  # reduce table header size
  names(df_sex) <- make_header_small(names(df_sex))
  
  # export
  fig_name <- "Fig_1_Sex"
  
  export_formattable(df_sex,
                     file = paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png"),
                     width = 1800, height = 1000)
  
  # paste path to image then read it in
  cat(paste0("![](", out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png)"), "\n")
  # cat(paste0("![](", "/Users/kenwosu/tempfig.png)"), "\n")
  
  
  ##############################################
  ##      Reported cases by age category      ##
  ##############################################
  
  # BUild epicurve per age for only the specified region
  # Expand dates per age category
  df_dates_age_category <- df %>% 
    select(report_date, patinfo_ageonset_years, is_reported, resadmin1_correct) %>%
    filter(is_reported == 1 & resadmin1_correct == my_region) %>%
    mutate(age_group = cut(patinfo_ageonset_years, breaks = c(0, 20, 40, 60, 120),
                           labels = c("< 20", "20-39", "40-59", "> 60"))) %>% 
    # not sure why needed
    mutate(age_group = as.character(age_group)) %>% 
    complete(report_date = seq.Date(min(report_date), max(report_date), by = "day")) %>%
    tidyr::expand(report_date, age_group) %>% 
    # logical variable needed to exclude from case count
    mutate(is_reported = 0) %>% 
    mutate(age_group = ifelse(is.na(age_group), "No info", age_group))
  
  # epicurve per age group
  df_age_category <- df %>% 
    filter(is_reported == 1)  %>% 
    filter(resadmin1_correct == my_region)  %>% 
    mutate(age_group = cut(patinfo_ageonset_years, breaks = c(0, 20, 40, 60, 120),
                           labels = c("< 20", "20-39", "40-59", "> 60"))) %>% 
    # not sure why needed
    mutate(age_group = as.character(age_group)) %>% 
    # add in new rows so that all dates are covered
    bind_rows(df_dates_age_category) %>%
    select(report_date, age_group, is_reported) %>%
    group_by(age_group, report_date) %>%
    summarise(reported_this_day = sum(is_reported, na.rm = TRUE)) %>% 
    group_by(age_group) %>% 
    # cases past week
    mutate(reported_this_day=rollsum(x = reported_this_day, k = 7, align = "right",  
                                     fill = na.fill(reported_this_day, 0))) %>%
    ungroup() %>% 
    mutate(age_group = ifelse(is.na(age_group), "No info", age_group)) %>% 
    mutate(epiweek = lubridate::epiweek(report_date)) %>% 
    # then take last day of each epiweek
    group_by(age_group, epiweek) %>% 
    slice(which.max(report_date)) %>% 
    ungroup() %>% 
    # arrange levels
    mutate(age_group = as.factor(age_group)) %>% 
    arrange(match (age_group, c("< 20", "20-39", "40-59", "> 60", "No info"))) %>% 
    # remove if empty
    group_by(age_group) %>% 
    filter(max(reported_this_day) > 0) %>% 
    # take only the highest value if there is a clash (If there are any NAs there will be a clash. Please fix this code it is bad)
    group_by(age_group, epiweek) %>% 
    filter(reported_this_day == max(reported_this_day)) %>% 
    ungroup() %>% 
    # pivot
    pivot_wider(id_cols = age_group, names_from = epiweek, values_from = reported_this_day) %>% 
    ungroup() %>% 
    rename(`Age  ` = age_group)
  
  # replace names
  names(df_age_category)[-c(1)] <- new_week_names
  
  # create table
  df_age_category <- formattable(df_age_category, 
                                 list(formattable::area(col = 2:ncol(df_age_category)) ~ 
                                        color_tile_small_text("white", "#7269b3"), 
                                      # just make the text small
                                      formattable::area(col = 1) ~ 
                                        color_tile_small_text_bold("white", "white")))
  
  # reduce table header size
  names(df_age_category) <- make_header_small(names(df_age_category))
  
  # export
  fig_name <- "Fig_2_AgeCategory"
  
  export_formattable(df_age_category,
                     file = paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png"),
                     width = 1800, height = 1000)
  
  # paste path to image then read it in
  cat(paste0("![](", out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png)"), "\n")
  #cat(paste0("![](", "/Users/kenwosu/tempfig.png)"), "\n")
  
  
  ############################################
  ##      Reported cases by HCW status      ##
  ############################################
  
  # BUild epicurve per hcw status for only the specified region
  # Expand dates per sex category
  df_dates_HCW <- df %>% 
    filter(is_reported == 1 & resadmin1_correct == my_region) %>% 
    complete(report_date = seq.Date(min(report_date), max(report_date), by = "day")) %>%
    tidyr::expand(report_date, patinfo_occus) %>% 
    # logical variable needed to exclude from case count
    mutate(is_reported = 0) %>% 
    mutate(patinfo_occus = ifelse(is.na(patinfo_occus), "No info", patinfo_occus))
  
  # epicurve per sex group
  df_HCW <- df %>%  
    filter(is_reported == 1) %>% 
    filter(resadmin1_correct == my_region) %>% 
    # add in new rows so that all dates are covered
    bind_rows(df_dates_HCW) %>%
    select(report_date, patinfo_occus, is_reported) %>%
    group_by(patinfo_occus, report_date) %>%
    summarise(reported_this_day = sum(is_reported, na.rm = T)) %>% 
    group_by(patinfo_occus) %>% 
    # cases past week
    mutate(reported_this_day = rollsum(x = reported_this_day, k = 7, align = "right",  
                                       fill = na.fill(reported_this_day, 0))) %>%
    ungroup() %>% 
    mutate(patinfo_occus = ifelse(is.na(patinfo_occus), "No info", patinfo_occus)) %>% 
    mutate(epiweek = lubridate::epiweek(report_date)) %>% 
    # then take last day of each epiweek
    group_by(patinfo_occus, epiweek) %>% 
    slice(which.max(report_date)) %>% 
    # remove if empty  
    group_by(patinfo_occus) %>% 
    filter(max(reported_this_day) > 0) %>% 
    ungroup() %>% 
    # take only the highest value if there is a clash (If there are any NAs there will be a clash. Please fix this code it is bad)
    group_by(patinfo_occus, epiweek) %>% 
    filter(reported_this_day == max(reported_this_day)) %>% 
    ungroup() %>% 
    # arrange levels
    mutate(patinfo_occus = as.factor(patinfo_occus)) %>% 
    arrange(match(patinfo_occus,  c("Y", "N", "No info"))) %>% 
    ungroup() %>% 
    # pivot
    pivot_wider(id_cols = patinfo_occus, names_from = epiweek, values_from = reported_this_day) %>% 
    rename(`HCW?  ` = patinfo_occus)
  
  # replace names
  names(df_HCW)[-c(1)] <- new_week_names
  
  # create table
  df_HCW <- formattable(df_HCW, 
                        list(formattable::area(col = 2:ncol(df_HCW)) ~ 
                               color_tile_small_text("white", "#5bb57c"), 
                             # just make the text small
                             formattable::area(col = 1) ~ 
                               color_tile_small_text_bold("white", "white")))
  
  # reduce table header size
  names(df_HCW) <- make_header_small(names(df_HCW))
  
  # export
  fig_name <- "Fig_3_HCW"
  
  export_formattable(df_HCW,
                     file = paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png"),
                     width = 1800, height = 1000)
  
  # paste path to image then read it in
  cat(paste0("![](", out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png)"), "\n")
  #cat(paste0("![](", "/Users/kenwosu/tempfig.png)"), "\n")
  
  
  ################################################
  ##      Reported cases by travel history      ##
  ################################################
  
  # Build epicurve per travel history for only the specified region
  # Expand dates per sex category
  df_dates_travel <- df %>% 
    filter(is_reported == 1 & resadmin1_correct == my_region) %>% 
    complete(report_date = seq.Date(min(report_date), max(report_date), by = "day")) %>%
    tidyr::expand (report_date, expo_travel) %>% 
    # logical variable needed to exclude from case count
    mutate(is_reported = 0) %>% 
    mutate(expo_travel = ifelse(is.na(expo_travel), "No info", expo_travel))
  
  # epicurve per sex group
  df_travel <- df %>%  
    filter(is_reported == 1)  %>% 
    filter(resadmin1_correct == my_region)  %>% 
    # add in new rows so that all dates are covered
    bind_rows(df_dates_travel) %>%
    select(report_date, expo_travel, is_reported) %>%
    group_by(expo_travel, report_date) %>%
    summarise(reported_this_day = sum(is_reported, na.rm = TRUE)) %>% 
    group_by(expo_travel) %>%
    # cases past week
    mutate(reported_this_day = rollsum(x = reported_this_day, k = 7, align = "right",  
                                       fill = na.fill(reported_this_day, 0))) %>%
    ungroup() %>% 
    mutate(expo_travel = ifelse(is.na(expo_travel), "No info", expo_travel)) %>% 
    mutate(epiweek = lubridate::epiweek(report_date)) %>% 
    # then take last day of each epiweek
    group_by(expo_travel, epiweek) %>% 
    slice(which.max(report_date)) %>% 
    ungroup() %>% 
    # remove if empty  
    group_by(expo_travel) %>% 
    filter(max(reported_this_day) > 0) %>% 
    ungroup() %>% 
    # take only the highest value if there is a clash 
    # (If there are any NAs there will be a clash. Please fix this code it is bad)
    group_by(expo_travel, epiweek) %>% 
    filter(reported_this_day == max(reported_this_day)) %>% 
    ungroup() %>% 
    # arrange levels
    mutate(expo_travel = as.factor(expo_travel)) %>% 
    arrange(match(expo_travel, c("Y", "N", "No info"))) %>% 
    # pivot
    pivot_wider(id_cols = expo_travel, names_from = epiweek, values_from = reported_this_day) %>% 
    rename(Travel = expo_travel) 
  
  # replace names
  names(df_travel)[-c(1)] <- new_week_names
  
  # create table
  df_travel <- formattable(df_travel, 
                           list(formattable::area(col = 2:ncol( df_travel)) ~ 
                                  color_tile_small_text("white", "#f09a73"), 
                                # just make the text small
                                formattable::area(col = 1) ~ 
                                  color_tile_small_text_bold("white", "white")))
  
  # reduce table header size
  names(df_travel) <- make_header_small(names(df_travel))
  
  # export
  fig_name <- "Fig_4_TravelHistory"
  
  export_formattable(df_travel,
                     file = paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png"),
                     width = 1800, height = 1000)
  
  # paste path to image then read it in
  cat(paste0("![](", out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png)"), "\n")
  #cat(paste0("![](", "/Users/kenwosu/tempfig.png)"), "\n")
  
  
  
  ##########################################
  ##      Reported cases by district      ##
  ##########################################
  
  # Build epicurve per district status for only the specified region
  # Expand dates per sex category
  df_dates_district <- df %>% 
    filter(is_reported == 1 & resadmin1_correct == my_region) %>% 
    complete(report_date = seq.Date(min(report_date), max(report_date), by = "day")) %>%
    tidyr::expand (resadmin2_correct, report_date) %>% 
    # logical variable needed to exclude from case count
    mutate(is_reported = 0) %>% 
    mutate(resadmin2_correct = ifelse(is.na(resadmin2_correct), "No info", resadmin2_correct))
  
  # epicurve per sex group
  df_district <- df %>%  
    filter(is_reported == 1) %>% 
    filter(resadmin1_correct == my_region) %>% 
    # add in new rows so that all dates are covered
    bind_rows(df_dates_district) %>%
    select(report_date, resadmin2_correct, is_reported) %>%
    group_by(resadmin2_correct, report_date) %>%
    summarise(reported_this_day = sum(is_reported, na.rm = TRUE)) %>% 
    group_by(resadmin2_correct) %>% 
    # cases past week
    mutate(reported_this_day = rollsum(x = reported_this_day, k = 7, align = "right",  
                                       fill = na.fill(reported_this_day, 0))) %>%
    ungroup() %>% 
    mutate(resadmin2_correct = ifelse(is.na(resadmin2_correct), "No info", resadmin2_correct)) %>% 
    mutate(epiweek = lubridate::epiweek(report_date)) %>% 
    # then take last day of each epiweek
    group_by(resadmin2_correct, epiweek) %>% 
    slice(which.max(report_date)) %>% 
    ungroup() %>% 
    # remove if empty  
    group_by(resadmin2_correct) %>% 
    filter(max(reported_this_day) > 0) %>% 
    ungroup() %>% 
    # take only the highest value if there is a clash 
    # (If there are any NAs there will be a clash. Please fix this code it is bad)
    group_by(resadmin2_correct, epiweek) %>% 
    filter(reported_this_day == max(reported_this_day)) %>% 
    ungroup() %>% 
    # pivot
    pivot_wider(id_cols = resadmin2_correct, names_from = epiweek, values_from = reported_this_day) %>% 
    rename(District = resadmin2_correct) 
  
  # replace names
  names(df_district)[-c(1)] <- new_week_names
  
  # create table
  df_district <- formattable(df_district, 
                             list(formattable::area(col = 2:ncol(df_district)) ~ 
                                    color_tile_small_text("white", "orange"), 
                                  # just make the text small
                                  formattable::area(col = 1) ~ 
                                    color_tile_small_text_bold("white", "white")))
  
  # reduce table header size
  names(df_district) <- make_header_small(names(df_district))
  
  # export
  fig_name <- "Fig_5_District"
  
  export_formattable(df_district,
                     file = paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png"),
                     width = 1800, height = 1000)
  
  # paste path to image then read it in
  cat(paste0("![](", out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png)"), "\n")
  #cat(paste0("![](", "/Users/kenwosu/tempfig.png)"), "\n")
  
  cat("\\newpage")
  
  
  ##################################################
  ##      Age-sex pyramnid of reported cases      ##
  ##################################################
  
  cat("  \n", "  \n##", "Cases and deaths by age-sex group")
  
  cat("\\vspace{12pt}")
  
  df_age_sex <- df %>%
    filter(resadmin1_correct == my_region ) %>% 
    mutate(age_group = cut(patinfo_ageonset_years, 
                           breaks = c(0, 5, 9, 19, 29, 39, 49, 59, 69, 79, 120),
                           labels = c("<5", "5-9", "10-19", "20-29", "30-39", "40-49",
                                      "50-59", "60-69", "70-79", ">80"), right = TRUE)) 
  
  # expand age group per age sex. 
  # Needed to avoid some weird ggplot problems. Related to ggnewscale
  df_age_sex_expand <- df_age_sex %>% 
    expand(age_group, patinfo_sex) %>% 
    mutate(is_reported = 0)
  
  
  df_age_sex <- df_age_sex %>% 
    bind_rows(df_age_sex_expand) %>% 
    # count number of cases  
    group_by(age_group, patinfo_sex, .drop = FALSE) %>%
    summarise(reported = sum(is_reported, na.rm = TRUE),
              deaths = sum(is_reported[patcourse_status == "DEAD"], na.rm = TRUE)) %>%
    ungroup() %>% 
    mutate(patinfo_sex = as.factor(patinfo_sex)) %>% 
    filter(!is.na(patinfo_sex) & !is.na(age_group))
  
  
  # long format is needed for the stacked bar chart.
  # (we only use the regular wide format above only for the text labels)
  df_age_sex_long <- df_age_sex %>% 
    # filter(!is.na(age_group)) %>% 
    # subtract out deaths from reported count to get CASES ALONE 
    # needed since we're going to build a STACKED bar chart.
    mutate(reported_alone = reported - deaths) %>% 
    pivot_longer(names_to = "classification", cols = c(reported_alone, deaths)) %>% 
    mutate(classification = fct_relevel(classification, c("deaths", "reported_alone"))) %>% 
    mutate(age_group = factor(age_group, 
                              levels = c("<5", "5-9", "10-19", "20-29", "30-39", "40-49",
                                         "50-59", "60-69", "70-79", ">80"))) %>% 
    arrange(match(age_group, c("<5", "5-9", "10-19", "20-29", "30-39", "40-49",
                               "50-59", "60-69", "70-79", ">80"))) %>% 
    mutate( patinfo_sex = as.factor(patinfo_sex)) %>% 
    filter(!is.na(age_group))
  
  
  p_age_sex_pyramid <- df_age_sex_long %>% 
    ggplot() +
    # males
    geom_bar(data = subset(df_age_sex_long, patinfo_sex == "M" & !is.na(age_group)),
             aes(x = age_group, y = value, fill = classification),
             stat="identity", position = "stack", color = "white", width = 1,
             size = 0.1) +
    scale_fill_manual(breaks = c("deaths"), values = c("red", "steelblue3")) +
    ggnewscale::new_scale_fill() +
    # females
    geom_bar(data = subset(df_age_sex_long, patinfo_sex == "F" & !is.na(age_group)),
             aes(x = age_group, y = -value, fill = classification),
             stat = "identity", position = "stack", color = "white", width = 1,
             size = 0) +
    scale_fill_manual(breaks = c("deaths"), values = c("red", "steelblue1")) +
    # CFR labels for males and females (bold font)
    geom_label(data = subset(df_age_sex, patinfo_sex == "M" & !is.na(age_group)),
               aes(x = age_group, y = reported, 
                   label = paste0("CFR: ", round(100 * deaths / reported, 1), "%")),
               size = 2.2,  fontface = "bold", colour = "dodgerblue4",
               hjust = 0, vjust = 0.2, label.size = 0, fill = "transparent") + 
    geom_label(data = subset(df_age_sex, patinfo_sex == "F" & !is.na(age_group)), 
               aes(x = age_group, y = -reported, 
                   label = paste0("CFR: ", round(100 * deaths / reported, 1), "%")),
               size = 2.2,  fontface = "bold", colour = "dodgerblue3",
               hjust = 1, vjust = 0.2, label.size = 0, fill = "transparent") + 
    # Case and death count labels (plain font)
    geom_label(data = subset(df_age_sex, patinfo_sex == "M" & !is.na(age_group)), 
               aes(x = age_group, y = reported, 
                   label = paste0("(", deaths, " of ", reported, ")")),
               size = 2, fontface = "plain", colour = alpha("dodgerblue4", 0.8),
               hjust = 0, vjust = 1, label.size = 0, fill = "transparent") + 
    geom_label(data = subset(df_age_sex, patinfo_sex == "F" & !is.na(age_group)), 
               aes(x = age_group, y = -reported, 
                   label = paste0("(", deaths, " of ", reported, ")")),
               size = 2, fontface = "plain", colour = alpha("dodgerblue3", 0.8),
               hjust = 1, vjust = 1, label.size = 0, fill = "transparent") + 
    # Female and male labels change this to work for your plot
    annotate("text", x = 10, y = -max(df_age_sex$reported) * 0.95, 
             label = "Female", size = 4, fontface = "bold", colour = "dodgerblue3") +
    annotate("text", x = 10, y = max(df_age_sex$reported) * 0.95, 
             label = "Male", size = 4 , fontface = "bold", colour = "dodgerblue4") +
    labs(x = "Age group", y = "Reported cases and deaths") +
    ggtitle(paste("Age-sex distribution of all COVID-19 reported cases and deaths in", my_region),
            subtitle = "Crude case-fatality rate, deaths and reported cases are indicated") +
    theme(legend.title = element_blank(),
          legend.position = "bottom") + 
    scale_y_continuous(limits = range(c(df_age_sex_long$value * 1.4,
                                        -df_age_sex_long$value * 1.4), na.rm = TRUE), labels = abs) +
    coord_flip()
  
  print(p_age_sex_pyramid)
  
  
  # export
  fig_name <- "Fig_6_Pyramid"
  
  ggsave(paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png"), 
         plot = p_age_sex_pyramid, 
         width = 35, height = 20, units = "cm", dpi = 600)
  
  
  ##################################################
  ##      Table of reported cases by category     ##
  ##################################################
  
  cat("  \n", "  \n##", "Table summaries","  \n")
  
  # This is a duplicate of the treemap, but in tabular form
  factor_table_flex <- factor_table_1 %>%
    select(Factor = classif,
           Level = value,
           Cases = howmany,
           `Prop. (%)` = percent) %>%
    flextable() %>%
    theme_vanilla() %>%
    set_caption(caption = paste("Breakdown of all reported cases by category in", my_region)) %>%
    merge_v(j = "Factor")
  
  #highlight header
  factor_table_flex <- bg(factor_table_flex, bg = "wheat", part = "header")
  
  
  # Save table then read back in as PNG
  table_name <- "Table_1_Treemap"
  
  invisible(save_as_image(factor_table_flex, 
                          path = paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", table_name, ".png") ))
  
  # read back in as PNG raster
  factor_table_flex_img <- readPNG(paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", table_name, ".png"))
  
  
  ###############################
  ##      Table of age sex     ##
  ###############################
  
  # This is a duplicate of the age sex pyramid but in tabular form
  age_sex_table_flex <- df_age_sex %>% 
    mutate(`Crude CFR (%)` = round(100 * deaths/reported), 1) %>% 
    select(`Age group` = age_group, 
           Sex = patinfo_sex, 
           `Reported cases` = reported, 
           Deaths = deaths, 
           `Crude CFR (%)`) %>% 
    # NaN to NA. More presentable
    mutate(`Crude CFR (%)` = ifelse(is.na(`Crude CFR (%)`), NA, `Crude CFR (%)`)) %>% 
    flextable() %>% 
    theme_vanilla() %>% 
    merge_v(j = "`Age group`") %>% 
    set_caption(caption = paste("Reported cases and deaths by age-sex group in", my_region))
  
  #highlight header
  age_sex_table_flex <- bg(age_sex_table_flex, bg = "wheat", part = "header")
  
  # highlight rows with cases
  age_sex_table_flex <- bg(age_sex_table_flex, 
                           j = "Deaths", 
                           i = ~ Deaths > 0, 
                           bg = "#fcd7d4", part = "body")
  
  age_sex_table_flex <- bg(age_sex_table_flex, 
                           j = c("Sex", "Reported cases", "Deaths", "`Crude CFR (%)`"), 
                           i = ~ `Reported cases` > 0, 
                           bg = "#fcd7d4", part = "body")
  
  # highlight rows with deaths
  age_sex_table_flex <- bg(age_sex_table_flex, 
                           j = c("Deaths", "`Crude CFR (%)`"), 
                           i = ~ Deaths > 0, 
                           bg = "#ffa28f", part = "body")
  
  
  # Save table then read back in as PNG
  table_name <- "Table_2_Age_Sex"
  
  invisible(save_as_image(age_sex_table_flex, 
                          path = paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", table_name, ".png")))
  
  # read back in as PNG raster
  age_sex_table_flex_img <- readPNG(paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", table_name, ".png"))
  
  
  # Now print both tables side by side
  grid.arrange(rasterGrob(factor_table_flex_img),
               rasterGrob(age_sex_table_flex_img),
               ncol = 2)
  
  
  ###########################################
  ##      Testing positivity over time     ##
  ###########################################
  
  cat("  \n##", "Test positivity over time", "  \n")
  
  # Weekly test positivity
  df_sub <- df %>% 
    filter(resadmin1_correct == my_region) 
  
  pos_trend <- as.data.frame.matrix(table(df_sub$epiweek, df_sub$Lab_result)) %>% 
    as_tibble(rownames = 'epiweek') %>% 
    mutate(epiweek = as.integer(epiweek)) %>% 
    # add in missing dates
    complete(epiweek = seq(min(df_sub$epiweek), max(df_sub$epiweek))) %>%
    # select only positive and negative tests
    select(epiweek, NEGATIVE, POSITIVE) %>% 
    mutate_at(vars(NEGATIVE,POSITIVE), replace_na, 0) %>% 
    mutate(pos_prop = round(POSITIVE / (NEGATIVE + POSITIVE + 0.001), 2), 
           pos_percent = pos_prop * 100, 
           neg_percent = 100-pos_percent, 
           pos_percent_trend = rollmean(pos_percent, k = 7, 
                                        align = "right", fill = na.fill(pos_percent, "extend")), 
           neg_percent_trend = rollmean(neg_percent, k = 7, 
                                        align = "right", fill = na.fill(neg_percent, "extend")), 
           total_tests = POSITIVE + NEGATIVE)  
  
  # epiweek labels for x axis
  date_labels <- format.Date(MMWRweek2Date(rep(2020, times = nrow(pos_trend)), 
                                           pos_trend$epiweek, 1), "%m.%d")
  
  # add in labels
  pos_trend <- pos_trend %>% 
    mutate(ew_date = paste0("Week", epiweek, "\n", date_labels))
  
  # plot
  p_test <- pos_trend  %>%   
    pivot_longer(c(POSITIVE, NEGATIVE)) %>% 
    ggplot() +
    geom_bar(aes(x = ew_date, y = value, fill = name), 
             stat = "identity", position = "dodge", width = 0.5) +
    scale_fill_manual(values = c(alpha("#13c28c", 0.9), alpha("#d93f16", 0.9)), 
                      labels = c("Negative", "Positive")) +
    geom_text(data = pos_trend, aes(x = ew_date, y = POSITIVE, 
                                    label = paste0(pos_percent, "%\n",
                                                   "(",POSITIVE, "/",
                                                   total_tests, ")")), 
              vjust = -0.05, hjust = 0, size = 1.8, 
              color = "black", lineheight = 0.8, fontface = "bold") +
    scale_x_discrete(name = "Epiweek and first day of that week", 
                     limits = c(pos_trend$ew_date),
                     # expand so that last geom text does not get cut off
                     expand = expansion(mult = c(0, 0.04))) +
    labs(y = "Number of tests conducted", 
         caption = "Tests are PCR tests on nasopharyngeal swabs") +
    ggtitle(paste("Number and results of SARS-CoV-2 tests each week in", my_region)) +
    theme(legend.title = element_blank())  + 
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.x = element_text(size = 6))
  
  print(p_test)
  
  fig_name <- "Fig_7_Tests"
  
  ggsave(paste0(out_path, my_country, "_Regional_Reported_", my_region, "_", fig_name, ".png"), 
         plot = p_test, 
         width = 35, height = 20, units = "cm", dpi = 600)
  
  # page break before new region section begins
  cat("\\newpage")
  
}

