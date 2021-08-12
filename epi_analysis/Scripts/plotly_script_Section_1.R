
###################################
###################################
##                               ##
##      INSTALATION SECTION      ##
##                               ##
###################################
###################################

library(pacman) # installs and/or loads everything

# NOTE TO MYSELF: CHECK WHICH WILL BE MAINTAINED IN THE END
p_load(char = c("lubridate", # for date manipulation
                "MMWRweek", # for epiweek manipulation
                "sf", "rnaturalearth", # for map creation
                "zoo", # for rolling averages (e.g. 7 day rolling average)
                "janitor", # for some handy cleaning functions not in tidyverse
                "treemapify", # for ggplot treemaps
                "raster", # to succesfully import and extract population from the raster file
                "rgdal", # for several spatial functions
                "RColorBrewer", # for palettes
                "leaflet", # for interactive maps
                "plotly", # for interactive plots
                "tidyverse"), # for everything, comes at the end to prevent masking of crucial functions
       update = FALSE)

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
lastdate <- "2020-10-04"

# defining wihch country to analyze
my_country <- "Niger"

# turning off annoying scientific notation
options(scipen = 999)


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



########################################################################################
##                                                                                    ##
##      Loading and formatting faux LL reference and data for continental Africa      ##
##                                                                                    ##
########################################################################################

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
  mutate(Epiweek = lubridate::isoweek(Reporting_Date)) %>% 
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

# join africa geoms with the COVID info about countries
africa_map <- africa_union %>% 
  st_as_sf() %>% 
  st_set_crs(4326) %>%
  mutate(name_long = case_when(name_long == "Cape Verde" ~ "Cabo Verde",
                               TRUE ~ name_long)) %>%
  rename(Country = name_long) %>%
  left_join(all_country_tab, by = "Country")

# Inserting breaks for choropleth maps
africa_map <- africa_map %>%
  mutate(cases_quintile = cut(Cases, 
                              quantile(africa_map$Cases, 
                                       probs = seq(0, 1, length.out = 6), na.rm = TRUE), 
                              include.lowest = TRUE),
         deaths_quintile = cut(Deaths, 
                               quantile(africa_map$Deaths, 
                                        probs = seq(0, 1, length.out = 6), na.rm = TRUE), 
                               include.lowest = TRUE),
         cases_per_m_quintile = cut(`Cases per million`, 
                                    quantile(africa_map$`Cases per million`, 
                                             probs = seq(0, 1, length.out = 6), na.rm = TRUE), 
                                    include.lowest = TRUE),
         deaths_per_m_quintile = cut(`Deaths per million`, 
                                     quantile(africa_map$`Deaths per million`, 
                                              probs = seq(0, 1, length.out = 6), na.rm = TRUE), 
                                     include.lowest = TRUE))

# creating palletes for variables
pallete.cases <- colorFactor(palette = "YlOrRd", africa_map$cases_quintile)
pallete.deaths <- colorFactor(palette = "YlOrRd", africa_map$deaths_quintile)
pallete.cases_per_m <- colorFactor(palette = "YlOrRd", africa_map$cases_per_m_quintile)
pallete.deaths_per_m <- colorFactor(palette = "YlOrRd", africa_map$deaths_per_m_quintile)


###################################################
##                                               ##
##      SETTING COLOR THEME FOR RColorBrewer     ##
##        NEEDED TO AVOID ERROR IN PLOTLY        ##
##                                               ##
###################################################

# Define the number of colors to prevent error in plots
nb.cols_1 <- length(unique(df_country$Country))
mycolors_1 <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols_1)


################################################
##                                            ##
##      SECTION 1 GRAPHS, MAPS AND TABLES     ##
##                                            ##
################################################

##################################################################
##                                                              ##
##      Daily absolute cases and deaths plot for my_country     ##
##                                                              ##
##################################################################

epi_curve_ll <- df_country %>% 
  filter(Country == my_country) %>% 
  select(Reporting_Date, Cases_this_day, Deaths_this_day) %>% 
  mutate(seven_day_case_avg = rollmean(x = Cases_this_day, k = 7, align = "right",  
                                       fill = na.fill(Cases_this_day, 0)),
         fourteen_day_case_avg = rollmean(x = Cases_this_day, k = 14, align = "right",  
                                          fill = na.fill(Cases_this_day, 0)),
         seven_day_death_avg = rollmean(x = Deaths_this_day, k = 7, align = "right",  
                                        fill = na.fill(Deaths_this_day, 0)),
         fourteen_day_death_avg = rollmean(x = Deaths_this_day, k = 14, align = "right",  
                                           fill = na.fill(Deaths_this_day, 0)))

# interactive plot for confirmd cases
epi_curve_ll %>%
  plot_ly(x = ~Reporting_Date) %>%
  add_bars(y = ~Cases_this_day, 
           colors = mycolors_1,
           name = "Cases this day", 
           hoverinfo = "text+x",
           text = ~paste0("<b>Confirmed cases in ", my_country, ": </b>", Cases_this_day)) %>%
  add_trace(y = ~seven_day_case_avg, 
            name = "7-day rolling avg. cases", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dash"),
            hoverinfo = "text+x",
            text = ~paste("<b>7-day rolling avg.: </b>", round(seven_day_case_avg, 2))) %>%
  add_trace(y = ~fourteen_day_case_avg, 
            name = "14-day rolling avg. cases", 
            type = "scatter",
            mode = "lines", 
            line = list(color = "black", dash = "dot"),
            hoverinfo = "text+x",
            text = ~paste0("<b>14-day rolling avg.: </b>", round(fourteen_day_case_avg, 2))) %>%
  layout(hovermode = "x unified",
         title = paste0("Daily COVID-19 confirmed cases in", my_country),
         yaxis = list(title = "Absolute number of COVID-19 confirmed cases"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
  )

# interactive plot for deaths
epi_curve_ll %>%
  plot_ly(x = ~Reporting_Date) %>%
  add_bars(y = ~Deaths_this_day, 
           colors = mycolors_1,
           name = "Cases this day", 
           hoverinfo = "text+x",
           text = ~paste0("<b>Deaths in ", my_country, ": </b>", Deaths_this_day)) %>%
  add_trace(y = ~seven_day_death_avg, 
            name = "7-day rolling avg. cases", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dash"),
            hoverinfo = "text+x",
            text = ~paste("<b>7-day rolling avg.: </b>", round(seven_day_death_avg, 2))) %>%
  add_trace(y = ~fourteen_day_death_avg, 
            name = "14-day rolling avg. cases", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dot"),
            hoverinfo = "text+x",
            text = ~paste0("<b>14-day rolling avg.: </b>", round(fourteen_day_death_avg, 2))) %>%
  layout(hovermode = "x unified",
         title = paste0("Daily COVID-19 deaths in", my_country),
         yaxis = list(title = "Absolute number of COVID-19 deaths"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
  )


##############################################
##                                          ##
##      GROWTH RATE OF CASES OVER TIME      ##
##                                          ##
##############################################

# growth rate of reported cases df
growth_rate_tab <- df_country %>% 
  filter(Country == my_country) %>% 
  select(Reporting_Date, Epiweek, Cases_past_week) %>% 
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

# growth rate of confirmed cases interactive plot
growth_rate_tab %>%
  plot_ly(x = ~Reporting_Date) %>%
  # ribbons are polygons in the background
  add_ribbons(x = ~Reporting_Date, 
              ymin = 0, 
              # ymax needs to remove Inf or otherwise plotly will explode to a large ymax
              ymax = max(growth_rate_tab$week_growth_perc[growth_rate_tab$week_growth_perc != Inf], 
                         na.rm = TRUE),
              color = I("red"), # red for increase in growth rate
              opacity = 0.5,
              hoverinfo = "none", # removes the hovering text (it is not needed in here)
              showlegend = FALSE, # to remove the unneeded trace info 
              line = list(color = "rgba(0, 0, 0, 0)")) %>%  # making contour line transparent
  add_ribbons(x = ~Reporting_Date, 
              ymax = 0, 
              ymin = min(growth_rate_tab$week_growth_perc[growth_rate_tab$week_growth_perc != Inf], 
                         na.rm = TRUE),
              color = I("green"), # green for decrease in growth rate
              opacity = 0.5,
              hoverinfo = "none", 
              showlegend = FALSE, 
              line = list(color = "rgba(0, 0, 0, 0)")) %>%
  add_trace(y = ~week_growth_perc, 
            name = "Weekly growth rate", 
            type = "scatter", # configuring trace as scatterplot
            mode = "markers+lines", # lines + points
            color = I("black"),
            hoverinfo = "text+x",
            text = ~paste0("<b>Date of reporting: </b>", Reporting_Date,
                           "<br><b>Epidemiological week: </b>", Epiweek,
                           "<br><b>Weekly growth rate: </b>", paste0(round(week_growth_perc, 2), "%"))) %>%
  layout(hovermode = "unified x",
         
         title = paste0("Week-on-week growth rate of new COVID-19 confirmed cases in ", my_country),
         yaxis = list(title = "Average daily growth rate (%) each week"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
         )


##############################################################
##                                                          ##
##      REGIONAL CASE AND DEATH PER MILLION COMPARISONS     ##
##                                                          ##
##############################################################

# comparison of incidence per million inhabitants with countries of the same region
df_regional_comparison <- df_country %>% 
  filter(Region == my_region)%>% 
  select(Reporting_Date, Cases_per_million, Deaths_per_million, Country) %>%
  group_by(Country) 

# interactive time series plot of confirmed cases
df_regional_comparison %>%
  plot_ly(x = ~Reporting_Date, 
          y = ~Cases_per_million, 
          color = ~Country, 
          colors = mycolors_1,
          type = "scatter", 
          mode = "lines",
          hoverinfo = "text+x",
          text = ~paste0("<b>Country: </b>", Country, 
                         "<br><b>Date: </b>", Reporting_Date,
                         "<br><b>Confirmed cases per million: </b>", round(Cases_per_million, 2))) %>%
  layout(hovermode = "unified x",
         title = paste0("Cumulative COVID-19 confirmed cases per million in countries in", my_region),
         yaxis = list(title = "COVID-19 confirmed cases per million"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
         )

# interactive time series plot of deaths
df_regional_comparison %>%
  plot_ly(x = ~Reporting_Date, 
          y = ~Deaths_per_million, 
          color = ~Country, 
          colors = mycolors_1,
          type = "scatter", 
          mode = "lines",
          hoverinfo = "text+x",
          text = ~paste0("<b>Country: </b>", Country, 
                         "<br><b>Date: </b>", Reporting_Date,
                         "<br><b>Deaths per million: </b>", round(Deaths_per_million, 2))) %>%
  layout(hovermode = "unified x",
         title = paste0("Cumulative COVID-19 deaths per million in countries in", my_region),
         yaxis = list(title = "COVID-19 deaths per million"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
         )


######################################################
##                                                  ##
##      Map of cumulative cases continent-wide      ##
##                                                  ##
######################################################

# interactive map for continental Africa
leaflet(africa_map) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.cases_per_m(cases_per_m_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>", "Country: ", "</b>", name, "<br>",
                              "<b>", "Region: ", "</b>", Region, "<br>",
                              "<b>", "Cases: ", "</b>", format(Cases,
                                                               decimal.mark = ".",
                                                               big.mark = ","), "<br>",
                              "<b>", "Deaths: ", "</b>", format(Deaths,
                                                                decimal.mark = ".",
                                                                big.mark = ","), "<br>",
                              "<b>", "Cases per million: ", "</b>", format(round(`Cases per million`, 2),
                                                                           decimal.mark = ".",
                                                                           big.mark = ","), "<br>",
                              "<b>", "Deaths per million: ", "</b>", format(round(`Deaths per million`, 2), 
                                                                            decimal.mark = ".", 
                                                                            big.mark = ","), "<br>")) %>%  
  addLegend("bottomright", 
            pal = pallete.cases_per_m,
            values = ~cases_per_m_quintile,
            title = ~paste0("Confirmed COVID-19 cases per million"),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
    )


######################################################
##                                                  ##
##      Map of cumulative deaths continent-wide     ##
##                                                  ##
######################################################

# interactive map for continental Africa
leaflet(africa_map) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.deaths_per_m(deaths_per_m_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>", "Country: ", "</b>", name, "<br>",
                              "<b>", "Region: ", "</b>", Region, "<br>",
                              "<b>", "Cases: ", "</b>", format(Cases,
                                                               decimal.mark = ".",
                                                               big.mark = ","), "<br>",
                              "<b>", "Deaths: ", "</b>", format(Deaths,
                                                                decimal.mark = ".",
                                                                big.mark = ","), "<br>",
                              "<b>", "Cases per million: ", "</b>", format(round(`Cases per million`, 2),
                                                                           decimal.mark = ".",
                                                                           big.mark = ","), "<br>",
                              "<b>", "Deaths per million: ", "</b>", format(round(`Deaths per million`, 2), 
                                                                            decimal.mark = ".", 
                                                                            big.mark = ","), "<br>")) %>%  
  addLegend("bottomright", 
            pal = pallete.deaths_per_m,
            values = ~deaths_per_m_quintile,
            title = ~paste0("COVID-19 deaths per million"),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )
