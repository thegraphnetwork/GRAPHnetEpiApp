
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

plot_ly(epi_curve_ll, x = ~Reporting_Date) %>%
  add_bars(y = ~Cases_this_day, name = "Cases this day", 
           hoverinfo = "text",
           text = ~paste("Confirmed cases: ", Cases_this_day),
           visible = TRUE) %>%
  add_trace(y = ~seven_day_case_avg, name = "7-day rolling avg. cases", 
            type = "scatter", mode = "lines", 
            line = list(color = "black", dash = "dash"),
            hoverinfo = "text",
            text = ~paste("7-day rolling avg.: ", seven_day_case_avg),
            visible = TRUE) %>%
  add_trace(y = ~fourteen_day_case_avg, name = "14-day rolling avg. cases", 
            type = "scatter", mode = "lines", 
            line = list(color = "black", dash = "dot"),
            hoverinfo = "text",
            text = ~paste("14-day rolling avg.: ", fourteen_day_case_avg),
            visible = TRUE) %>%
  add_bars(y = ~Deaths_this_day, name = "Deaths this day", 
           marker = list(color = "#56bfa3"),
           hoverinfo = "text",
           text = ~paste("Deaths: ", Deaths_this_day),
           visible = FALSE) %>%
  add_trace(y = ~seven_day_death_avg, name = "7-day rolling avg. deaths", 
            type = "scatter", mode = "lines", 
            line = list(color = "black", dash = "dash"),
            hoverinfo = "text",
            text = ~paste("7-day rolling avg.: ", seven_day_death_avg),
            visible = FALSE) %>%
  add_trace(y = ~fourteen_day_death_avg, name = "14-day rolling avg. deaths", 
            type = "scatter", mode = "lines", 
            line = list(color = "black", dash = "dot"),
            hoverinfo = "text",
            text = ~paste("14-day rolling avg.: ", seven_day_death_avg),
            visible = FALSE) %>%
  layout(updatemenus = list(
    list(
      y = 0,
      buttons = list(
        list(method = "restyle",
             args = list("visible", list(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)),
             label = "Cases"),
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)),
             label = "Deaths")))),
    hovermode = "x unified",
    title = paste("Daily COVID-19 cases in", my_country),
    yaxis = list(
      title = "Absolute number of COVID-19 cases"),
    xaxis = list(
      title = "Date of Reporting",
      type = "date",
      tickformat = "%b %d (%a)",
      rangeslider = list(type = "date")
    )
  )



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

# plot
plot_ly(growth_rate_tab, x = ~Reporting_Date) %>%
  # ribbons are polygons in the background
  add_ribbons(x = ~Reporting_Date, ymin = 0, 
              # ymax needs to remove Inf or otherwise plotly will explode to a large ymax
              ymax = max(growth_rate_tab$week_growth_perc[growth_rate_tab$week_growth_perc != Inf], 
                         na.rm = TRUE),
              hoverinfo = "none", # removes the hovering text (it is not needed in here)
              showlegend = FALSE, # to remove the unneeded trace info 
              line = list(color = "rgba(0, 0, 0, 0)"), # making contour line transparent
              fillcolor = "rgba(247, 157, 87, 0.3)") %>% # red for increase in growth rate
  add_ribbons(x = ~Reporting_Date, ymax = 0, 
              ymin = min(growth_rate_tab$week_growth_perc[growth_rate_tab$week_growth_perc != Inf], 
                         na.rm = TRUE),
              hoverinfo = "none", showlegend = FALSE, 
              line = list(color = "rgba(0, 0, 0, 0)"), # making contour line transparent
              fillcolor = "rgba(86, 191, 163, 0.3)") %>% # green for decrease in growth rate
  add_trace(y = ~week_growth_perc, name = "Weekly growth rate", 
            type = "scatter", # configuring trace as scatterplot
            mode = "markers+lines", # lines + points
            # configuring hoverinfo as text with a small title before and rounded values
            hoverinfo = "text",
            text = ~paste("Weekly growth rate: ", round(week_growth_perc, 2), "%")) %>%
  layout(
    title = paste0("Week-on-week growth rate of new COVID-19 cases in ", my_country),
    yaxis = list(
      title = "Average daily growth rate (%) each week"),
    xaxis = list(
      title = "Date of Reporting",
      type = "date",
      tickformat = "%b %d (%a)",
      rangeslider = list(type = "date")
    )
  )



################################################################
##                                                            ##
##      REGIONAL CASE and DEATH PER MILLION COMPARISONS       ##
##                                                            ##
################################################################

# comparison of incidence per million inhabitants with countries of the same region
df_regional_comparison <- df_country %>% 
  filter(Region == my_region)%>% 
  select(Reporting_Date, Cases_per_million, Deaths_per_million, Country) %>%
  group_by(Country) 

# plot
plot_ly(df_regional_comparison, x = ~Reporting_Date, y = ~Cases_per_million, 
        color = ~Country, type = "scatter", mode = "lines",
        hoverinfo = "text", visible = TRUE,
        text = ~paste("<b>Country: </b>", Country, 
                      "<br><b>Date: </b>", Reporting_Date,
                      "<br><b>Confirmed cases per million: </b>", round(Cases_per_million, 2))) %>%
  add_trace(y = ~Deaths_per_million, color = ~Country, type = "scatter", mode = "lines",
            hoverinfo = "text", visible = FALSE,
            text = ~paste("<b>Country: </b>", Country, 
                          "<br><b>Date: </b>", Reporting_Date,
                          "<br><b>Deaths cases per million: </b>", round(Deaths_per_million, 2))) %>%
  layout(updatemenus = list(
    list(
      y = 0,
      buttons = list(
        list(method = "restyle",
             args = list("visible", list(TRUE, FALSE)),
             label = "Cases per million"),
        list(method = "restyle",
             args = list("visible", list(FALSE, TRUE)),
             label = "Deaths per million")))),
    title = paste("Cumulative cases and deaths per million for countries in", my_region),
    yaxis = list(
      title = "Cases/Deaths per million"),
    xaxis = list(
      title = "Date of Reporting",
      type = "date",
      tickformat = "%b %d (%a)",
      rangeslider = list(type = "date")
    )
  )



######################################################
##                                                  ##
##      Map of cumulative cases continent-wide      ##
##                                                  ##
######################################################

# interactive map for continental Africa
leaflet(africa_map) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satélite") %>%
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
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satélite") %>%
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
