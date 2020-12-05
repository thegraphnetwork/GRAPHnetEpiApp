
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
                "ggspatial", # for map notation
                "RColorBrewer", # for palettes
                "RPostgreSQL", # to connect to PostgreSQL database
                "leaflet", # for interactive maps
                "plotly", # for interactive plots
                "RPostgreSQL", # for postgreSQL
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

###################################
##                               ##
##     Defining db variables     ##
##                               ##
###################################

# info to connect to db
dbname <- "shapp"
user <- "postgres"
password <- "postgres"
host <- "postgres"

# connecting to db
db_app <- dbConnect(dbDriver("PostgreSQL"), user=user,
                    password=password, dbname=dbname)

# listing all available db
dbListTables(db_app)

# Tables are inside schemas
dbListFields(db_app, "line_list")
dbListFields(db_app, "country")

# defining which country to analyze
my_country <- "Burkina Faso"

# Importing db and converting to tibble for easy to use dplyr functions
df <- dbReadTable(db_app, "line_list") %>%
  as_tibble() %>%
  filter(country_name == my_country)
  
# defining which country gpkg should be loaded
df_gpkg <- st_read(db_app, "burkina_faso_gadm36 gadm36_BFA_1")

# turning off annoying scientific notation
options(scipen = 999)


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

# cleaning and manipulating df
df <- df %>%
  # Transforming all dates properly
  mutate(report_date = as.Date(report_date),
         consultation_dateHF = as.Date(consultation_datehf),
         Lab_datetaken = as.Date(lab_datetaken),
         Lab_resdate = as.Date(lab_resdate),
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
         report_date = if_else(is.na(report_date), consultation_datehf, report_date),
         report_date = if_else(is.na(report_date), lab_datetaken, report_date),
         report_date = if_else(is.na(report_date), patcourse_dateonset, report_date),
         report_date = if_else(is.na(report_date), patcourse_datedeath, report_date),
         report_date = if_else(is.na(report_date), patcourse_datedischarge, report_date),
         report_date = if_else(is.na(report_date), lab_resdate, report_date)) %>%
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
dictionary <- read.csv2("./Report/Scripts/Dictionaries/dict_niger.csv", encoding = "UTF-8")

# testing if the names in the cleanCSV are correct or not
test_incorrect_admin1 <- df$patinfo_resadmin1 %in% dictionary$X.U.FEFF.incorrect[dictionary$admin_lvl == 1]

test_incorrect_admin2 <- df$patinfo_resadmin2 %in% dictionary$X.U.FEFF.incorrect[dictionary$admin_lvl == 2]

df$resadmin1_correct <- NA

df$resadmin2_correct <- NA

# Loop to test individually if each patinfo_resadmin1 entry is correct or not
for(i in seq_along(df$patinfo_resadmin1)){
  if(test_incorrect_admin1[i] == TRUE) {
    index <- dictionary$X.U.FEFF.incorrect %in% df$patinfo_resadmin1[i]
    df$resadmin1_correct[i] <- dictionary$correct[dictionary$admin_lvl == 1 & index == TRUE]
  }
  if(test_incorrect_admin2[i] == TRUE) {
    index <- dictionary$X.U.FEFF.incorrect %in% df$patinfo_resadmin2[i]
    df$resadmin2_correct[i] <- dictionary$correct[dictionary$admin_lvl == 2 & index == TRUE]
  }
}

# E se nao existir nada para corrigir?
# Resposta: A saida que elaboramos foi de espelhar o conteudo; os dicionarios possuem os nomes corretos 
# na variavel "incorrect" para, caso a entrada esteja certa, o retorno dela no loop continua correto


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
  mutate(epiweek = lubridate::isoweek(report_date))


####################################################################
##     Extracting population values from raster to gpkg files     ##
####################################################################

# Reading population data
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
df2 <- df %>% 
  mutate(resadmin1_correct = case_when(is.na(resadmin1_correct) ~ "No Info",
                                       TRUE ~ resadmin1_correct),
         resadmin1_correct = fct_relevel(resadmin1_correct, "No Info", after = Inf))

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
  mutate(epiweek = lubridate::isoweek(report_date),
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
  mutate(epiweek = lubridate::isoweek(report_date),
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

# Merging my_country daily DF and gpkg
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

# Merging my_country daily EW and gpkg
df_gpkg_ew <- df_gpkg %>%
  st_as_sf() %>%
  full_join(df_ew, by = c("NAME_1" = "resadmin1_correct")) %>%
  # this part of the code is aimed to fill the regions without cases with data
  # this way plotting the maps wont show holes
  # replacing NA with zeros in all epidemiological variables 
  mutate_at(vars(reported_this_week:discharges_trend_log), ~replace_na(., 0)) %>%
  # inserting the max epiweek in the regions with missing data
  mutate(epiweek = if_else(is.na(epiweek), max(na.omit(epiweek)), epiweek))


################################################################
##                                                            ##
##      CREATING BREAKS AND PALLETES FOR CHOROPLETH MAPS      ##
##                                                            ##
################################################################

df_gpkg_daily <- df_gpkg_daily %>%
  mutate(reported_quintile = cut(cum_reported, 
                                 unique(quantile(df_gpkg_daily$cum_reported, 
                                                 probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                 include.lowest = TRUE),
         confirmed_quintile = cut(cum_confirmed, 
                                  unique(quantile(df_gpkg_daily$cum_confirmed, 
                                                  probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                  include.lowest = TRUE),
         deaths_quintile = cut(cum_deaths, 
                               unique(quantile(df_gpkg_daily$cum_deaths, 
                                               probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                               include.lowest = TRUE),
         incidence_reported_quintile = cut(incidence_reported, 
                                           unique(quantile(df_gpkg_daily$incidence_reported, 
                                                           probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                           include.lowest = TRUE),
         incidence_confirmed_quintile = cut(incidence_confirmed, 
                                            unique(quantile(df_gpkg_daily$incidence_confirmed, 
                                                            probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                            include.lowest = TRUE),
         mortality_quintile = cut(mortality, 
                                  unique(quantile(df_gpkg_daily$mortality, 
                                                  probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                  include.lowest = TRUE))

# creating palletes for variables
pallete.reported <- colorFactor(palette = "YlOrRd", df_gpkg_daily$reported_quintile)
pallete.confirmed <- colorFactor(palette = "YlOrRd", df_gpkg_daily$confirmed_quintile)
pallete.deaths <- colorFactor(palette = "YlOrRd", df_gpkg_daily$deaths_quintile)
pallete.incidence_reported <- colorFactor(palette = "YlOrRd", df_gpkg_daily$incidence_reported_quintile)
pallete.incidence_confirmed <- colorFactor(palette = "YlOrRd", df_gpkg_daily$incidence_confirmed_quintile)
pallete.mortality <- colorFactor(palette = "YlOrRd", df_gpkg_daily$mortality_quintile)


###################################################
##                                               ##
##      SETTING COLOR THEME FOR RColorBrewer     ##
##        NEEDED TO AVOID ERROR IN PLOTLY        ##
##                                               ##
###################################################

# Define the number of colors to prevent error in plots
nb.cols_2 <- length(unique(df_daily$resadmin1_correct))
mycolors_2 <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols_2)


#####################################################################
##                                                                 ##
##      MAPS SECTION: SEVERAL THAT COULD SUIT BEST THE REPORT      ##
##                                                                 ##
#####################################################################

########################################################################
##      Interactive region level maps: cumulative reported cases      ##
########################################################################

df_gpkg_daily %>%
  filter(report_date == max(report_date, na.rm = TRUE)) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.reported(reported_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>", "Country: ", "</b>", NAME_0, "<br>",
                              "<b>", "Region: ", "</b>", NAME_1, "<br>",
                              "<b>", "Reported cases: ", "</b>", format(cum_reported,
                                                                        decimal.mark = ".",
                                                                        big.mark = ","), "<br>",
                              "<b>", "Confirmed cases: ", "</b>", format(cum_confirmed,
                                                                         decimal.mark = ".",
                                                                         big.mark = ","), "<br>",
                              "<b>", "Deaths: ", "</b>", format(cum_deaths,
                                                                decimal.mark = ".",
                                                                big.mark = ","), "<br>",
                              "<b>", "Incidence of reported cases: ", "</b>", format(round(incidence_reported, 2),
                                                                                     decimal.mark = ".",
                                                                                     big.mark = ","), "<br>",
                              "<b>", "Incidence of confirmed cases: ", "</b>", format(round(incidence_confirmed, 2), 
                                                                                      decimal.mark = ".", 
                                                                                      big.mark = ","), "<br>")) %>%  
  addLegend("bottomright", 
            pal = pallete.reported,
            values = ~reported_quintile,
            title = ~paste0("Cumulative reported COVID-19 cases as of ", max(df_gpkg_daily$report_date)),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )


#########################################################################
##      Interactive region level maps: cumulative confirmed cases      ##
#########################################################################

df_gpkg_daily %>%
  filter(report_date == max(report_date, na.rm = TRUE)) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.confirmed(confirmed_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>", "Country: ", "</b>", NAME_0, "<br>",
                              "<b>", "Region: ", "</b>", NAME_1, "<br>",
                              "<b>", "Reported cases: ", "</b>", format(cum_reported,
                                                                        decimal.mark = ".",
                                                                        big.mark = ","), "<br>",
                              "<b>", "Confirmed cases: ", "</b>", format(cum_confirmed,
                                                                         decimal.mark = ".",
                                                                         big.mark = ","), "<br>",
                              "<b>", "Deaths: ", "</b>", format(cum_deaths,
                                                                decimal.mark = ".",
                                                                big.mark = ","), "<br>",
                              "<b>", "Incidence of reported cases: ", "</b>", format(round(incidence_reported, 2),
                                                                                     decimal.mark = ".",
                                                                                     big.mark = ","), "<br>",
                              "<b>", "Incidence of confirmed cases: ", "</b>", format(round(incidence_confirmed, 2), 
                                                                                      decimal.mark = ".", 
                                                                                      big.mark = ","), "<br>")) %>%  
  addLegend("bottomright", 
            pal = pallete.confirmed,
            values = ~confirmed_quintile,
            title = ~paste0("Cumulative confirmed COVID-19 cases as of ", max(df_gpkg_daily$report_date)),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )


####################################################
##      Region level maps: cumulative deaths      ##
####################################################

df_gpkg_daily %>%
  filter(report_date == max(report_date, na.rm = TRUE)) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.deaths(deaths_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>", "Country: ", "</b>", NAME_0, "<br>",
                              "<b>", "Region: ", "</b>", NAME_1, "<br>",
                              "<b>", "Reported cases: ", "</b>", format(cum_reported,
                                                                        decimal.mark = ".",
                                                                        big.mark = ","), "<br>",
                              "<b>", "Confirmed cases: ", "</b>", format(cum_confirmed,
                                                                         decimal.mark = ".",
                                                                         big.mark = ","), "<br>",
                              "<b>", "Deaths: ", "</b>", format(cum_deaths,
                                                                decimal.mark = ".",
                                                                big.mark = ","), "<br>",
                              "<b>", "Incidence of reported cases: ", "</b>", format(round(incidence_reported, 2),
                                                                                     decimal.mark = ".",
                                                                                     big.mark = ","), "<br>",
                              "<b>", "Incidence of confirmed cases: ", "</b>", format(round(incidence_confirmed, 2), 
                                                                                      decimal.mark = ".", 
                                                                                      big.mark = ","), "<br>")) %>%  
  addLegend("bottomright", 
            pal = pallete.deaths,
            values = ~deaths_quintile,
            title = ~paste0("Cumulative COVID-19 deaths as of ", max(df_gpkg_daily$report_date)),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )


###################################################################
##      Region level maps: incidence rate of reported cases      ##
###################################################################

df_gpkg_daily %>%
  filter(report_date == max(report_date, na.rm = TRUE)) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.incidence_reported(incidence_reported_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>", "Country: ", "</b>", NAME_0, "<br>",
                              "<b>", "Region: ", "</b>", NAME_1, "<br>",
                              "<b>", "Reported cases: ", "</b>", format(cum_reported,
                                                                        decimal.mark = ".",
                                                                        big.mark = ","), "<br>",
                              "<b>", "Confirmed cases: ", "</b>", format(cum_confirmed,
                                                                         decimal.mark = ".",
                                                                         big.mark = ","), "<br>",
                              "<b>", "Deaths: ", "</b>", format(cum_deaths,
                                                                decimal.mark = ".",
                                                                big.mark = ","), "<br>",
                              "<b>", "Incidence of reported cases: ", "</b>", format(round(incidence_reported, 2),
                                                                                     decimal.mark = ".",
                                                                                     big.mark = ","), "<br>",
                              "<b>", "Incidence of confirmed cases: ", "</b>", format(round(incidence_confirmed, 2), 
                                                                                      decimal.mark = ".", 
                                                                                      big.mark = ","), "<br>")) %>%  
  addLegend("bottomright", 
            pal = pallete.incidence_reported,
            values = ~incidence_reported_quintile,
            title = ~paste0("Incidence of reported COVID-19 cases as of ", max(df_gpkg_daily$report_date)),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )


####################################################################
##      Region level maps: incidence rate of confirmed cases      ##
####################################################################

df_gpkg_daily %>%
  filter(report_date == max(report_date, na.rm = TRUE)) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.incidence_confirmed(incidence_confirmed_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>", "Country: ", "</b>", NAME_0, "<br>",
                              "<b>", "Region: ", "</b>", NAME_1, "<br>",
                              "<b>", "Reported cases: ", "</b>", format(cum_reported,
                                                                        decimal.mark = ".",
                                                                        big.mark = ","), "<br>",
                              "<b>", "Confirmed cases: ", "</b>", format(cum_confirmed,
                                                                         decimal.mark = ".",
                                                                         big.mark = ","), "<br>",
                              "<b>", "Deaths: ", "</b>", format(cum_deaths,
                                                                decimal.mark = ".",
                                                                big.mark = ","), "<br>",
                              "<b>", "Incidence of reported cases: ", "</b>", format(round(incidence_reported, 2),
                                                                                     decimal.mark = ".",
                                                                                     big.mark = ","), "<br>",
                              "<b>", "Incidence of confirmed cases: ", "</b>", format(round(incidence_confirmed, 2), 
                                                                                      decimal.mark = ".", 
                                                                                      big.mark = ","), "<br>")) %>%  
  addLegend("bottomright", 
            pal = pallete.incidence_confirmed,
            values = ~incidence_confirmed_quintile,
            title = ~paste0("Incidence of confirmed COVID-19 cases as of ", max(df_gpkg_daily$report_date)),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )


#################################################
##      Region level maps: mortality rate      ##
#################################################

df_gpkg_daily %>%
  filter(report_date == max(report_date, na.rm = TRUE)) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.mortality(mortality_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>", "Country: ", "</b>", NAME_0, "<br>",
                              "<b>", "Region: ", "</b>", NAME_1, "<br>",
                              "<b>", "Reported cases: ", "</b>", format(cum_reported,
                                                                        decimal.mark = ".",
                                                                        big.mark = ","), "<br>",
                              "<b>", "Confirmed cases: ", "</b>", format(cum_confirmed,
                                                                         decimal.mark = ".",
                                                                         big.mark = ","), "<br>",
                              "<b>", "Deaths: ", "</b>", format(cum_deaths,
                                                                decimal.mark = ".",
                                                                big.mark = ","), "<br>",
                              "<b>", "Incidence of reported cases: ", "</b>", format(round(incidence_reported, 2),
                                                                                     decimal.mark = ".",
                                                                                     big.mark = ","), "<br>",
                              "<b>", "Incidence of confirmed cases: ", "</b>", format(round(incidence_confirmed, 2), 
                                                                                      decimal.mark = ".", 
                                                                                      big.mark = ","), "<br>")) %>%  
  addLegend("bottomright", 
            pal = pallete.mortality,
            values = ~mortality_quintile,
            title = ~paste0("Mortality of COVID-19 as of ", max(df_gpkg_daily$report_date)),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )


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

# constructing a df with several variables for interactive plotly
df_epi_curve <- df_daily %>% 
  ungroup() %>%
  select(report_date:discharges_this_day) %>% 
  mutate(resadmin1_correct_reported =
           # Maintaining only the 5 provinces with highest number of reported cases, 
           # others are summed and recoded
           case_when(!resadmin1_correct %in% provinces_most_reported 
                     | resadmin1_correct == "NA" ~ "Other",
                     TRUE ~ as.character(resadmin1_correct)),
         resadmin1_correct_reported = as.factor(resadmin1_correct_reported),
         resadmin1_correct_reported = fct_relevel(resadmin1_correct_reported, "Other", after = Inf),
         resadmin1_correct_confirmed =
           # Maintaining only the 5 provinces with highest number of confirmed cases, 
           # others are summed and recoded
           case_when(!resadmin1_correct %in% provinces_most_confirmed 
                     | resadmin1_correct == "NA" ~ "Other",
                     TRUE ~ as.character(resadmin1_correct)),
         resadmin1_correct_confirmed = as.factor(resadmin1_correct_confirmed),
         resadmin1_correct_confirmed = fct_relevel(resadmin1_correct_confirmed, "Other", after = Inf),
         resadmin1_correct_deaths =
           # Maintaining only the 5 provinces with highest number of deaths, 
           # others are summed and recoded
           case_when(!resadmin1_correct %in% provinces_most_deaths 
                     | resadmin1_correct == "NA" ~ "Other",
                     TRUE ~ as.character(resadmin1_correct)),
         resadmin1_correct_deaths = as.factor(resadmin1_correct_deaths),
         resadmin1_correct_deaths = fct_relevel(resadmin1_correct_deaths, "Other", after = Inf),
         resadmin1_correct_discharges =
           # Maintaining only the 5 provinces with highest number of discharges, 
           # others are summed and recoded
           case_when(!resadmin1_correct %in% provinces_most_discharges 
                     | resadmin1_correct == "NA" ~ "Other",
                     TRUE ~ as.character(resadmin1_correct)),
         resadmin1_correct_discharges = as.factor(resadmin1_correct_discharges),
         resadmin1_correct_discharges = fct_relevel(resadmin1_correct_discharges, "Other", after = Inf))

# daily absolute reported cases plot per region
df_epi_curve %>%
  plot_ly(x = ~report_date) %>%
  add_bars(y = ~reported_this_day, 
           color = ~resadmin1_correct_reported, 
           legendgroup = ~resadmin1_correct_reported,
           hoverinfo = "text+x", 
           text = ~paste0("<b>Reported cases in ", resadmin1_correct_reported, ": </b>", reported_this_day)) %>%
  add_trace(data = df_daily_national,
            y = ~reported_7day_avg, 
            name = "7-day rolling average", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dash"),
            hoverinfo = "text+x",
            text = ~paste0("<b>7-day rolling avg.: </b>", round(reported_7day_avg, 2))) %>%
  add_trace(data = df_daily_national,
            y = ~reported_14day_avg, 
            name = "14-day rolling average", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dot"),
            hoverinfo = "text+x",
            text = ~paste0("<b>14-day rolling avg.: </b>", round(reported_14day_avg, 2))) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Daily COVID-19 reported cases in ", my_country, 
                        "<br> as of ", max(df_epi_curve$report_date, na.rm = TRUE)),
         yaxis = list(title = "Absolute number of reported cases"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
  )

# daily absolute confirmed cases plot per region
df_epi_curve %>%
  plot_ly(x = ~report_date) %>%
  add_bars(y = ~confirmed_this_day, 
           color = ~resadmin1_correct_confirmed, 
           legendgroup = ~resadmin1_correct_confirmed,
           hoverinfo = "text+x", 
           text = ~paste0("<b>Confirmed cases in ", resadmin1_correct_confirmed, ": </b>", confirmed_this_day)) %>%
  add_trace(data = df_daily_national,
            y = ~confirmed_7day_avg, 
            name = "7-day rolling average", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dash"),
            hoverinfo = "text+x",
            text = ~paste("<b>7-day rolling avg.: </b>", round(confirmed_7day_avg, 2))) %>%
  add_trace(data = df_daily_national,
            y = ~confirmed_14day_avg, 
            name = "14-day rolling average", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dot"),
            hoverinfo = "text+x",
            text = ~paste("<b>14-day rolling avg.: </b>", round(confirmed_14day_avg, 2))) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Daily COVID-19 confirmed cases in ", my_country, 
                        "<br> as of ", max(df_epi_curve$report_date, na.rm = TRUE)),
         yaxis = list(title = "Absolute number of confirmed cases"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
  )

# daily absolute deaths plot per region
df_epi_curve %>%
  plot_ly(x = ~report_date) %>%
  add_bars(y = ~deaths_this_day, 
           color = ~resadmin1_correct_deaths, 
           legendgroup = ~resadmin1_correct_deaths,
           hoverinfo = "text+x", 
           text = ~paste0("<b>Deaths in ", resadmin1_correct_deaths, ": </b>", deaths_this_day)) %>%
  add_trace(data = df_daily_national,
            y = ~deaths_7day_avg, 
            name = "7-day rolling average", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dash"),
            hoverinfo = "text+x",
            text = ~paste("<b>7-day rolling avg.: </b>", round(deaths_7day_avg, 2))) %>%
  add_trace(data = df_daily_national,
            y = ~deaths_14day_avg, 
            name = "14-day rolling average", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dot"),
            hoverinfo = "text+x",
            text = ~paste("<b>14-day rolling avg.: </b>", round(deaths_14day_avg, 2))) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Daily COVID-19 deaths in ", my_country, 
                        "<br> as of ", max(df_epi_curve$report_date, na.rm = TRUE)),
         yaxis = list(title = "Absolute number of deaths"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
  )

# daily absolute discharges plot per region
df_epi_curve %>%
  plot_ly(x = ~report_date) %>%
  add_bars(y = ~discharges_this_day, 
           color = ~resadmin1_correct_discharges, 
           legendgroup = ~resadmin1_correct_discharges,
           hoverinfo = "text+x", 
           text = ~paste0("<b>Discharges in ", resadmin1_correct_discharges, ": </b>", discharges_this_day)) %>%
  add_trace(data = df_daily_national,
            y = ~discharges_7day_avg, 
            name = "7-day rolling average", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dash"),
            hoverinfo = "text+x",
            text = ~paste("<b>7-day rolling avg.: </b>", round(discharges_7day_avg, 2))) %>%
  add_trace(data = df_daily_national,
            y = ~discharges_14day_avg, 
            name = "14-day rolling average", 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", dash = "dot"),
            hoverinfo = "text+x",
            text = ~paste("<b>14-day rolling avg.: </b>", round(discharges_14day_avg, 2))) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Daily COVID-19 discharges in ", my_country, 
                        "<br> as of ", max(df_epi_curve$report_date, na.rm = TRUE)),
         yaxis = list(title = "Absolute number of discharges"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
  )

# daily relative reported cases plot per region
df_epi_curve %>%
  group_by(report_date) %>%
  mutate(reported_prop =  100 * reported_this_day / sum(reported_this_day)) %>%
  plot_ly(x = ~report_date) %>%
  add_bars(y = ~reported_prop, 
           color = ~resadmin1_correct_reported, 
           legendgroup = ~resadmin1_correct_reported,
           hoverinfo = "text+x", 
           text = ~paste0("<b>Reported cases (%) in ", resadmin1_correct_reported, ": </b>", 
                          round(reported_prop, 2), "%")) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Daily COVID-19 reported cases in ", my_country, 
                        "<br> as of ", max(df_epi_curve$report_date, na.rm = TRUE)),
         yaxis = list(title = "Relative number (%) of reported cases"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
  )

# daily relative confirmed cases plot per region
df_epi_curve %>%
  group_by(report_date) %>%
  mutate(confirmed_prop =  100 * confirmed_this_day / sum(confirmed_this_day)) %>%
  plot_ly(x = ~report_date) %>%
  add_bars(y = ~confirmed_prop, 
           color = ~resadmin1_correct_confirmed, 
           legendgroup = ~resadmin1_correct_confirmed,
           hoverinfo = "text+x", 
           text = ~paste0("<b>Confirmed cases (%) in ", resadmin1_correct_confirmed, ": </b>", 
                          round(confirmed_prop, 2), "%")) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Daily COVID-19 confirmed cases in ", my_country, 
                        "<br> as of ", max(df_epi_curve$report_date, na.rm = TRUE)),
         yaxis = list(title = "Relative number (%) of confirmed cases"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
  )

# daily relative deaths plot per region
df_epi_curve %>%
  group_by(report_date) %>%
  mutate(deaths_prop =  100 * deaths_this_day / sum(deaths_this_day),
         deaths_prop = replace_na(deaths_prop, 0)) %>%
  plot_ly(x = ~report_date) %>%
  add_bars(y = ~deaths_prop, 
           color = ~resadmin1_correct_deaths, 
           legendgroup = ~resadmin1_correct_deaths,
           hoverinfo = "text+x", 
           text = ~paste0("<b>Deaths (%) in ", resadmin1_correct_deaths, ": </b>", 
                          round(deaths_prop, 2), "%")) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Daily COVID-19 deaths in ", my_country, 
                        "<br> as of ", max(df_epi_curve$report_date, na.rm = TRUE)),
         yaxis = list(title = "Relative number (%) of deaths"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
  )

# daily relative discharges plot per region
df_epi_curve %>%
  group_by(report_date) %>%
  mutate(discharges_prop =  100 * discharges_this_day / sum(discharges_this_day),
         discharges_prop = replace_na(discharges_prop, 0)) %>%
  plot_ly(x = ~report_date) %>%
  add_bars(y = ~discharges_prop, 
           color = ~resadmin1_correct_discharges, 
           legendgroup = ~resadmin1_correct_discharges,
           hoverinfo = "text+x", 
           text = ~paste0("<b>Discharges (%) in ", resadmin1_correct_discharges, ": </b>", 
                          round(discharges_prop, 2), "%")) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Daily COVID-19 discharges in ", my_country, 
                        "<br> as of ", max(df_epi_curve$report_date, na.rm = TRUE)),
         yaxis = list(title = "Relative number (%) of discharges"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date"))
  )


#######################################################
##      FACETTED PLOT FOR REGIONS OF MY COUNTRY      ##
#######################################################

#################################################
##      Daily reported cases per region facet  ##
#################################################

# extracting regions of my_country for loop
regions_reported <- unique(df_daily$resadmin1_correct)

# creating a list to store all subplots
regions_reported_list <- list()

# creating a list to store the titles for all subplots
regions_reported_title <- list()

# loop to create one individual plot for each region of my_country
for (i in 1:length(regions_reported)) {
  # needed to avoid over duplication of legends (could not think of a better way to do this)
  show_legend <- if (i == 1) {TRUE} else {FALSE}
  
  # needed to add individual titles for each subplot 
  regions_reported_title[[i]] <- list(
    text = paste0("Daily reported cases in ", regions_reported[i]),
    # set to "paper" to standardize distances
    xref = "paper",
    yref = "paper",
    # anchoring and aligning to the center topmost area of each individual plot
    yanchor = "top",
    xanchor = "center",
    align = "center",
    # centralizing at x-axis and at the top of y-axis
    x = 0.5,
    y = 1,
    # removes annoying arrow
    showarrow = FALSE)
  
  # storing individual plots of each region in the list
  regions_reported_list[[i]] <- plot_ly(data = df_daily %>% filter(resadmin1_correct == regions_reported[i]),
                                        x = ~report_date,
                                        y = ~reported_log, 
                                        name = ~paste0("Reported cases (log)"),
                                        # use this to leave just one legend instead of several 
                                        # (one for each region)
                                        showlegend = show_legend,
                                        # standardizing all subplots to the same color
                                        color = I("darkgreen"),
                                        type = "bar",
                                        # needed to show both y-axis data and x-axis report date
                                        hoverinfo = "text+x", 
                                        # presenting real and log-transformed values
                                        text = ~paste0("<b>Reported cases in ", resadmin1_correct, ": </b>", 
                                                       reported_this_day,
                                                       "<br><b>Reported cases (log) in ", resadmin1_correct, ": </b>", 
                                                       round(reported_log, 2))) %>%
    # adding trend curve to the plot (returns real count and log transformed count)
    add_trace(data = df_daily %>% filter(resadmin1_correct == regions_reported[i]),
              y = ~reported_trend_log, 
              name = ~paste0("Reported cases trend (log)"),
              type = "scatter",
              mode = "lines",
              line = list(color = "black", dash = "dash"),
              hoverinfo = "text+x",
              text = ~paste0("<b>Reported cases trend: </b>", round(reported_trend, 2),
                             "<br><b>Reported cases trend (log): </b>", round(reported_trend_log, 2))) %>%
    # adding a marker to the plot that indicates peak count (returns real count and log transformed count)
    add_trace(data = df_daily %>% filter(resadmin1_correct == regions_reported[i],
                                         reported_log == max(reported_log, na.rm = TRUE)) %>%
                # keeping the marker only at the last peak (useful when there are more than one peak - important
                # when counts are low)
                slice_max(report_date),
              y = ~reported_log, 
              name = "Reported cases trend (log)",
              type = "scatter",
              mode = "markers",
              # making the marker easy to visualize
              marker = list(size = 5, color = "red", line = list(width = 2, color = "black")),
              hoverinfo = "text+x",
              text = ~paste0("<b>Date: </b>",report_date,
                             "<br><b>Peak of reported cases: </b>", reported_this_day,
                             "<br><b>Peak of reported cases (log): </b>", round(reported_log, 2))) %>%
    layout(hovermode = "x unified",
           # this is the title for the whole plot, not for each subplot
           title = paste0("Daily reported cases for each region in ", my_country),
           yaxis = list(title = "Reported cases (log-transformed)",
                        # making the log-transformed y-axis easier to read
                        range = c(0, max(df_daily$reported_log, na.rm = TRUE) + 1.2),
                        tickvals = c(0, 1, 2, 3, 4),
                        ticktext = c(0, 10, 100, 1000, 10000)),
           xaxis = list(title = "Date of Reporting",
                        type = "date",
                        tickformat = "%b %d (%a)"),
           # using the annotation feature to insert the individual title for each subplot
           annotations = regions_reported_title[[i]])
}

# plotting all regions of my_country as a unique facetted plot
subplot(regions_reported_list, nrows = 3, shareX = TRUE, shareY = TRUE) 


##################################################
##      Daily confirmed cases per region facet  ##
##################################################

# extracting regions of my_country for loop
regions_confirmed <- unique(df_daily$resadmin1_correct)

# creating a list to store all subplots
regions_confirmed_list <- list()

# creating a list to store the titles for all subplots
regions_confirmed_title <- list()


# loop to create one individual plot for each region of my_country
for (i in 1:length(regions_confirmed)) {
  # needed to avoid over duplication of legends (could not think of a better way to do this)
  show_legend <- if (i == 1) {TRUE} else {FALSE}
  
  # needed to add individual titles for each subplot 
  regions_confirmed_title[[i]] <- list(
    text = paste0("Daily confirmed cases in ", regions_confirmed[i]),
    # set to "paper" to standardize distances
    xref = "paper",
    yref = "paper",
    # anchoring and aligning to the center topmost area of each individual plot
    yanchor = "top",
    xanchor = "center",
    align = "center",
    # centralizing at x-axis and at the top of y-axis
    x = 0.5,
    y = 1,
    # removes annoying arrow
    showarrow = FALSE)
  
  # storing individual plots of each region in the list
  regions_confirmed_list[[i]] <- plot_ly(data = df_daily %>% filter(resadmin1_correct == regions_confirmed[i]),
                                         x = ~report_date,
                                         y = ~confirmed_log, 
                                         name = ~paste0("Confirmed cases (log)"),
                                         # use this to leave just one legend instead of several 
                                         # (one for each region)
                                         showlegend = show_legend,
                                         # standardizing all subplots to the same color
                                         color = I("darkgreen"),
                                         type = "bar",
                                         # needed to show both y-axis data and x-axis report date
                                         hoverinfo = "text+x", 
                                         # presenting real and log-transformed values
                                         text = ~paste0("<b>Confirmed cases in ", resadmin1_correct, ": </b>", 
                                                        confirmed_this_day,
                                                        "<br><b>Confirmed cases (log) in ", resadmin1_correct, ": </b>", 
                                                        round(confirmed_log, 2))) %>%
    # adding trend curve to the plot (returns real count and log transformed count)
    add_trace(data = df_daily %>% filter(resadmin1_correct == regions_confirmed[i]),
              y = ~confirmed_trend_log, 
              name = ~paste0("Confirmed cases trend (log)"),
              type = "scatter",
              mode = "lines",
              line = list(color = "black", dash = "dash"),
              hoverinfo = "text+x",
              text = ~paste0("<b>Confirmed cases trend: </b>", round(confirmed_trend, 2),
                             "<br><b>Confirmed cases trend (log): </b>", round(confirmed_trend_log, 2))) %>%
    # adding a marker to the plot that indicates peak count (returns real count and log transformed count)
    add_trace(data = df_daily %>% filter(resadmin1_correct == regions_confirmed[i],
                                         confirmed_log == max(confirmed_log, na.rm = TRUE)) %>%
                # keeping the marker only at the last peak (useful when there are more than one peak - important
                # when counts are low)
                slice_max(report_date),
              y = ~confirmed_log, 
              name = "Confirmed cases trend (log)",
              type = "scatter",
              mode = "markers",
              # making the marker easy to visualize
              marker = list(size = 5, color = "red", line = list(width = 2, color = "black")),
              hoverinfo = "text+x",
              text = ~paste0("<b>Date: </b>",report_date,
                             "<br>Peak of confirmed cases: </b>", confirmed_this_day,
                             "<br><b>Peak of confirmed cases (log): </b>", round(confirmed_log, 2))) %>%
    layout(hovermode = "x unified",
           # this is the title for the whole plot, not for each subplot
           title = paste0("Daily confirmed cases for each region in ", my_country),
           yaxis = list(title = "Confirmed cases (log-transformed)",
                        # making the log-transformed y-axis easier to read
                        range = c(0, max(df_daily$confirmed_log, na.rm = TRUE) + 1.2),
                        tickvals = c(0, 1, 2, 3, 4),
                        ticktext = c(0, 10, 100, 1000, 10000)),
           xaxis = list(title = "Date of Reporting",
                        type = "date",
                        tickformat = "%b %d (%a)"),
           # using the annotation feature to insert the individual title for each subplot
           annotations = regions_confirmed_title[[i]])
}

# plotting all regions of my_country as a unique facetted plot
subplot(regions_confirmed_list, nrows = 3, shareX = TRUE, shareY = TRUE) 


#########################################
##      Daily deaths per region facet  ##
#########################################

# extracting regions of my_country for loop
regions_deaths <- unique(df_daily$resadmin1_correct)

# creating a list to store all subplots
regions_deaths_list <- list()

# creating a list to store the titles for all subplots
regions_deaths_title <- list()


# loop to create one individual plot for each region of my_country
for (i in 1:length(regions_deaths)) {
  # needed to avoid over duplication of legends (could not think of a better way to do this)
  show_legend <- if (i == 1) {TRUE} else {FALSE}
  
  # needed to add individual titles for each subplot 
  regions_deaths_title[[i]] <- list(
    text = paste0("Daily deaths in ", regions_deaths[i]),
    # set to "paper" to standardize distances
    xref = "paper",
    yref = "paper",
    # anchoring and aligning to the center topmost area of each individual plot
    yanchor = "top",
    xanchor = "center",
    align = "center",
    # centralizing at x-axis and at the top of y-axis
    x = 0.5,
    y = 1,
    # removes annoying arrow
    showarrow = FALSE)
  
  # storing individual plots of each region in the list
  regions_deaths_list[[i]] <- plot_ly(data = df_daily %>% filter(resadmin1_correct == regions_deaths[i]),
                                      x = ~report_date,
                                      y = ~deaths_log, 
                                      name = ~paste0("Deaths (log)"),
                                      # use this to leave just one legend instead of several 
                                      # (one for each region)
                                      showlegend = show_legend,
                                      # standardizing all subplots to the same color
                                      color = I("darkgreen"),
                                      type = "bar",
                                      # needed to show both y-axis data and x-axis report date
                                      hoverinfo = "text+x", 
                                      # presenting real and log-transformed values
                                      text = ~paste0("<b>Deaths in ", resadmin1_correct, ": </b>", 
                                                     deaths_this_day,
                                                     "<br><b>Deaths (log) in ", resadmin1_correct, ": </b>", 
                                                     round(deaths_log, 2))) %>%
    # adding trend curve to the plot (returns real count and log transformed count)
    add_trace(data = df_daily %>% filter(resadmin1_correct == regions_deaths[i]),
              y = ~deaths_trend_log, 
              name = ~paste0("Deaths trend (log)"),
              type = "scatter",
              mode = "lines",
              line = list(color = "black", dash = "dash"),
              hoverinfo = "text+x",
              text = ~paste0("<b>Deaths trend: </b>", round(deaths_trend, 2),
                             "<br><b>Deaths trend (log): </b>", round(deaths_trend_log, 2))) %>%
    # adding a marker to the plot that indicates peak count (returns real count and log transformed count)
    add_trace(data = df_daily %>% filter(resadmin1_correct == regions_deaths[i],
                                         deaths_log == max(deaths_log, na.rm = TRUE)) %>%
                # keeping the marker only at the last peak (useful when there are more than one peak - important
                # when counts are low)
                slice_max(report_date),
              y = ~deaths_log, 
              name = "Deaths trend (log)",
              type = "scatter",
              mode = "markers",
              # making the marker easy to visualize
              marker = list(size = 5, color = "red", line = list(width = 2, color = "black")),
              hoverinfo = "text+x",
              text = ~paste0("<b>Date: </b>",report_date,
                             "<br>Peak of deaths: </b>", deaths_this_day,
                             "<br><b>Peak of deaths (log): </b>", round(deaths_log, 2))) %>%
    layout(hovermode = "x unified",
           # this is the title for the whole plot, not for each subplot
           title = paste0("Daily deaths for each region in ", my_country),
           yaxis = list(title = "Deaths (log-transformed)",
                        # making the log-transformed y-axis easier to read
                        range = c(0, max(df_daily$deaths_log, na.rm = TRUE) + 1.2),
                        tickvals = c(0, 1, 2, 3, 4),
                        ticktext = c(0, 10, 100, 1000, 10000)),
           xaxis = list(title = "Date of Reporting",
                        type = "date",
                        tickformat = "%b %d (%a)"),
           # using the annotation feature to insert the individual title for each subplot
           annotations = regions_deaths_title[[i]])
}

# plotting all regions of my_country as a unique facetted plot
subplot(regions_deaths_list, nrows = 3, shareX = TRUE, shareY = TRUE) 


#############################################
##      Daily discharges per region facet  ##
#############################################

# extracting regions of my_country for loop
regions_discharges <- unique(df_daily$resadmin1_correct)

# creating a list to store all subplots
regions_discharges_list <- list()

# creating a list to store the titles for all subplots
regions_discharges_title <- list()


# loop to create one individual plot for each region of my_country
for (i in 1:length(regions_discharges)) {
  # needed to avoid over duplication of legends (could not think of a better way to do this)
  show_legend <- if (i == 1) {TRUE} else {FALSE}
  
  # needed to add individual titles for each subplot 
  regions_discharges_title[[i]] <- list(
    text = paste0("Daily discharges in ", regions_discharges[i]),
    # set to "paper" to standardize distances
    xref = "paper",
    yref = "paper",
    # anchoring and aligning to the center topmost area of each individual plot
    yanchor = "top",
    xanchor = "center",
    align = "center",
    # centralizing at x-axis and at the top of y-axis
    x = 0.5,
    y = 1,
    # removes annoying arrow
    showarrow = FALSE)
  
  # storing individual plots of each region in the list
  regions_discharges_list[[i]] <- plot_ly(data = df_daily %>% filter(resadmin1_correct == regions_discharges[i]),
                                          x = ~report_date,
                                          y = ~discharges_log, 
                                          # standardizing all subplots to the same color
                                          color = I("darkgreen"),
                                          name = ~paste0("Discharges (log)"),
                                          # use this to leave just one legend instead of several 
                                          # (one for each region)
                                          showlegend = show_legend,
                                          type = "bar",
                                          # needed to show both y-axis data and x-axis report date
                                          hoverinfo = "text+x", 
                                          # presenting real and log-transformed values
                                          text = ~paste0("<b>Discharges in ", resadmin1_correct, ": </b>", 
                                                         discharges_this_day,
                                                         "<br><b>Discharges (log) in ", resadmin1_correct, ": </b>", 
                                                         round(discharges_log, 2))) %>%
    # adding trend curve to the plot (returns real count and log transformed count)
    add_trace(data = df_daily %>% filter(resadmin1_correct == regions_discharges[i]),
              y = ~discharges_trend_log, 
              name = ~paste0("Discharges trend (log)"),
              type = "scatter",
              mode = "lines",
              line = list(color = "black", dash = "dash"),
              hoverinfo = "text+x",
              text = ~paste0("<b>Discharges trend: </b>", round(discharges_trend, 2),
                             "<br><b>Discharges trend (log): </b>", round(discharges_trend_log, 2))) %>%
    # adding a marker to the plot that indicates peak count (returns real count and log transformed count)
    add_trace(data = df_daily %>% 
                filter(resadmin1_correct == regions_discharges[i],
                       discharges_log == max(discharges_log, na.rm = TRUE)) %>%
                # keeping the marker only at the last peak (useful when there are more than one peak - important
                # when counts are low)
                slice_max(report_date),
              y = ~discharges_log, 
              name = "Discharges trend (log)",
              type = "scatter",
              mode = "markers",
              # making the marker easy to visualize
              marker = list(size = 5, color = "red", line = list(width = 2, color = "black")),
              hoverinfo = "text+x",
              text = ~paste0("<b>Date: </b>",report_date,
                             "<br><b>Peak of discharges: </b>", discharges_this_day,
                             "<br><b>Peak of discharges (log): </b>", round(discharges_log, 2))) %>%
    layout(hovermode = "x unified",
           # this is the title for the whole plot, not for each subplot
           title = paste0("Daily discharges for each region in ", my_country),
           yaxis = list(title = "Discharges (log-transformed)",
                        # making the log-transformed y-axis easier to read
                        range = c(0, max(df_daily$discharges_log, na.rm = TRUE) + 1.2),
                        tickvals = c(0, 1, 2, 3, 4),
                        ticktext = c(0, 10, 100, 1000, 10000)),
           xaxis = list(title = "Date of Reporting",
                        type = "date",
                        tickformat = "%b %d (%a)"),
           # using the annotation feature to insert the individual title for each subplot
           annotations = regions_discharges_title[[i]])
}

# plotting all regions of my_country as a unique facetted plot
subplot(regions_discharges_list, nrows = 3, shareX = TRUE, shareY = TRUE) 


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

# interactive time series plot: reported cases
df_daily %>% 
  filter(resadmin1_correct != "No Info") %>%
  plot_ly(x = ~report_date) %>%
  add_lines(y = ~cum_reported, 
            color = ~resadmin1_correct,
            colors = mycolors_2,
            hoverinfo = "text+x", 
            # presenting reported cases info
            text = ~paste0("<b>Region: </b>", resadmin1_correct,
                           "<br><b>Reported cases: </b>", cum_reported,
                           "<br><b>Incidence of reported cases: </b>", paste0(round(incidence_reported, 2), 
                                                                              " per 100,000"))) %>%
  layout(hovermode = "unified x",
         title = paste0("Reported cases across regions in ", my_country),
         yaxis = list(title = "Cumulative number of reported cases"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")
         )
  )

# interactive time series plot: confirmed cases
df_daily %>% 
  filter(resadmin1_correct != "No Info") %>%
  plot_ly(x = ~report_date) %>%
  add_lines(y = ~cum_confirmed, 
            color = ~resadmin1_correct,
            colors = mycolors_2,
            hoverinfo = "text+x", 
            # presenting confirmed cases info
            text = ~paste0("<b>Region: </b>", resadmin1_correct,
                           "<br><b>Confirmed cases: </b>", cum_confirmed,
                           "<br><b>Incidence of confirmed cases: </b>", paste0(round(incidence_confirmed, 2), 
                                                                               " per 100,000"))) %>%
  layout(hovermode = "unified x",
         title = paste0("Confirmed cases across regions in ", my_country),
         yaxis = list(title = "Cumulative number of confirmed cases"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")
         )
  )

# interactive time series plot: deaths
df_daily %>% 
  filter(resadmin1_correct != "No Info") %>%
  plot_ly(x = ~report_date) %>%
  add_lines(y = ~cum_deaths, 
            color = ~resadmin1_correct,
            colors = mycolors_2,
            hoverinfo = "text+x", 
            # presenting deaths cases info
            text = ~paste0("<b>Region: </b>", resadmin1_correct,
                           "<br><b>Deaths: </b>", cum_deaths,
                           "<br><b>Mortality: </b>", paste0(round(mortality, 2), 
                                                            " per 100,000"))) %>%
  layout(hovermode = "unified x",
         title = paste0("Deaths across regions in ", my_country),
         yaxis = list(title = "Cumulative number of deaths cases"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")
         )
  )

# interactive time series plot: incidence of reported cases
df_daily %>% 
  filter(resadmin1_correct != "No Info") %>%
  plot_ly(x = ~report_date) %>%
  add_lines(y = ~incidence_reported, 
            color = ~resadmin1_correct,
            colors = mycolors_2,
            hoverinfo = "text+x", 
            # presenting reported cases info
            text = ~paste0("<b>Region: </b>", resadmin1_correct,
                           "<br><b>Reported cases: </b>", cum_reported,
                           "<br><b>Incidence of reported cases: </b>", paste0(round(incidence_reported, 2), 
                                                                              " per 100,000"))) %>%
  layout(hovermode = "unified x",
         title = paste0("Incidence of reported cases across regions in ", my_country),
         yaxis = list(title = "Incidence of reported cases per 100,000"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")
         )
  )

# interactive time series plot: incidence of confirmed cases
df_daily %>% 
  filter(resadmin1_correct != "No Info") %>%
  plot_ly(x = ~report_date) %>%
  add_lines(y = ~incidence_confirmed, 
            color = ~resadmin1_correct,
            colors = mycolors_2,
            hoverinfo = "text+x", 
            # presenting confirmed cases info
            text = ~paste0("<b>Region: </b>", resadmin1_correct,
                           "<br><b>Confirmed cases: </b>", cum_confirmed,
                           "<br><b>Incidence of confirmed cases: </b>", paste0(round(incidence_confirmed, 2), 
                                                                               " per 100,000"))) %>%
  layout(hovermode = "unified x",
         title = paste0("Incidence of confirmed cases across regions in ", my_country),
         yaxis = list(title = "Incidence of confirmed cases per 100,000"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")
         )
  )

# interactive time series plot: mortality
df_daily %>% 
  filter(resadmin1_correct != "No Info") %>%
  plot_ly(x = ~report_date) %>%
  add_lines(y = ~mortality, 
            color = ~resadmin1_correct,
            colors = mycolors_2,
            hoverinfo = "text+x", 
            # presenting confirmed cases info
            text = ~paste0("<b>Region: </b>", resadmin1_correct,
                           "<br><b>Deaths: </b>", cum_deaths,
                           "<br><b>Mortality: </b>", paste0(round(mortality, 2), 
                                                            " per 100,000"))) %>%
  layout(hovermode = "unified x",
         title = paste0("Mortality across regions in ", my_country),
         yaxis = list(title = "Mortality per 100,000"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")
         )
  )


######################################################
######################################################
##                                                  ##
##      5 NATIONAL WEEKLY GROWTH RATE OVER TIME     ##
##                                                  ##
##              GROWTH RATES SECTION                ##
##                                                  ##
######################################################
######################################################

###############################################
##      Growth rate of  cases and deaths     ##
###############################################

# growth rate of reported cases df
df_growth_rate <- df_daily_national %>% 
  mutate(epiweek = MMWRweek(report_date)$MMWRweek,
         reported_past_7 = rollsum(reported_this_day, 7, 
                                   fill = na.fill(reported_this_day, 0)),
         confirmed_past_7 = rollsum(confirmed_this_day, 7, 
                                    fill = na.fill(confirmed_this_day, 0)),
         deaths_past_7 = rollsum(deaths_this_day, 7, 
                                 fill = na.fill(deaths_this_day, 0))) %>% 
  select(report_date, epiweek, reported_past_7:deaths_past_7) %>% 
  group_by(epiweek) %>% 
  # take the last day of each epiweek
  slice(which.max(report_date)) %>% 
  ungroup() %>% 
  # reported cases in the past week vs cases two weeks ago
  mutate(diff_reported = reported_past_7 - lag(reported_past_7, 1), 
         week_growth_reported = diff_reported / lag(reported_past_7, 1),
         week_growth_reported_perc = 100 * week_growth_reported, 
         # formula to convert weeklygrowth to daily growth equivalent
         growth_reported = (((week_growth_reported + 1) ^ (1/7)) - 1), 
         growth_reported_perc = growth_reported * 100,
         # confirmed cases in the past week vs cases two weeks ago
         diff_confirmed = confirmed_past_7 - lag(confirmed_past_7, 1), 
         week_growth_confirmed = diff_confirmed / lag(confirmed_past_7, 1),
         week_growth_confirmed_perc = 100 * week_growth_confirmed, 
         growth_confirmed = (((week_growth_confirmed + 1) ^ (1/7)) - 1), 
         growth_confirmed_perc = growth_confirmed * 100,
         # deaths in the past week vs cases two weeks ago
         diff_deaths = deaths_past_7 - lag(deaths_past_7, 1), 
         week_growth_deaths = diff_deaths / lag(deaths_past_7, 1),
         week_growth_deaths_perc = 100 * week_growth_deaths, 
         growth_deaths = (((week_growth_deaths + 1) ^ (1/7)) - 1), 
         growth_deaths_perc = growth_deaths * 100)

# growth rate of reported cases plot
df_growth_rate %>% 
  plot_ly(x = ~report_date)%>%
  add_ribbons(ymin = 0,
              ymax = max(df_growth_rate$week_growth_reported_perc[df_growth_rate$week_growth_reported_perc != Inf], 
                         na.rm = TRUE),
              color = I("red"),
              opacity = 0.5,
              hoverinfo = "none",
              showlegend = FALSE,
              line = list(color = "rgba(0, 0, 0, 0)")) %>%
  add_ribbons(ymin = min(df_growth_rate$week_growth_reported_perc[df_growth_rate$week_growth_reported_perc != Inf], 
                         na.rm = TRUE),
              ymax = 0,
              color = I("green"),
              opacity = 0.5,
              hoverinfo = "none",
              showlegend = FALSE, 
              line = list(color = "rgba(0, 0, 0, 0)")) %>%
  add_trace(y = ~week_growth_reported_perc, 
            type = "scatter", # configuring trace as scatterplot
            mode = "markers+lines", # lines + points
            color = I("black"), # black line
            hoverinfo = "text+x", 
            text = ~paste0("<b>Date of reporting: </b>", report_date,
                           "<br><b>Epidemiological week: </b>", epiweek,
                           "<br><b>Weekly growth rate: </b>", paste0(round(week_growth_reported_perc, 2), "%"))) %>%
  layout(hovermode = "unified x",
         title = paste0("Week-on-week growth rate of new COVID-19 reported cases in ", my_country),
         yaxis = list(title = "Average daily growth rate (%) each week"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")))

# growth rate of confirmed cases plot
df_growth_rate %>% 
  plot_ly(x = ~report_date)%>%
  add_ribbons(ymin = 0,
              ymax = max(df_growth_rate$week_growth_confirmed_perc[df_growth_rate$week_growth_confirmed_perc != Inf], 
                         na.rm = TRUE),
              color = I("red"),
              opacity = 0.8,
              hoverinfo = "none",
              showlegend = FALSE) %>%
  add_ribbons(ymin = min(df_growth_rate$week_growth_confirmed_perc[df_growth_rate$week_growth_confirmed_perc != Inf], 
                         na.rm = TRUE),
              ymax = 0,
              color = I("green"),
              opacity = 0.8,
              hoverinfo = "none",
              showlegend = FALSE) %>%
  add_trace(y = ~week_growth_confirmed_perc, 
            type = "scatter",
            mode = "markers+lines",
            color = I("black"),
            hoverinfo = "text", 
            text = ~paste0("<b>Date of reporting: </b>", report_date,
                           "<br><b>Epidemiological week: </b>", epiweek,
                           "<br><b>Weekly growth rate: </b>", paste0(round(week_growth_confirmed_perc, 2), "%"))) %>%
  layout(hovermode = "unified x",
         title = paste0("Week-on-week growth rate of new COVID-19 confirmed cases in ", my_country),
         yaxis = list(title = "Average daily growth rate (%) each week"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")))

# growth rate of deaths plot
df_growth_rate %>% 
  plot_ly(x = ~report_date)%>%
  add_ribbons(ymin = 0,
              ymax = max(df_growth_rate$week_growth_deaths_perc[df_growth_rate$week_growth_deaths_perc != Inf], 
                         na.rm = TRUE),
              color = I("red"),
              opacity = 0.8,
              hoverinfo = "none",
              showlegend = FALSE) %>%
  add_ribbons(ymin = min(df_growth_rate$week_growth_deaths_perc[df_growth_rate$week_growth_deaths_perc != Inf], 
                         na.rm = TRUE),
              ymax = 0,
              color = I("green"),
              opacity = 0.8,
              hoverinfo = "none",
              showlegend = FALSE) %>%
  add_trace(y = ~week_growth_deaths_perc, 
            type = "scatter",
            mode = "markers+lines",
            color = I("black"),
            hoverinfo = "text", 
            text = ~paste0("<b>Date of reporting: </b>", report_date,
                           "<br><b>Epidemiological week: </b>", epiweek,
                           "<br><b>Weekly growth rate: </b>", paste0(round(week_growth_deaths_perc, 2), "%"))) %>%
  layout(hovermode = "unified x",
         title = paste0("Week-on-week growth rate of new COVID-19 deaths in ", my_country),
         yaxis = list(title = "Average daily growth rate (%) each week"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")))


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
all_dates_origin <- df2 %>%
  filter(!is.na(expo_travel)) %>%
  filter(is_reported == 1) %>%
  complete(report_date = seq.Date(min(report_date, na.rm = TRUE), 
                                  max(report_date, na.rm = TRUE), by = "day")) %>%
  tidyr::expand(report_date, expo_travel, resadmin1_correct) %>%
  filter(!is.na(expo_travel))

# Calculate daily reported cases
reported_travel_hist <- df2 %>%
  # filtering out missing data 
  filter(!is.na(expo_travel)) %>%
  # summarise the number of reported each day in each region
  group_by(resadmin1_correct, report_date, expo_travel) %>%
  summarise(reported_this_day = sum(is_reported, na.rm = T)) %>%
  # add in new rows so that all dates are covered
  full_join(all_dates_origin) %>%
  arrange(report_date, resadmin1_correct)

# Calculate daily confirmed cases
confirmed_travel_hist <-  df2 %>%
  # filtering out missing data 
  filter(!is.na(expo_travel)) %>%
  # summarise the number of confirmed cases each day in each region
  group_by(resadmin1_correct, report_date, expo_travel) %>%
  summarise(confirmed_this_day = sum(is_reported[report_classif == "CONFIRMED"], na.rm = T)) %>%
  # add in new rows so that all dates are covered
  full_join(all_dates_origin) %>%
  arrange(report_date, resadmin1_correct)

# Calculate daily deaths
deaths_travel_hist <- df2 %>%
  # filtering out missing data 
  filter(!is.na(expo_travel)) %>%
  # summarise the number of deaths each day in each region
  group_by(resadmin1_correct, report_date, expo_travel) %>%
  summarise(deaths_this_day = sum(is_reported[patcourse_status == "DEAD"], na.rm = T)) %>%
  # add in new rows so that all dates are covered
  full_join(all_dates_origin) %>%
  arrange(report_date, resadmin1_correct)

# Calculate daily discharges as above
discharges_travel_hist <- df2 %>% 
  # filtering out missing data 
  filter(!is.na(expo_travel)) %>%
  # summarise the number of deaths each day in each region
  group_by(resadmin1_correct, report_date, expo_travel) %>%
  summarise(discharges_this_day = sum(is_reported[!is.na(patcourse_datedischarge)], na.rm = T)) %>%
  # add in new rows so that all dates are covered
  full_join(all_dates_origin) %>%
  arrange(report_date, resadmin1_correct)

# national epicurve per travel history
df_origin_reported <- inner_join(reported_travel_hist, confirmed_travel_hist,
                                 by= c("report_date", "resadmin1_correct", "expo_travel"))  %>% 
  right_join(deaths_travel_hist,
             by= c("report_date", "resadmin1_correct", "expo_travel")) %>% 
  left_join(discharges_travel_hist,
            by= c("report_date", "resadmin1_correct", "expo_travel")) %>%
  # grouping and summarising national level data
  group_by(expo_travel, report_date) %>%
  summarise(reported_this_day = sum(reported_this_day, na.rm = TRUE),
            confirmed_this_day = sum(confirmed_this_day, na.rm = TRUE),
            deaths_this_day = sum(deaths_this_day, na.rm = TRUE),
            discharges_this_day = sum(discharges_this_day, na.rm = TRUE)) %>%
  group_by(report_date) %>%
  # creating relative counts
  mutate(reported_prop = round((reported_this_day/ sum(reported_this_day) * 100), 1),
         confirmed_prop = round((confirmed_this_day/ sum(confirmed_this_day) * 100), 1),
         deaths_prop = round((deaths_this_day/ sum(deaths_this_day) * 100), 1),
         discharges_prop = round((discharges_this_day/ sum(discharges_this_day) * 100), 1)) %>%
  # cases past week, to smooth out the curve
  # slightly less honest to the data, but yields better insights.
  group_by(expo_travel) %>%
  mutate(reported_trend = rollmean(x = reported_this_day, k = 7, align = "right",  
                                   fill = na.fill(reported_this_day, NA)),
         confirmed_trend = rollmean(x = confirmed_this_day, k = 7, align = "right",  
                                    fill = na.fill(confirmed_this_day, NA)),
         deaths_trend = rollmean(x = deaths_this_day, k = 7, align = "right",  
                                 fill = na.fill(deaths_this_day, NA)),
         discharges_trend = rollmean(x = discharges_this_day, k = 7, align = "right",  
                                     fill = na.fill(discharges_this_day, NA))) %>%
  # grouping by report date
  group_by(report_date) %>%
  # defining past week proportion of cases/deaths/discharges
  mutate(reported_past_week_prop = round((reported_trend/ sum(reported_trend) * 100), 1),
         confirmed_past_week_prop = round((confirmed_trend/ sum(confirmed_trend) * 100), 1),
         deaths_past_week_prop = round((deaths_trend/ sum(deaths_trend) * 100), 1),
         discharges_past_week_prop = round((discharges_trend/ sum(discharges_trend) * 100), 1),
         expo_travel = fct_recode(expo_travel,
                                  "Positive travel history" = "Y",
                                  "Negative travel history" = "N"))

# interactive plot of absolute reported cases vs travel history
df_origin_reported %>%
  plot_ly(x = ~report_date,
          y = ~reported_this_day, 
          type = "bar",
          color = ~expo_travel, 
          colors = mycolors_2,
          legendgroup = ~expo_travel,
          hoverinfo = "text+x",
          text = ~paste0("<b>Date of reporting: </b>", report_date,
                         "<br><b>Reported cases: </b>", reported_this_day)) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Classification of new reported cases by travel history over time in ", my_country),
         yaxis = list(title = "Absolute number of reported cases"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")))

# interactive plot of absolute confirmed cases vs travel history
df_origin_reported %>%
  plot_ly(x = ~report_date,
          y = ~confirmed_this_day, 
          type = "bar",
          color = ~expo_travel, 
          colors = mycolors_2,
          legendgroup = ~expo_travel,
          hoverinfo = "text+x",
          text = ~paste0("<b>Date of reporting: </b>", report_date,
                         "<br><b>Confirmed cases: </b>", confirmed_this_day)) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Classification of new confirmed cases by travel history over time in ", my_country),
         yaxis = list(title = "Absolute number of confirmed cases"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")))

# interactive plot of absolute deaths vs travel history
df_origin_reported %>%
  plot_ly(x = ~report_date,
          y = ~deaths_this_day, 
          type = "bar",
          color = ~expo_travel, 
          colors = mycolors_2,
          legendgroup = ~expo_travel,
          hoverinfo = "text+x",
          text = ~paste0("<b>Date of reporting: </b>", report_date,
                         "<br><b>Deaths: </b>", deaths_this_day)) %>%
  layout(barmode = "stack",
         hovermode = "x unified",
         title = paste0("Classification of new deaths by travel history over time in ", my_country),
         yaxis = list(title = "Absolute number of deaths"),
         xaxis = list(title = "Date of Reporting",
                      type = "date",
                      tickformat = "%b %d (%a)",
                      rangeslider = list(type = "date")))


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
  mutate(age_group = cut(as.numeric(patinfo_ageonset_years), 
                         breaks = c(0, 5, 9, 19, 29, 39, 49, 59, 69, 79, Inf),
                         labels = c("< 5", "5-9", "10-19", "20-29", "30-39", "40-49",
                                    "50-59", "60-69", "70-79", "> 80"), 
                         right = TRUE)) %>% 
  # for each age group and sex, sum the number of cases and the number of deaths
  group_by(age_group, patinfo_sex) %>%
  summarise(reported = sum(is_reported, na.rm = TRUE),
            confirmed = sum(is_reported[report_classif == "CONFIRMED"], na.rm = TRUE),
            deaths = sum(is_reported[patcourse_status == "DEAD"], na.rm = TRUE)) %>%
  filter(!is.na(age_group)) %>%
  ungroup()

# long format for the stacked bar chart
df_age_sex_long <-  df_age_sex %>% 
  # subtract out deaths from reported count to get CASES ALONE 
  # needed since we're going to build a STACKED bar chart.
  mutate(`Reported cases` = reported - deaths,
         `Confirmed cases` = confirmed - deaths,
         Deaths = deaths,
         patinfo_sex = recode_factor(patinfo_sex,
                                     "M" = "Male",
                                     "F" = "Female")) %>% 
  pivot_longer(names_to = "classification", cols = c(`Reported cases`, 
                                                     `Confirmed cases`, 
                                                     Deaths)) %>% 
  mutate(classification = fct_relevel(classification, c("Reported cases", 
                                                        "Confirmed cases", 
                                                        "Deaths")),
         # in order for the pyramid to be correctly displayed, one of the groups should be negative
         # we will hack the axis later to make it the absolute number
         value = if_else(patinfo_sex == "Female", value * (-1), value),
         # value to be passed to hoverinfo in plotly
         text_value = paste0("<b>", classification, ": </b>", abs(value)),
         # creating a variable with the absolute number of reported cases
         color_info = paste0(patinfo_sex, " ", classification),
         color_info = fct_relevel(color_info, c("Female Reported cases", "Female Confirmed cases", "Female Deaths",
                                                "Male Reported cases", "Male Confirmed cases","Male Deaths")),
         # calculating CFR
         CFR_reported = round(deaths / reported * 100, 2),
         CFR_confirmed = round(deaths / confirmed * 100, 2))

#########################################
##      Pyramid for reported cases     ##
#########################################

# creating object to make the x-axis readable (no negative values) and that maintains itself after zooming out
# how many digits the higher count of reported cases have?
digits_reported <- (nchar(max(df_age_sex$reported)) - 1) * (-1)

# next, we round it and arbitrarily divide it by 5, so that visualization and interpretation is still good
round_reported <- round(max(df_age_sex$reported), digits = digits_reported) / 5

# age-sex pyramid plot of reported cases
df_age_sex_long %>%
  filter(classification != "Confirmed cases") %>%
  plot_ly(x = ~value, # inverting x axis
          y = ~age_group, # inverting x axis
          color = ~color_info,
          colors = c("Female Reported cases" = "#66C2A5",
                     "Female Deaths" = "red",
                     "Male Reported cases" = "#8DA0CB",
                     "Male Deaths" = "red"),
          customdata = ~text_value,
          hoverinfo = "text",
          text = ~paste0("<b>Sex: </b>", patinfo_sex,
                         "<br><b>Age group: </b>", age_group,
                         "<br>", text_value,
                         "<br><b>CFR (based on reported cases): </b>", CFR_reported, "%")) %>%
  # changing orientations to horizontal
  add_bars(orientation = "h") %>%
  layout(bargap = 0.1,
         # needed to make bars correctly placed
         barmode = "relative",
         title = paste0("Age-sex distribution of all COVID-19 reported cases in ", my_country),
         yaxis = list(title = "Age group"),
         xaxis = list(title = "COVID-19 reported cases and deaths",
                      # changing axis ticks and making it absolute counts
                      tickmode = "array",
                      # creating a sequence of values large enough to acomodate a zoom out
                      tickvals = seq(-round_reported * 10, round_reported * 10, round_reported),
                      # adding correct (positive) counts at axis ticks
                      ticktext = c(seq(-round_reported * 10, 0, round_reported) * -1,
                                   seq(round_reported, round_reported * 10, round_reported))))


##########################################
##      Pyramid for confirmed cases     ##
##########################################

# creating object to make the x-axis readable (no negative values) and that maintains itself after zooming out
# how many digits the higher count of confirmed cases have?
digits_confirmed <- (nchar(max(df_age_sex$confirmed)) - 1) * (-1)

# next, we round it and arbitrarily divide it by 5, so that visualization and interpretation is still good
round_confirmed <- round(max(df_age_sex$confirmed), digits = digits_confirmed) / 5

# age-sex pyramid plot of confirmed cases
df_age_sex_long %>%
  filter(classification != "Reported cases") %>%
  plot_ly(x = ~value, # inverting x axis
          y = ~age_group, # inverting x axis
          color = ~color_info,
          colors = c("Female Confirmed cases" = "#66C2A5",
                     "Female Deaths" = "red",
                     "Male Confirmed cases" = "#8DA0CB",
                     "Male Deaths" = "red"),
          customdata = ~text_value,
          hoverinfo = "text",
          text = ~paste0("<b>Sex: </b>", patinfo_sex,
                         "<br><b>Age group: </b>", age_group,
                         "<br>", text_value,
                         "<br><b>CFR (based on confirmed cases): </b>", CFR_confirmed, "%")) %>%
  # changing orientations to horizontal
  add_bars(orientation = "h") %>%
  layout(bargap = 0.1,
         # needed to make bars correctly placed
         barmode = "relative",
         title = paste0("Age-sex distribution of all COVID-19 confirmed cases in ", my_country),
         yaxis = list(title = "Age group"),
         xaxis = list(title = "COVID-19 confirmed cases and deaths",
                      # changing axis ticks and making it absolute counts
                      tickmode = "array",
                      # creating a sequence of values large enough to acomodate a zoom out
                      tickvals = seq(-round_confirmed * 10, round_confirmed * 10, round_confirmed),
                      # adding correct (positive) counts at axis ticks
                      ticktext = c(seq(-round_confirmed * 10, 0, round_confirmed) * -1,
                                   seq(round_confirmed, round_confirmed * 10, round_confirmed))))


###############################
###############################
##                           ##
##      8 COMORBITIES        ##
##                           ##
##          SECTION          ##
##                           ##
###############################
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

# interactive bar plot of most common comorbities
df_comorb %>%
  plot_ly(x = ~n, # inverting x axis
          y = ~reorder(condition, n), # inverting x axis
          hoverinfo = "text",
          text = ~paste0("<b>Number of cases with ", condition, ": </b>", n)) %>%
  # changing orientations to horizontal
  add_bars(orientation = "h") %>%
  layout(title = paste0("Most common comorbidities reported by hospitalised patients in ", my_country),
         yaxis = list(title = "Comorbity"),
         xaxis = list(title = "Number of cases"))


