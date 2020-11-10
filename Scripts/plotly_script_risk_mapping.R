
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
                "leaflet", # for interactive maps
                "plotly", # for interactive plots
                "tidyverse"), # for everything, comes at the end to prevent masking of crucial functions
       update = FALSE)

##############################################
##                                          ##
##      IMPORTING RISK MAPPING RESULTS      ##
##                                          ##
##############################################

########################################################################################################
##      creating an object with country names and iso_codes to facilitate results file importing      ##
########################################################################################################

africa <- subset(rnaturalearthdata::countries50, 
                 region_un == "Africa" & type == "Sovereign country") %>%
  # removing geometries
  as_tibble() %>%
  # selecting only countries and iso_code
  select("admin", "adm0_a3")

# selecting correct iso_code
iso_code_aux <- africa %>%
  filter(admin %in% my_country) %>%
  pull()

##################################################
##      Importing risk mapping result files     ##
##################################################

# importing risk mapping result files: Mortality Risk Index for admin level 1
df_MRI_adm01 <- read.csv(paste0(iso_code_aux, "_adm01_MRI.csv")) %>%
  mutate(Region = str_to_title(Region)) %>%
  as_tibble()

# importing risk mapping result files: Transmission Risk Index for admin level 1
df_TRI_adm01 <- read.csv(paste0(iso_code_aux, "_adm01_TRI.csv")) %>%
  mutate(patinfo_resadmin1 = str_to_title(patinfo_resadmin1)) %>%
  as_tibble()

# importing risk mapping result files: Transmission Risk Index for admin level 2
# PS: Not all countries have results for admin level 2!!!!
df_TRI_adm02 <- read.csv(paste0(iso_code_aux, "_adm02_TRI.csv")) %>%
  mutate(patinfo_resadmin1 = str_to_title(patinfo_resadmin1),
         patinfo_resadmin2 = str_to_title(patinfo_resadmin2)) %>%
  as_tibble()

############################################################################
##      standardizing the admin lvl 1 names in the risk mapping file      ##
############################################################################

# testing if the resadmin1 names in the risk mapping file are correct or not
risk_incorrect_admin1 <- df_TRI_adm01$patinfo_resadmin1 %in% str_to_title(dict$X.U.FEFF.incorrect)[dict$admin_lvl == 1]

# Loop to test individually if each patinfo_resadmin1 entry is correct or not
df_TRI_adm01$resadmin1_correct <- NA_character_

for(i in seq_along(df_TRI_adm01$patinfo_resadmin1)){
  if(risk_incorrect_admin1[i] == TRUE) {
    index <- str_to_title(dict$X.U.FEFF.incorrect) %in% df_TRI_adm01$patinfo_resadmin1[i]
    df_TRI_adm01$resadmin1_correct[i] <- dict$correct[dict$admin_lvl == 1 & index == TRUE]
  }
}

######################################################################
##      Merging my_country daily gpkg with risk mapping results     ##
######################################################################

# a spatial df for the Mortality Risk Index (only for admin lvl 1)
df_risk_MRI_1 <- df_gpkg %>%
  st_as_sf() %>%
  # because of the several different languages that NAME_1 can have, let's force an UTF-8 encoding
  mutate(NAME_1 = stringi::stri_encode(NAME_1, from = "UTF-8", to = "UTF-8"),
         # now we can remove any accents from the variables, and merging will become easier
         NAME_1 = stringi::stri_trans_general(str = NAME_1, id = "Any-ASCII")) %>%
  # joining gpkg with MRI results for admin lvl 1
  full_join(df_MRI_adm01, by = c("NAME_1" = "Region"))

# a daily spatial df for the Transmission Risk Index (for admin lvl 1)
df_risk_TRI_1 <- df_gpkg %>%
  st_as_sf() %>%
  # because of the several different languages that NAME_1 can have accross countries, let's force an UTF-8 encoding
  mutate(NAME_1 = stringi::stri_encode(NAME_1, from = "UTF-8", to = "UTF-8")) %>%
  # joining gpkg with MRI results for admin lvl 1
  full_join(df_TRI_adm01, by = c("NAME_1" = "resadmin1_correct"))

######################################################################
##      Creating breaks and palletes for risk mapping leaflets      ##
######################################################################

# breaks and palletes for Transmission Risk Indexes
df_risk_TRI_1 <- df_risk_TRI_1 %>%
  mutate(TRI_RIDX_quintile = cut(TRI_RIDX, 
                                 unique(quantile(df_risk_TRI_1$TRI_RIDX, 
                                                 probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                 include.lowest = TRUE),
         TRI_IDX_quintile = cut(TRI_IDX, 
                                unique(quantile(df_risk_TRI_1$TRI_IDX, 
                                                probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                include.lowest = TRUE),
         TRI_IDX2_quintile = cut(TRI_IDX2, 
                                 unique(quantile(df_risk_TRI_1$TRI_IDX2, 
                                                 probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                 include.lowest = TRUE))

# creating palletes for Transmission Risk Indexes
pallete.TRI_RIDX <- colorFactor(palette = "YlOrRd", df_risk_TRI_1$TRI_RIDX_quintile)
pallete.TRI_IDX <- colorFactor(palette = "YlOrRd", df_risk_TRI_1$TRI_IDX_quintile)
pallete.TRI_IDX2 <- colorFactor(palette = "YlOrRd", df_risk_TRI_1$TRI_IDX2_quintile)


# breaks and palletes for Mortality Risk Indexes
df_risk_MRI_1 <- df_risk_MRI_1 %>%
  mutate(MRI_RIDX_quintile = cut(MRI_RIDX, 
                                unique(quantile(df_risk_MRI_1$MRI_RIDX, 
                                                probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                include.lowest = TRUE),
         MRI_RIDX2_quintile = cut(MRI_RIDX2, 
                                 unique(quantile(df_risk_MRI_1$MRI_RIDX2, 
                                                 probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                 include.lowest = TRUE),
         MRI_IDX_quintile = cut(MRI_IDX, 
                                unique(quantile(df_risk_MRI_1$MRI_IDX, 
                                                probs = seq(0, 1, length.out = 6), na.rm = TRUE)), 
                                include.lowest = TRUE))

# creating palletes for Mortality Risk Indexes
pallete.MRI_RIDX <- colorFactor(palette = "YlOrRd", df_risk_MRI_1$MRI_RIDX_quintile)
pallete.MRI_RIDX2 <- colorFactor(palette = "YlOrRd", df_risk_MRI_1$MRI_RIDX2_quintile)
pallete.MRI_IDX <- colorFactor(palette = "YlOrRd", df_risk_MRI_1$MRI_IDX_quintile)


###############################################################
##                                                           ##
##      RISK MAPPING SECTION: TRANSMISSION RISK INDEXES      ##
##                                                           ##
###############################################################

# interactive map for admin level 1 departments of my_country: 
# Transmission Risk Index (raw)
df_risk_TRI_1 %>%
  # filtering for the last date (could be replaced by a slider in shiny)
  filter(report_date == max(report_date, na.rm = TRUE)) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.TRI_RIDX(TRI_RIDX_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>Country: </b>", NAME_0,
                              "<br><b>Department: </b>", NAME_1,
                              "<br><b>Transmission Risk Index (raw): </b>", round(TRI_RIDX, 2))) %>%  
  addLegend("bottomright", 
            pal = pallete.TRI_RIDX,
            values = ~TRI_RIDX_quintile,
            title = ~paste0("Transmission Risk Index <br> (raw)"),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )

# interactive map for admin level 1 departments of my_country: 
# Transmission Risk Index Normalized between 0 and 100 by the maximum raw value for each day)
df_risk_TRI_1 %>%
  # filtering for the last date (could be replaced by a slider in shiny)
  filter(report_date == max(report_date, na.rm = TRUE)) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.TRI_IDX(TRI_IDX_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>Country: </b>", NAME_0,
                              "<br><b>Department: </b>", NAME_1,
                              "<br><b>Transmission Risk Index (standardized by day): </b>", round(TRI_IDX, 2))) %>%  
  addLegend("bottomright", 
            pal = pallete.TRI_IDX,
            values = ~TRI_IDX_quintile,
            title = ~paste0("Transmission Risk Index <br> (standardized by day)"),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )

# interactive map for admin level 1 departments of my_country: 
# Transmission Risk Index Normalized between 0 and 100 by the maximum raw value for each day
# but exluding outliers)
df_risk_TRI_1 %>%
  # filtering for the last date (could be replaced by a slider in shiny)
  filter(report_date == max(report_date, na.rm = TRUE)) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.TRI_IDX2(TRI_IDX2_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>Country: </b>", NAME_0,
                              "<br><b>Department: </b>", NAME_1,
                              "<br><b>Transmission Risk Index (standardized by day and without outliers): </b>",
                              round(TRI_IDX2, 2))) %>%  
  addLegend("bottomright", 
            pal = pallete.TRI_IDX2,
            values = ~TRI_IDX2_quintile,
            title = ~paste0("Transmission Risk Index <br> (standardized by day and without outliers)"),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )


############################################################
##                                                        ##
##      RISK MAPPING SECTION: MORTALITY RISK INDEXES      ##
##                                                        ##
############################################################

# interactive map for admin level 1 departments of my_country: 
# Mortality Risk Index (raw and not including distance from medical facility)
df_risk_MRI_1 %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.MRI_RIDX(MRI_RIDX_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>Country: </b>", NAME_0,
                              "<br><b>Department: </b>", NAME_1,
                              "<br><b>Mortality Risk Index (raw and not including distance from medical facility): </b>", round(MRI_RIDX, 2))) %>%  
  addLegend("bottomright", 
            pal = pallete.MRI_RIDX,
            values = ~MRI_RIDX_quintile,
            title = ~paste0("Mortality Risk Index <br> (raw and not including distance from medical facility)"),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )

# interactive map for admin level 1 departments of my_country: 
# Mortality Risk Index (raw and including distance from medical facility)
df_risk_MRI_1 %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.MRI_RIDX2(MRI_RIDX2_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>Country: </b>", NAME_0,
                              "<br><b>Department: </b>", NAME_1,
                              "<br><b>Mortality Risk Index (raw and including distance from medical facility): </b>", round(MRI_RIDX2, 2))) %>%  
  addLegend("bottomright", 
            pal = pallete.MRI_RIDX2,
            values = ~MRI_RIDX2_quintile,
            title = ~paste0("Mortality Risk Index <br> (raw and including distance from medical facility)"),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )

# interactive map for admin level 1 departments of my_country: 
# Normalized Mortality Risk Index including distance from medical facility between 0 and 100 
# based on the maximum and minimum raw value.
df_risk_MRI_1 %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.75,
              fillColor = ~pallete.MRI_IDX(MRI_IDX_quintile),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>Country: </b>", NAME_0,
                              "<br><b>Department: </b>", NAME_1,
                              "<br><b>Mortality Risk Index (standardized and including distance from medical facility): </b>",
                              round(MRI_IDX, 2))) %>%  
  addLegend("bottomright", 
            pal = pallete.MRI_IDX,
            values = ~MRI_IDX_quintile,
            title = ~paste0("Mortality Risk Index <br> (standardized and including distance from medical facility)"),
            opacity = 1) %>% 
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = TRUE)
  )

