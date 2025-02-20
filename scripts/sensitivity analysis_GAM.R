### Description: This script runs sensitivity analysis GAM Model (with and without spatial term) --------------------------------------------
#Clean environment 
rm(list=ls())

#Loading required packages 
library(tidycensus)
library(tidyverse)
library(dplyr)
library(sf)
library(gamm4)
library(mgcv)
library(lme4)
library(sjPlot)

# Set working directory to the currently open RStudio project's root folder
setwd(rstudioapi::getActiveProject())
print(getwd())

# Getting data from the 2015-2022 5-year ACS
# a census api key is required to use get_acs
# B01003_001 total population
# B02001_003 black alone
# B03003_003 hispanic or latino
# B17001_002 income in the past 12 mo below poverty level

acs.dt <- tidycensus::get_acs(geography = "tract", state = "NY", variables = c("B01003_001", "B02001_003", "B03003_003", "B17001_002"), 
                              geometry = TRUE, year = 2022)

acs.wide <- spread(acs.dt[,1:4], variable, estimate)  
names(acs.wide)[3:6] <- c("popn", "black", "hisp", "pov") 
# estimate proportion of socio-demographic variables
acs.wide$perc.black <- acs.wide$black/acs.wide$popn    
acs.wide$perc.hisp  <- acs.wide$hisp/acs.wide$popn    
acs.wide$perc.pov   <- acs.wide$pov/acs.wide$popn  
acs.wide <- acs.wide %>% mutate(GEOID = as.numeric(GEOID))

# obtain the spatial object containing census tracts that will be used in further analysis
census_tracts_ses <- acs.wide %>%
  st_transform(2163)

# estimate area (km2)
census_tracts_ses$area_km2 <- as.numeric(st_area(census_tracts_ses)) / (1000*1000)

# estimate population density in inhabitants / km2
census_tracts_ses$pop_dens <- census_tracts_ses$popn/census_tracts_ses$area_km2
census_tracts_ses_cent <- st_centroid(census_tracts_ses) %>%
  sf::st_transform(4326)
census_tracts_ses_cent <- data.frame(GEOID = census_tracts_ses_cent$GEOID, sf::st_coordinates(census_tracts_ses_cent))
colnames(census_tracts_ses_cent)[2:3] <- c("lon", "lat")
census_tracts_ses <- dplyr::left_join(census_tracts_ses, census_tracts_ses_cent, by = "GEOID") %>% clean_names()

# loading LEHD Origin-Destination Employment Statistics (LODES) - WAC
lodes_wac <- read.csv('data/joins/nyc_lodes_wac.csv') %>% clean_names() %>% 
  select(geoid, tot_jobs, off_jobs) 

# Joining office jobs data with census data
census_tracts_ses <- census_tracts_ses %>% 
  left_join(lodes_wac, by = "geoid") 

# Loading traffic data
traffic_data <- read.csv("data/cleaned_data/se_pt_traffic_long_simulated.csv") %>% clean_names()

# Loading tracts with os --> (2020 census tracts)
os_tracts_2020 <- read.csv("data/joins/os_tracts_2020.csv") %>% clean_names() %>% 
  select("tractid" = "geoid", "boro_name", "boro_ct2020") %>% 
  mutate(treat = 1)

# Loading contiguous tracts (2020 census tracts)
cont_tracts_2020 <- read.csv("data/joins/cont_tracts_2020.csv") %>% clean_names() %>% 
  select("tractid" = "geoid", "boro_name", "boro_ct2020") %>% 
  mutate(treat = 2)

# Joining traffic and open streets (treated) information, excluding contiguous tracts
traffic_data <- traffic_data %>% 
  #remove parks and airports --> central park, prospect park, LGA, and JFK, Randall's Island)
  filter(!(br_2020 %in% c(1014300, 3017700, 4033100, 4071600, 1024000))) %>% 
  #tract containing incomplete information for start-end traffic 
  filter(geoid != 36005051602) %>% 
  #filter hoffman island 
  filter(geoid != 36085990100) %>% 
  left_join(os_tracts_2020, by = c("geoid" = "tractid")) %>% 
  mutate(treat = ifelse(is.na(treat), 0, treat)) %>% 
  mutate(post = if_else(str_detect(my, "21"), 1, 0)) 
traffic_data <- traffic_data %>% 
  select(-"boro_name", -"boro_ct2020")

# Joining covariate information 
traffic_covar <- traffic_data %>% 
  left_join(census_tracts_ses, by = "geoid") %>% 
  mutate (perc_off_jobs = off_jobs/tot_jobs) %>% 
  mutate(
    perc_black = if_else(is.na(perc_black), 0, perc_black),
    perc_hisp = if_else(is.na(perc_hisp), 0, perc_hisp),
    perc_pov = if_else(is.na(perc_pov), 0, perc_pov)) %>% 
  filter(!is.na(perc_off_jobs))

# Filtering out contiguous tracts 
traffic_nocont <- traffic_covar %>% 
  left_join(cont_tracts_2020, by = c("geoid" = "tractid")) %>% 
  filter(is.na(treat.y)) %>% 
  select(-"treat.y", -"boro_name", -"boro_ct2020") %>% 
  rename(treat = treat.x)

# Scaling relevant variables 
scaled <- dplyr::ungroup(traffic_nocont) %>% 
  dplyr::select(pop_dens, perc_black, perc_hisp, perc_pov, perc_off_jobs, lon, lat) %>% 
  purrr::map_df(~ scale(.x, center = FALSE, scale = sd(.x, na.rm = TRUE)))

data_scale <- cbind(traffic_nocont[, c("my", "geoid", "total_traf", "se_traf", "pt_traf", "treat", "post", "area_km2")],
                    scaled[, c("pop_dens", "perc_black", "perc_hisp", "perc_pov", "perc_off_jobs", "lon", "lat")]) %>% 
  mutate(my = as.factor(my))

str(data_scale)

#Sensitivity analysis results - Generalized Additive Mixed Linear model 
gamm_mod <- gamm4(total_traf ~ treat + post + treat*post + perc_pov + perc_black + perc_hisp + pop_dens + perc_off_jobs,
                  random = ~(1|geoid),
                  family = negative.binomial(1), 
                  data = data_scale, 
                  control = lme4::glmerControl(optCtrl=list(maxfun=2e4)))
summary(gamm_mod$mer, re.test = FALSE)
tab_model(gamm_mod$mer)

#Sensitivity analysis results - Generalized Additive Mixed model including spatial term 
gamm_mod_2 <- gamm4(total_traf ~ treat + post + treat*post + t2(lat,lon) + perc_pov + perc_black + perc_hisp + pop_dens + perc_off_jobs,
                    random = ~(1|geoid),
                    family = negative.binomial(1), 
                    data = data_scale, 
                    control = lme4::glmerControl(optCtrl=list(maxfun=2e4)))

summary(gamm_mod_2$mer, re.test = FALSE)
tab_model(gamm_mod_2$mer)



