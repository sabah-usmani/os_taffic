#### Description: This script prepares data for analysis ----------------------- #### 

#Installing and loading relevant packages if not already installed 
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("janitor")
#install.packages("sf")
#install.packages("lmtest")
#install.packages("tidycensus") 
#install.packages("httr")
#install.packages("jsonlite")
#install.packages("ggplot2)
#install.packages("lehdr")
#install.packages("rstudioapi") 

library(rstudioapi)
library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(lmtest)
library(RColorBrewer)
library(scales)
library(tidycensus)
library(httr)
library(jsonlite)
library(ggplot2)
#LEHD Origin-Destination Employment Statistics (LODES) dataset
library(lehdr)

# Set working directory to the currently open RStudio project's root folder
setwd(rstudioapi::getActiveProject())
print(getwd())

## Preparing percentage contiguous (spillover) variable data-----------------------------

# Loading census shapefile 2020
census <- st_read("data/shapefiles/census_tracts_2020/nyct2020.shp") %>%
  clean_names()

#Loading open streets treated tracts shapefile - 2020 Census
treated <- st_read("data/shapefiles/os_tracts_2020/os_tracts_2020.shp") %>%
  clean_names() %>% 
  mutate(treated = 1) %>% 
  as.data.frame() %>% 
  select (boro_ct2020, treated)

#Joining census and treated tracts
census_joined <- left_join(census, treated, by = "boro_ct2020")  %>%
  mutate(treated = if_else(is.na(treated), 0, treated))

#Plotting treated variable (open street tracts) from census joined
census_joined$treated <- as.factor(census_joined$treated)
ggplot(census_joined) +
  geom_sf(aes(fill = factor(treated))) +  # Treated as a factor (categorical)
  scale_fill_manual(values = c("orange", "blue"), labels = c("Not Treated", "Treated")) +
  theme_minimal() +
  labs(title = "Open Streets Tracts", fill = "Treated") +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Adding a new variable to count the number of polygons that touch a tract in tidy
census_joined <- census_joined %>% 
  mutate(n = lengths(st_touches(census_joined)))

#new column for number of contiguous polygons where treat == 1
neighbors <- st_touches(census_joined)

census_joined <- census_joined %>% 
  mutate(n_treated = sapply(neighbors, function(x) sum(census_joined$treated[x] == 1, na.rm = TRUE))) %>% 
  mutate(perc_treated = n_treated/n*100) %>% 
  #turn na in perct_treated to 0 and 1 decimal place 
  mutate(ifelse(is.na(perc_treated), 0, perc_treated)) %>%
  mutate(perc_treated = round(perc_treated, 1)) 

#plot n_treated (number of treated census tracts) variable from census joined
plot(census_joined["n_treated"])

#plot perc treated variable from census joined  (Figure 1b)
census_joined %>%
  #filter parks and airports 
  filter(!(boro_ct2020 %in% c(1014300, 3017700, 4033100, 4071600, 1024000, 5990100))) %>%
  ggplot() +
  geom_sf(aes(fill = perc_treated)) +
  scale_fill_gradientn(colors = colorRampPalette(c("black", "yellow"))(6),
                       na.value = "grey100", 
                       values = rescale(seq(0, 400, length.out = 6))) +
  theme(legend.position = "bottom") +
  labs(title = "Percentage of Contiguous Treated Tracts",
       fill = "Contiguous Tracts Treated (%)") + 
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.5, "cm"),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5))

#save plot (percentage contiguous treated)
ggsave("plots/percentage_contiguous_treated.png", width = 6, height = 6, dpi = 300)

#save census_joined as csv
write_csv(census_joined, "data/cleaned_data/spillover_variable.csv")

#Preparing census data (covariates)--------------------------------------------------------

# Getting data from the 2015-2019 5-year ACS
# a census api key is required to use get_acs
census_api_key("aa512886c5449a582d837da8d3a07af66a043fe5", install = TRUE, overwrite = TRUE)
# B01003_001 total population
# B02001_003 black alone
# B03003_003 Hispanic / Latino
# B17001_002 income in the past 12 mo below poverty level (in 2022 dollars)
# B19013_001 median income (in 2022 dollars)
#B23025_004 Employed 
ACSlist <- load_variables(2022, "acs5")

acs.dt <- tidycensus::get_acs(geography = "tract", state = "NY", variables = c("B01003_001", "B02001_003", "B03003_003", "B17001_002", "B19013_001", "B23025_004"), geometry = FALSE, year = 2022)
acs.wide <- spread(acs.dt[,1:4], variable, estimate)  
names(acs.wide)[3:8] <- c("popn", "black", "hisp", "pov", "med_inc", "emp")

acs.wide <- acs.wide %>% 
  mutate(popn = if_else(is.na(popn), 1, popn))  # Set a default to avoid division errors

# estimate proportion of socio-demographic variables
acs.wide$perc.black <- acs.wide$black/acs.wide$popn    
acs.wide$perc.hisp  <- acs.wide$hisp/acs.wide$popn    
acs.wide$perc.pov   <- acs.wide$pov/acs.wide$popn  
acs.wide$perc.emp   <- acs.wide$emp/acs.wide$popn  

# Using FIPS code '36' for state 'NY'
#bronx 005; kings 047; manhattan 061; queens 081; stat isl 085; 
acs.nyc <- acs.wide[which(substr(acs.wide$GEOID, 1, 5) %in% c("36005", "36047", "36061", "36081", "36085")),]

# saving nyc census data 
write.csv(acs.nyc, file = 'data/joins/nyccensuscovar.csv' , row.names = FALSE)

## Preparing Transit Data (Covariates)----------------------------------------

#url for MTA subway stations 
url <- "https://data.ny.gov/resource/39hk-dx4f.json"
#Sending GET request to API
response <- GET(url)
#Parse JSON content
mta_stops <- content(response, as = "text")
mta_stops <- fromJSON(mta_stops) %>% clean_names() 

mta_stops <- mta_stops %>%
  mutate(gtfs_latitude = as.numeric(gtfs_latitude), gtfs_longitude = as.numeric(gtfs_longitude))

#shapefile of mta stops 
mta_sf <- st_as_sf(mta_stops, coords = c("gtfs_longitude", "gtfs_latitude"), crs = 4326)

# Loading census shapefile 2020
census <- st_read("data/shapefiles/census_tracts_2020/nyct2020.shp") %>%
  clean_names()

# Set CRS for both datasets to EPSG:2263 (NAD83 / New York Long Island)
mta_sf <- st_transform(mta_sf, 4326)
census <- st_transform(census, 4326)

# Spatial join to assign each station to a Census tract
mta_within_census <- st_join(mta_sf, census, join = st_within)

# Count the number of MTA stations per Census tract
census_station_counts <- mta_within_census %>%
  group_by(geoid) %>%  
  summarise(subway_count = n()) %>%
  ungroup()

census_mta <- left_join(census, as.data.frame(census_station_counts), by = "geoid") %>% 
  mutate(subway_count = if_else(is.na(subway_count), 0, subway_count))

#Loading bus shelter data (shapefile)
bus_shelters <- st_read("data/transit_data/bus_shelters/geo_export_a6844404-9ff7-48a6-b98f-297f01bd22c4.shp") %>% clean_names()

bus_shelters <- st_transform(bus_shelters, 4326)

# Spatial join to assign each station to a Census tract
bus_within_census <- st_join(bus_shelters, census, join = st_within)

# Count the number of bus stations per Census tract
bus_shelters_counts <- bus_within_census %>%
  group_by(geoid) %>%  
  summarise(bus_count = n()) %>%
  ungroup()

#MTA stations and Bus Shelters combined
census_transit <- left_join(census_mta, as.data.frame(bus_shelters_counts), by = "geoid") %>% 
  mutate(bus_count = if_else(is.na(bus_count), 0, bus_count)) %>% 
  mutate(transit_count = subway_count + bus_count) %>% 
  select(geoid, subway_count, bus_count, transit_count)

#saving transit data as a shapefile
st_write(census_transit, "data/transit_data/combined/combined_transit_data.shp", overwrite = TRUE, append = FALSE)

## Preparing NYC zoning data (Covariates)----------------------------------------

#Loading NYC zoning districts (landuse) data 
zoning_data <- st_read("data/zoning/nyc_zoning_districts.shp") %>% clean_names()

zoning_data <- zoning_data %>% 
mutate(broad_category = case_when(
  str_starts(zonedist, "R") ~ "Residential",
  str_starts(zonedist, "C") ~ "Commercial",
  str_starts(zonedist, "M") ~ "Manufacturing",
  TRUE ~ "Park"
))

# Dissolve by broad zoning category
zoning_data <- st_make_valid(zoning_data)

zoning_simple <- zoning_data %>%
  group_by(broad_category) %>%
  summarise(geometry = st_union(geometry))

zoning_simple <- st_transform(zoning_simple, 4326)
census <- st_transform(census, 4326)

# Calculate the intersection between zoning and census tracts
zoning_within_tracts <- st_intersection(census, zoning_simple) %>% 
  mutate(intersect_area = st_area(geometry))

# Summarize area by census tract to get zoning coverage
coverage_summary <- zoning_within_tracts %>%
  group_by(geoid, broad_category) %>%
  summarise(zoning_area = sum(intersect_area)) %>%
  ungroup() %>% 
  as.data.frame() %>% 
  select(-geometry)

coverage_summary <- coverage_summary %>% 
  mutate(zoning_area = as.numeric(zoning_area))

# Pivot to wide format
coverage_wide <- coverage_summary %>%
  pivot_wider(names_from = broad_category, values_from = zoning_area, values_fill = 0)

#combine with census data
census_zoning <- left_join(census, coverage_wide, by = "geoid") %>% 
  mutate(total_area = st_area(geometry),total_area = as.numeric(total_area))%>% 
  select(geoid, boro_ct2020, Commercial, Manufacturing, Park, Residential, total_area, geometry) %>% 
  mutate(Commercial = Commercial/total_area, Manufacturing = Manufacturing/total_area, Park = Park/total_area, Residential = Residential/total_area)

#save zoning data as a shapefile
st_write(census_zoning, "data/zoning/perc_coverage_zoning.shp", overwrite = TRUE, append = FALSE)

#Preparing Transportation Alternatives Data (Sensitivity Analysis) ----------------------------------------
ta_tracts <- readRDS("data/shapefiles/ta_tracts/cns_trct_open_streets_rev01.rds") %>% clean_names()

#make dataframe 
ta_tracts_df <- as.data.frame(ta_tracts) %>% 
  select(geoid, op_st_active, op_st_presence)

#Loading census shapefile 2020
census <- st_read("data/shapefiles/census_tracts_2020/nyct2020.shp") %>% clean_names() %>% 
  mutate(geoid = as.numeric(geoid))

#transforming ta_tracts to match census crs
ta_tracts <- st_transform(ta_tracts, st_crs(census))

# Perform spatial intersection to find overlapping areas between 'ta_tracts' and 'census' tracts
intersections <- st_intersection(ta_tracts, census) %>%
  mutate(intersection_area = st_area(.))

# Assign each census tract to the transit area (TA) tract with the largest intersection area
majority_assignment <- intersections %>%
  group_by(geoid.1) %>%
  filter(intersection_area == max(intersection_area)) %>%
  ungroup() %>% 
  select(geoid_20 = geoid.1, op_st_active, op_st_presence, geometry)

#convert sf to df
majority_assignment_df <- as.data.frame(majority_assignment) %>% 
  select(-geometry)

ta_tracts_20 <- census %>% 
  left_join(majority_assignment_df, by = c("geoid" = "geoid_20")) 

active_tracts <- ta_tracts_20 %>% 
  filter(op_st_active == 1)

# Find bordering tracts
contiguous_tracts <- st_filter(ta_tracts_20, active_tracts, .predicate = st_touches) %>% 
  filter(op_st_active == 0) %>% 
  as.data.frame() %>% 
  mutate(cont = 1) %>%
  select(geoid, op_st_active, cont)

ta_tracts_20 <- ta_tracts_20 %>% 
  left_join(contiguous_tracts, by = "geoid") %>% 
  select(-op_st_active.y) 

ta_tracts_20 <- ta_tracts_20 %>% 
  mutate(cont = ifelse(is.na(cont), 0, cont)) %>% 
  as.data.frame() %>% 
  select(geoid, boro_ct2020, op_st_active = op_st_active.x, op_st_presence = op_st_active.x, cont)

#Save TA Open Streets data 
write.csv(ta_tracts_20, file = 'data/joins/activestreets_ta.csv' , row.names = FALSE)

## Preparing LEHD Origin-Destination Employment Statistics (LODES) Data (Covariate)----------------------------------

#Loading LODES Workplace Area Characteristics (WAC) data for New York City (NYC) for 2019 
ny_wac <- grab_lodes(
  state = "ny",
  year = 2019,          # Specify the year of interest
  lodes_type = "wac",   # Specify Workplace Area Characteristics
  job_type = "JT00",    # All jobs
  segment = "S000",     # All workers
  agg_geo = "tract"     # Aggregate to census tract level
)

# FIPS codes for NYC counties
nyc_fips <- c("36005", "36047", "36061", "36081", "36085")

# Filter for NYC tracts
nyc_wac <- ny_wac[substr(ny_wac$w_tract, 1, 5) %in% nyc_fips, ] %>% 
  select(state, geoid = w_tract, tot_jobs = C000, CNS09:CNS20) %>% 
  #calculating total oficce jobs
  mutate(off_jobs = CNS09 + CNS10 + CNS11 + CNS12 + CNS13 + CNS14 + CNS15 + CNS16 + CNS17 + CNS18 + CNS19 + CNS20)

#save LODES data as a csv
write.csv(nyc_wac, file = 'data/joins/nyc_lodes_wac.csv' , row.names = FALSE)
   