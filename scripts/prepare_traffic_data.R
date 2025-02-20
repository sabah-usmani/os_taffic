
#### Description: This script prepares trafffic data for analysis ----------------------- #### 
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

#### Description: This script prepares traffic data ---------------------------- ####

## Preparing start-end traffic data (uses 2020 NYC census tracts) ----------------

# Loading Monthly Data for all boroughs start_end (May-Aug 2019, May-Aug 2021) (Downloaded from StreetLight)

bronx_may19 <- read.csv("data/traffic_data/start_end/bronx/may_19_bronx/may_19_bronx_za_all.csv") %>% clean_names()

bronx_jun19 <- read.csv("data/traffic_data/start_end/bronx/june_19_bronx/june_19_bronx_za_all.csv") %>% clean_names()

bronx_jul19 <- read.csv("data/traffic_data/start_end/bronx/july_19_bronx/july_19_bronx_za_all.csv") %>% clean_names()

bronx_aug19 <- read.csv("data/traffic_data/start_end/bronx/aug_19_bronx/aug_19_bronx_za_all.csv") %>% clean_names()

bronx_may21 <- read.csv("data/traffic_data/start_end/bronx/may_21_bronx/may_21_bronx_za_all.csv") %>% clean_names()

bronx_jun21 <- read.csv("data/traffic_data/start_end/bronx/june_21_bronx/june_21_bronx_za_all.csv") %>% clean_names()

bronx_jul21 <- read.csv("data/traffic_data/start_end/bronx/july_21_bronx/july_21_bronx_za_all.csv") %>% clean_names()

bronx_aug21 <- read.csv("data/traffic_data/start_end/bronx/aug_21_bronx/aug_21_bronx_za_all.csv") %>% clean_names()

bklynqueens1_may19 <- read.csv("data/traffic_data/start_end/bklyn_queens_1/may_19_queens_bk_1/may_19_queens_bk_1_za_all.csv") %>% clean_names()

bklynqueens1_jun19 <- read.csv("data/traffic_data/start_end/bklyn_queens_1/jun_19_queens_bk_1/jun_19_queens_bk_1_za_all.csv") %>% clean_names()

bklynqueens1_jul19 <- read.csv("data/traffic_data/start_end/bklyn_queens_1/jul_19_queens_bk_1/jul_19_queens_bk_1_za_all.csv") %>% clean_names()

bklynqueens1_aug19 <- read.csv("data/traffic_data/start_end/bklyn_queens_1/aug_19_queens_bk_1/aug_19_queens_bk_1_za_all.csv") %>% clean_names()

bklynqueens1_may21 <- read.csv("data/traffic_data/start_end/bklyn_queens_1/may_21_queens_bk_1/may_21_queens_bk_1_za_all.csv") %>% clean_names()

bklynqueens1_jun21 <- read.csv("data/traffic_data/start_end/bklyn_queens_1/jun_21_queens_bk_1/jun_21_queens_bk_1_za_all.csv") %>%  clean_names()

bklynqueens1_jul21 <- read.csv("data/traffic_data/start_end/bklyn_queens_1/jul_21_queens_bk_1/jul_21_queens_bk_1_za_all.csv") %>% clean_names()

bklynqueens1_aug21 <- read.csv("data/traffic_data/start_end/bklyn_queens_1/aug_21_queens_bk_1/aug_21_queens_bk_1_za_all.csv") %>% clean_names()

bklynqueens2_may19 <- read.csv("data/traffic_data/start_end/bklyn_queens_2/may_19_queens_bk_2/may_19_queens_bk_2_za_all.csv") %>% clean_names()

bklynqueens2_jun19 <- read.csv("data/traffic_data/start_end/bklyn_queens_2/jun_19_queens_bk_2/jun_19_queens_bk_2_za_all.csv") %>% clean_names()

bklynqueens2_jul19 <- read.csv("data/traffic_data/start_end/bklyn_queens_2/jul_19_queens_bk_2/jul_19_queens_bk_2_za_all.csv") %>% clean_names()

bklynqueens2_aug19 <- read.csv("data/traffic_data/start_end/bklyn_queens_2/aug_19_queens_bk_2/aug_19_queens_bk_2_za_all.csv") %>% clean_names()

bklynqueens2_may21 <- read.csv("data/traffic_data/start_end/bklyn_queens_2/may_21_queens_bk_2/may_21_queens_bk_2_za_all.csv") %>% clean_names()

bklynqueens2_jun21 <- read.csv("data/traffic_data/start_end/bklyn_queens_2/jun_21_queens_bk_2/jun_21_queens_bk_2_za_all.csv") %>% clean_names() 

bklynqueens2_jul21 <- read.csv("data/traffic_data/start_end/bklyn_queens_2/jul_21_queens_bk_2/jul_21_queens_bk_2_za_all.csv") %>% clean_names()

bklynqueens2_aug21 <- read.csv("data/traffic_data/start_end/bklyn_queens_2/aug_21_queens_bk_2/aug_21_queens_bk_2_za_all.csv") %>% clean_names() 
  
si_may19 <- read.csv("data/traffic_data/start_end/si/may_19_si/may_19_si_za_all.csv") %>% clean_names()

si_jun19 <- read.csv("data/traffic_data/start_end/si/jun_19_si/jun_19_si_za_all.csv") %>% clean_names()

si_jul19 <- read.csv("data/traffic_data/start_end/si/jul_19_si/jul_19_si_za_all.csv") %>% clean_names()

si_aug19 <- read.csv("data/traffic_data/start_end/si/aug_19_si/aug_19_si_za_all.csv") %>% clean_names()

si_may21 <- read.csv("data/traffic_data/start_end/si/may_21_si/may_21_si_za_all.csv") %>% clean_names()

si_jun21 <- read.csv("data/traffic_data/start_end/si/jun_21_si/jun_21_si_za_all.csv") %>% clean_names()

si_jul21 <- read.csv("data/traffic_data/start_end/si/jul_21_si/jul_21_si_za_all.csv") %>% clean_names()

si_aug21 <- read.csv("data/traffic_data/start_end/si/aug_21_si/aug_21_si_za_all.csv") %>% clean_names()

ma_may19 <- read.csv("data/traffic_data/start_end/manhattan/may_19_man/may_19_man_za_all.csv") %>% clean_names()

ma_jun19 <- read.csv("data/traffic_data/start_end/manhattan/jun_19_man/jun_19_man_za_all.csv") %>% clean_names()

ma_jul19 <- read.csv("data/traffic_data/start_end/manhattan/jul_19_man/jul_19_man_za_all.csv") %>% clean_names()

ma_aug19 <- read.csv("data/traffic_data/start_end/manhattan/aug_19_man/aug_19_man_za_all.csv") %>% clean_names()

ma_may21 <- read.csv("data/traffic_data/start_end/manhattan/may_21_man/may_21_man_za_all.csv") %>%  clean_names()

ma_jun21 <- read.csv("data/traffic_data/start_end/manhattan/jun_21_man/jun_21_man_za_all.csv") %>% clean_names()

ma_jul21 <- read.csv("data/traffic_data/start_end/manhattan/jul_21_man/jul_21_man_za_all.csv") %>% clean_names()

ma_aug21 <- read.csv("data/traffic_data/start_end/manhattan/aug_21_man/aug_21_man_za_all.csv") %>% clean_names()

#join rows into one monthly dataframe for start_end traffic may-august 2019 and 2021 (2020 census tracts)  
#Manhattan Traffic  
ma_traffic <- bind_rows(ma_may19, ma_jun19, ma_jul19, ma_aug19, ma_may21, ma_jun21, ma_jul21, ma_aug21) %>% 
  filter(zone_name != "New York") %>% 
  mutate(zone_name = as.numeric(zone_name))

#Rest of the boroughs traffic + Manhattan traffic (above)
se_traffic <- bind_rows(bronx_may19,bronx_jun19,bronx_jul19,bronx_aug19,bronx_may21,bronx_jun21,bronx_jul21,bronx_aug21, bklynqueens1_may19, bklynqueens1_jun19, bklynqueens1_jul19, bklynqueens1_aug19, bklynqueens1_may21, bklynqueens1_jun21, bklynqueens1_jul21, bklynqueens1_aug21, bklynqueens2_may19, bklynqueens2_jun19, bklynqueens2_jul19, bklynqueens2_aug19, bklynqueens2_may21, bklynqueens2_jun21, bklynqueens2_jul21, bklynqueens2_aug21, si_may19, si_jun19, si_jul19, si_aug19, si_may21, si_jun21, si_jul21, si_aug21, ma_traffic) %>% filter(zone_name != "New York") %>% 
mutate(zone_name = as.numeric(zone_name)) 

#checking number of unique census tracts (2020 NYC census tracts)
se_traffic %>% distinct(zone_name) %>% count() #2328 tracts 

#clean environment except for se_traffic
rm(list = ls()[!ls() %in% "se_traffic"])

#organize columns and filter all day traffic 
nyc_traffic_alldays <- se_traffic %>% 
  select(data_periods, intersection_type, tractid = zone_name, day_type, day_part, ave_day_traffic = average_daily_zone_traffic_st_l_volume) %>%
  mutate(data_periods = ifelse(data_periods == "May 01, 2019 - May 31, 2019", "may19", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Jun 01, 2019 - Jun 30, 2019", "jun19", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Jul 01, 2019 - Jul 31, 2019", "jul19", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Aug 01, 2019 - Aug 31, 2019", "aug19", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "May 01, 2021 - May 31, 2021", "may21", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Jun 01, 2021 - Jun 30, 2021", "jun21", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Jul 01, 2021 - Jul 31, 2021", "jul21", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Aug 01, 2021 - Aug 31, 2021", "aug21", data_periods)) %>% 
  filter(day_part == "0: All Day (12am-12am)") %>% 
  mutate(day_type = sub("^.{2}", "", day_type)) %>% 
  filter(day_type == " All Days (M-Su)") %>% 
  group_by(data_periods, tractid) %>% 
  summarise(total = sum(ave_day_traffic), .groups = "drop") %>% 
  filter(tractid != "         NA")

#checking dataframe structure 
str(nyc_traffic_alldays)

#Loading NYC census tracts 2020 for join 
tracts2020 <- read.csv("data/census_data/censustracts2020.csv") %>% clean_names()

#join start_end traffic data with census tracts and remove tracts with NA values (n=3, water body tracts)
nyc_traffic_joined <- nyc_traffic_alldays %>% 
  left_join(tracts2020, by = c("tractid" = "geoid")) %>% 
  select(data_periods, tractid, total, boro_code, boro_name, boro_ct2020, shape_area) %>% 
  filter(!is.na(boro_ct2020))

#Saving daily average monthly traffic - start-end 
write.csv(nyc_traffic_joined, "data/cleaned_data/nyc_dailytraffic_se.csv", row.names = FALSE)

#Converting traffic data (Start_end) to wide format and computing traffic change - startend
nyc_traffic_wide <- nyc_traffic_joined %>% 
  pivot_wider(names_from = data_periods, values_from = total) %>% 
  mutate(may_change = may21 - may19, 
         jun_change = jun21 - jun19, 
         jul_change = jul21 - jul19, 
         aug_change = aug21 - aug19)

#saving daily traffic wide data for summer 2019 and 2021 - startend 
write.csv(nyc_traffic_wide, "data/joins/nyc_trafficwide_se.csv")

## Preparing passthrough traffic data ---------------------------------------------- 

#Loading passthrough data (downloaded from StreetLight)

#may 19 passthrough traffic (raw data)
may19_pt_1_2 <- read.csv("data/traffic_data/passthrough/may_19/NYC_pt_1_2_may19/NYC_pt_1_2_may19_za_all.csv") %>% clean_names()

may19_pt_3_4 <- read.csv("data/traffic_data/passthrough/may_19/NYC_pt_3_4_may19/NYC_pt_3_4_may19_za_all.csv") %>% clean_names()

may19_pt_5_6 <- read.csv("data/traffic_data/passthrough/may_19/NYC_pt_5_6_may19/NYC_pt_5_6_may19_za_all.csv") %>% clean_names()

#jun 19 passthrough traffic (raw data)
jun19_pt_1_2 <- read.csv("data/traffic_data/passthrough/jun_19/NYC_pt_1_2_jun19/NYC_pt_1_2_jun19_za_all.csv") %>% clean_names()

jun19_pt_3_4 <- read.csv("data/traffic_data/passthrough/jun_19/NYC_pt_3_4_jun19/NYC_pt_3_4_jun19_za_all.csv") %>% clean_names()

jun19_pt_5_6 <- read.csv("data/traffic_data/passthrough/jun_19/NYC_pt_5_6_jun19/NYC_pt_5_6_jun19_za_all.csv") %>% clean_names()

#jul 19 passthrough traffic (raw data)
jul19_pt_1_2 <- read.csv("data/traffic_data/passthrough/jul_19/NYC_pt_1_2_jul19/NYC_pt_1_2_jul19_za_all.csv") %>% clean_names()

jul19_pt_3_4 <- read.csv("data/traffic_data/passthrough/jul_19/NYC_pt_3_4_jul19/NYC_pt_3_4_jul19_za_all.csv") %>% clean_names()

jul19_pt_5_6 <- read.csv("data/traffic_data/passthrough/jul_19/NYC_pt_5_6_jul19/NYC_pt_5_6_jul19_za_all.csv") %>% clean_names()

#aug 19 passthrough traffic (raw data)
aug19_pt_1_2 <- read.csv("data/traffic_data/passthrough/aug_19/NYC_pt_1_2_aug19/NYC_pt_1_2_aug19_za_all.csv") %>% clean_names()

aug19_pt_3_4 <- read.csv("data/traffic_data/passthrough/aug_19/NYC_pt_3_4_aug19/NYC_pt_3_4_aug19_za_all.csv") %>% clean_names()

aug19_pt_5_6 <- read.csv("data/traffic_data/passthrough/aug_19/NYC_pt_5_6_aug19/NYC_pt_5_6_aug19_za_all.csv") %>% clean_names()

#may 21 passthrough traffic (raw data)
may21_pt_1_2 <- read.csv("data/traffic_data/passthrough/may_21/NYC_pt_1_2_may21/NYC_pt_1_2_may21_za_all.csv") %>% clean_names()

may21_pt_3_4 <- read.csv("data/traffic_data/passthrough/may_21/NYC_pt_3_4_may21/NYC_pt_3_4_may21_za_all.csv") %>% clean_names()

may21_pt_5_6 <- read.csv("data/traffic_data/passthrough/may_21/NYC_pt_5_6_may21/NYC_pt_5_6_may21_za_all.csv") %>% clean_names()

#jun 21 passthrough traffic (raw data)
jun21_pt_1_2 <- read.csv("data/traffic_data/passthrough/jun_21/NYC_pt_1_2_jun21/NYC_pt_1_2_jun21_za_all.csv") %>% clean_names()

jun21_pt_3_4 <- read.csv("data/traffic_data/passthrough/jun_21/NYC_pt_3_4_jun21/NYC_pt_3_4_jun21_za_all.csv") %>% clean_names()

jun21_pt_5_6 <- read.csv("data/traffic_data/passthrough/jun_21/NYC_pt_5_6_jun21/NYC_pt_5_6_jun21_za_all.csv") %>% clean_names()

#jul 21 passthrough traffic (raw data)
jul21_pt_1_2 <- read.csv("data/traffic_data/passthrough/jul_21/NYC_pt_1_2_jul21/NYC_pt_1_2_jul21_za_all.csv") %>% clean_names()

jul21_pt_3_4 <- read.csv("data/traffic_data/passthrough/jul_21/NYC_pt_3_4_jul21/NYC_pt_3_4_jul21_za_all.csv") %>% clean_names()

jul21_pt_5_6 <- read.csv("data/traffic_data/passthrough/jul_21/NYC_pt_5_6_jul21/NYC_pt_5_6_jul21_za_all.csv") %>% clean_names()

#aug 21 passthrough traffic (raw data)
aug21_pt_1_2 <- read.csv("data/traffic_data/passthrough/aug_21/NYC_pt_1_2_aug21/NYC_pt_1_2_aug21_za_all.csv") %>% clean_names()

aug21_pt_3_4 <- read.csv("data/traffic_data/passthrough/aug_21/NYC_pt_3_4_aug21/NYC_pt_3_4_aug21_za_all.csv") %>% clean_names()

aug21_pt_5_6 <- read.csv("data/traffic_data/passthrough/aug_21/NYC_pt_5_6_aug21/NYC_pt_5_6_aug21_za_all.csv") %>% clean_names()

#combining data for passthrough traffic may-august 2019 and 2021 
pt_traf <- bind_rows(may19_pt_1_2, may19_pt_3_4, may19_pt_5_6, may21_pt_1_2, may21_pt_3_4, may21_pt_5_6, jun19_pt_1_2, jun19_pt_3_4, jun19_pt_5_6, jun21_pt_1_2, jun21_pt_3_4, jun21_pt_5_6, jul19_pt_1_2, jul19_pt_3_4, jul19_pt_5_6, jul21_pt_1_2, jul21_pt_3_4, jul21_pt_5_6, aug19_pt_1_2, aug19_pt_3_4, aug19_pt_5_6, aug21_pt_1_2, aug21_pt_3_4, aug21_pt_5_6) %>% 
  select(data_periods, zone_id, zone_name, intersection_type, day_type, day_part, avg_day_traffic = average_daily_zone_traffic_st_l_volume, avg_pt_travel_time_sec = avg_all_travel_time_sec, avg_pt_trip_length_mi = avg_all_trip_length_mi) %>% 
  mutate(data_periods = ifelse(data_periods == "May 01, 2019 - May 31, 2019", "may19", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Jun 01, 2019 - Jun 30, 2019", "jun19", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Jul 01, 2019 - Jul 31, 2019", "jul19", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Aug 01, 2019 - Aug 31, 2019", "aug19", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "May 01, 2021 - May 31, 2021", "may21", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Jun 01, 2021 - Jun 30, 2021", "jun21", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Jul 01, 2021 - Jul 31, 2021", "jul21", data_periods)) %>% 
  mutate(data_periods = ifelse(data_periods == "Aug 01, 2021 - Aug 31, 2021", "aug21", data_periods)) %>% 
  mutate(intersection_type = "passthrough") %>% 
  mutate(day_type = sub("^.{2}", "", day_type)) %>% 
  mutate(day_part = sub("^.{2}", "", day_part)) %>% 
  mutate(year = case_when(
    grepl("19", data_periods) ~ "2019",
    grepl("21", data_periods) ~ "2021",
    TRUE ~ NA_character_  )) %>% 
  filter(day_type == " All Days (M-Su)", 
         day_part == " All Day (12am-12am)") %>%
  mutate(zone_name = as.numeric(zone_name)) 

#Joining traffic data with census tracts (2010 tracts)
#Loading 2010 census tracts CSV  
tracts2010 <- read.csv("data/census_data/censustracts2010.csv") %>% clean_names()%>% 
mutate(boro_ct2010 = as.numeric(boro_ct2010)) %>% 
select(ct_label, boro_code, boro = boro_name, ct2010, boro_ct2010, shape_area)

#join census data with pt_traf 
pt_census_join <- pt_traf %>% 
select(zone_name, zone_id, year, my = data_periods, intersection_type, day_type, day_part, avg_day_traffic, avg_pt_travel_time_sec, avg_pt_trip_length_mi) %>% 
left_join(tracts2010, by = c("zone_name" = "boro_ct2010"))

#average/mean trip length in miles (pt)
pt_census_join %>% 
  summarise(avg_trip_length = mean(avg_pt_trip_length_mi, na.rm = TRUE))

#average/mean travel time in seconds (pt)
pt_census_join %>% 
  summarise(avg_travel_time = mean(avg_pt_travel_time_sec, na.rm = TRUE)/60)

#pt traffic data - col names 
names(pt_census_join)
  
#Saving daily average monthly traffic - pass through with census tract information
write.csv(pt_census_join, "data/cleaned_data/nyc_dailytraffic_pt.csv", row.names = FALSE)

#Converting traffic data to wide format and computing traffic change - passthrough
nyc_traffic_wide_pt <- pt_census_join %>%
  select(-day_type, -day_part, -zone_id, -year, -ct_label, -boro_code, -ct2010, -shape_area, -intersection_type, -boro) %>% 
  pivot_wider(
    names_from = my,
    values_from = c(avg_day_traffic, avg_pt_travel_time_sec, avg_pt_trip_length_mi)
  )
  
#Saving daily average monthly traffic - pass through (wide format)
write.csv(nyc_traffic_wide_pt, "data/joins/nyc_trafficwide_pt.csv", row.names = FALSE)

# Preparing combined traffic data (start-end + pass through traffic)-----------------------------------

#clean environment
rm(list = ls())

#Loading cleaned start-end and pass through traffic data (wide)
se_traffic_wide <- read.csv("data/joins/nyc_trafficwide_se.csv") %>% clean_names()
pt_traffic_wide <- read.csv("data/joins/nyc_trafficwide_pt.csv") %>% clean_names()

#Preparing combined traffic data for join 
se_clean <- se_traffic_wide %>% 
  rename (borough = boro_name) %>%  
  mutate(zone_name = as.numeric(boro_ct2020)) %>% 
  select(zone_name, everything(), -"boro_ct2020")
  
pt_clean <- pt_traffic_wide %>% 
  mutate(zone_name = as.numeric(zone_name)) 

se_clean %>% pull(zone_name) %>% unique() %>% length()
pt_clean %>% pull(zone_name) %>% unique() %>% length()

#Convert shapefiles for start-end and pass through traffic data
#Loading NYC census tracts 2020  SHP for join
census20.shp <- st_read("data/census_data/nyct2020_24c/nyct2020.shp") %>% clean_names() %>% 
  mutate(geoid = as.numeric(geoid))

census10.shp <- st_read("data/census_data/nyct2010_24c/nyct2010.shp") %>% clean_names() %>% 
  mutate(boro_ct2010 = as.numeric(boro_ct2010))

#Creating se shapefile 
se_shp <- right_join(census20.shp, se_clean, by = c("geoid" = "tractid")) %>% 
st_write("data/shapefiles/se_traffic.shp", overwrite = TRUE, append = FALSE)

#Creating pt shapefile
pt_shp <- right_join(census10.shp, pt_clean, by = c("boro_ct2010" = "zone_name")) %>% 
  rename(
    pt_may19  = avg_day_traffic_may19,
    pt_may21  = avg_day_traffic_may21,
    pt_jun19  = avg_day_traffic_jun19,
    pt_jun21  = avg_day_traffic_jun21,
    pt_jul19  = avg_day_traffic_jul19,
    pt_jul21  = avg_day_traffic_jul21,
    pt_aug19  = avg_day_traffic_aug19,
    pt_aug21  = avg_day_traffic_aug21,
    tt_may19    = avg_pt_travel_time_sec_may19,
    tt_may21    = avg_pt_travel_time_sec_may21,
    tt_jun19    = avg_pt_travel_time_sec_jun19,
    tt_jun21    = avg_pt_travel_time_sec_jun21,
    tt_jul19    = avg_pt_travel_time_sec_jul19,
    tt_jul21    = avg_pt_travel_time_sec_jul21,
    tt_aug19    = avg_pt_travel_time_sec_aug19,
    tt_aug21    = avg_pt_travel_time_sec_aug21,
    len_may19   = avg_pt_trip_length_mi_may19,
    len_may21   = avg_pt_trip_length_mi_may21,
    len_jun19   = avg_pt_trip_length_mi_jun19,
    len_jun21   = avg_pt_trip_length_mi_jun21,
    len_jul19   = avg_pt_trip_length_mi_jul19,
    len_jul21   = avg_pt_trip_length_mi_jul21,
    len_aug19   = avg_pt_trip_length_mi_aug19,
    len_aug21   = avg_pt_trip_length_mi_aug21
  ) %>% select(-ct_label, -boro_code,-cd_eligibil,-nta_code,-puma)

st_write(pt_shp,"data/shapefiles/pt_traffic.shp", overwrite = TRUE, append = FALSE)

#pt and se shapefiles joined in QGIS (2020 census tracts) and saved as se_pt_traffic.shp

#Loading combined se and pt traffic shapefile (2020 Tracts)
se_pt_shp <- st_read("data/shapefiles/se_pt_traffic.shp") %>% clean_names() %>% 
  select(geoid, br_2020, boro_nm, shp_area = shp_r_x, nta_nam, aug19:ag_chng, pt_my19:pt_ag21, geometry) 

se_pt_no_geom <- st_drop_geometry(se_pt_shp)

#Converting shp to csv  
se_pt_no_geom %>% write.csv("data/cleaned_data/se_pt_traffic_wide.csv", row.names = FALSE)

#Loading combined se and pt traffic data (wide format)
se_pt_traffic <- read.csv("data/cleaned_data/se_pt_traffic_wide.csv") %>% clean_names() %>% 
  rename(may19_se = may19, may21_se = may21, jun19_se = jun19, jun21_se = jun21, jul19_se = jul19, jul21_se = jul21, aug19_se = aug19, aug21_se = aug21, may19_pt = pt_my19, may21_pt = pt_my21, jun19_pt = pt_jn19, jun21_pt = pt_jn21, jul19_pt = pt_jl19, jul21_pt = pt_jl21, aug19_pt = pt_ag19, aug21_pt = pt_ag21, shape_area = shp_area)

#chaking column names 
names(se_pt_traffic)

#Converting into long format
se_pt_long <- se_pt_traffic %>%
  select(-my_chng, -jn_chng, -jl_chng, -ag_chng) %>% 
  pivot_longer(
    cols = starts_with(c("may", "jun", "jul", "aug")),  # Select columns by month names
    names_to = c("my", ".value"),  #Create a 'my' (monthyear) column and separate 'se' and 'pt'
    names_pattern = "(\\w{3}\\d{2})_(.*)"  ) %>% 
  rename(borough = boro_nm, se_traf = se, pt_traf = pt) %>%
  mutate(total_traf = se_traf + pt_traf) %>% 
  mutate(my = factor(my, levels = c("may19", "jun19", "jul19", "aug19", "may21", "jun21", "jul21", "aug21"))) %>%
  #setting aug19 as reference year 
#mutate(my = forcats::fct_relevel(my, "aug19"))
  select(my, geoid, br_2020, se_traf, pt_traf, total_traf, everything())

#Loading combined traffic data (long format)
traffic_clean <- se_pt_long

#checking for NAs in traffic data
traffic_clean %>% filter(is.na(se_traf)) %>% pull(geoid) %>% unique() %>% length()
traffic_clean %>% filter(is.na(pt_traf)) %>% pull(geoid) %>% unique() %>% length()
traffic_clean %>% filter(is.na(total_traf)) %>% pull(geoid) %>% unique() %>% length()

#total census tracts (N=2,325)
traffic_clean %>% pull(geoid) %>% unique() %>% length() 

#removing row with incomplete data and renaming columns for clarity 
traffic_clean <- traffic_clean %>% 
  mutate(
    se_traf = ifelse(geoid == 36005051602 & is.na(se_traf), 0, se_traf),
    total_traf = ifelse(geoid == 36005051602 & is.na(total_traf), se_traf + pt_traf, total_traf)
  ) 

#Saving combined traffic data (long format) as a csv
write.csv(traffic_clean, "data/cleaned_data/se_pt_traffic_long.csv", row.names = FALSE)

