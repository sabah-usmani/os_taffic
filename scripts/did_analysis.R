### Description: This script runs difference-in-differences analysis (main model) and difference-in-differences sensitivity analysis for 'active' streets only (transportation alternatives data) ------------------------

#Clean environment 
rm(list=ls())

# Set working directory to the currently open RStudio project's root folder
setwd(rstudioapi::getActiveProject())
print(getwd())

#Installing and loading required packages

# Define the list of required packages
packages <- c("MatchIt", "tidyverse", "knitr", "kableExtra", "fixest", "readxl", 
              "janitor", "coefplot", "rlang", "sf", "lmtest", "stargazer", 
              "plm", "sandwich", "broom", "haven", "sjPlot", "sjmisc", 
              "sjlabelled", "modelsummary", "ggplot2", "webshot")

# Install and load the packages
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#Setting themes for plots
theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.color = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d

#Loading Data sets---------------------------------------------------------------------
#Loading traffic 
traffic_data <- read.csv("data/cleaned_data/se_pt_traffic_long_simulated.csv") %>% clean_names() %>%   mutate(my = tolower(gsub("([0-9]+)-([A-Za-z]+)", "\\2\\1", my)))
  
#Loading tracts with os --> (2020 census tracts)
os_tracts_2020 <- read.csv("data/joins/os_tracts_2020.csv") %>% clean_names() %>% 
  select("tractid" = "geoid", "boro_name", "boro_ct2020") %>% 
  mutate(treat = 1)

#Loading percentage contiguous (spillover) variable data 
spillover <- read.csv("data/cleaned_data/spillover_variable.csv") %>% clean_names() %>% 
  select("zone_name" = "boro_ct2020", treated, n, n_treated, perc_treated)

#Loading census tracts (2020)
census_2020 <- read.csv("data/census_data/censustracts2020.csv") %>% clean_names()

#Loading matching variables (NY)
census_covar <- read.csv("data/joins/nyccensuscovar.csv") %>% clean_names() 

#Additional covariates - transit stop count 
transit_stops <- st_read("data/transit_data/combined/combined_transit_data.shp") %>% clean_names() %>% 
  mutate(geoid = as.numeric(geoid))

#Additional covariates - zoning data
zoning <- st_read("data/zoning/perc_coverage_zoning.shp") %>% clean_names() %>% 
  mutate(geoid = as.numeric(geoid))

#Additional covariates - LEHD Origin-Destination Employment Statistics (LODES) - WAC
lodes_wac <- read.csv('data/joins/nyc_lodes_wac.csv') %>% clean_names() %>% 
  select(geoid, tot_jobs, off_jobs) 

#Prep traffic data (PT + SE) for DID Analysis--------------------------------------------------

#unique geoids 
total_tracts_2020 <- traffic_data %>% pull(geoid) %>% unique() %>% length() 

#filtering/cleaning traffic data 
traffic_data <- traffic_data %>% 
  #setting aug19 as reference year 
mutate(my = forcats::fct_relevel(my, "aug19")) %>% 
#remove parks and airports --> central park, prospect park, LGA, and JFK, Randall's Island)
filter(!(br_2020 %in% c(1014300, 3017700, 4033100, 4071600, 1024000))) %>% 
#tract containing incomplete information for start-end traffic 
filter(geoid != 36005051602) %>% 
  #filter hoffman island 
  filter(geoid != 36085990100)

#Included tracts
total_tracts <- traffic_data %>% pull(geoid) %>% unique() %>% length()

#percentage excluded tracts 
total_tracts_excluded <- (total_tracts_2020-total_tracts)/total_tracts_2020 * 100

#Joining traffic and Open Streets data 
traffic_os_joined <- traffic_data %>% 
  left_join(os_tracts_2020, by = c("br_2020" = "boro_ct2020")) %>% 
  mutate(treat = replace(treat, is.na(treat), 0)) %>% 
  mutate(post = if_else(str_detect(my, "21"), 1, 0)) %>% 
  select(-tractid, -boro_name)

#Joining spillover variable (contiguous percentage contiguous OS tracts) 
#joining traffic and spillover data 
traffic_spill <- traffic_os_joined %>% 
  left_join(spillover, by = c("br_2020" = "zone_name")) %>% 
  #excluding contiguous tracts
  mutate(treat_post = treat * post)%>% 
#For island tracts, perc_treat == 0 because no contiguous tracts 
  mutate(perc_treated = if_else(is.na(perc_treated), 0, perc_treated)) %>% 
  select(-treated) 

## Joining covariates with traffic data----------------------------------------

#Joining matching variables with traffic data (contiguous tracts included)
census_covar <- census_covar %>%
  left_join(census_2020, by = "geoid") %>% 
  mutate(pop_den = popn/shape_area) %>% 
  mutate(zone_name = as.numeric(boro_ct2020)) %>% 
  filter(!is.na(boro_ct2020))

#total tracts 
total_tracts <- census_covar %>% pull(geoid) %>% unique() %>% length()

#Joining census data with traffic data 
prop_data <- traffic_spill %>% left_join(census_covar, by = "geoid") %>% 
  mutate(
    black = if_else(popn == 0, 0, black),
    hisp = if_else(popn == 0, 0, hisp),
    pov = if_else(popn == 0, 0, pov),
    perc_black = if_else(popn == 0, 0, perc_black),
    perc_hisp = if_else(popn == 0, 0, perc_hisp),
    perc_pov = if_else(popn == 0, 0, perc_pov)
  ) %>% 
  #Joining transit stops and zoning data
  left_join(transit_stops, by = "geoid" ) %>%
  left_join(zoning, by = "geoid") %>% 
  rename(transit_stops = trnst_c, commercial = commrcl, manufacturing = mnfctrn, residential = resdntl) %>% 
  #Joining LODES jobs by Workplace Area Characteristics (WAC)
  left_join(lodes_wac, by = "geoid") %>% 
  #Remove LODES NA values 
  filter(!is.na(tot_jobs) | !is.na(off_jobs)) %>% 
  mutate(perc_jobs = off_jobs/tot_jobs)

#total tracts
tracts_matching <- prop_data %>% pull(geoid) %>% unique() %>%  length()

#Total excluded tracts 
excluded_tracts <- total_tracts - tracts_matching

#Total open streets tracts (treated) --> count rows where treat == 1 (Total = 296)
prop_data %>% filter(treat == 1) %>% pull(geoid) %>% unique() %>% length()

#mean traffic volume 
mean_traffic <- prop_data %>% filter(post==0) %>% summarize(mean = mean(total_traf)) %>% pull(mean)

## Matched Analysis (PSM)-----------------------------------------------------------
#Matching using MatchIt package (one to one nearest neighbor)
m.out <- matchit(treat ~ perc_jobs + pop_den + perc_black + perc_hisp + perc_pov, data = prop_data, method = "nearest")
data_matched <- match.data(m.out)
data_matched$Group <- ifelse(data_matched$treat == 1, "Treated", "Comparison")

#joining transit stops data and zoning data 
data_matched <- data_matched %>% 
  left_join(transit_stops, by = "geoid" ) %>%
  left_join(zoning, by = "geoid") 
 
#Summarize matched data by total treated and control
summary_traffic <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(total_traf), sd = sd(total_traf))
summary_percblack <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(perc_black), sd = sd(perc_black))
summary_hisp <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(perc_hisp), sd = sd(perc_hisp))
summary_percpov <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(perc_pov), sd = sd(perc_pov))
summary_transit <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(transit_stops), sd = sd(transit_stops))
summary_coms <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(commercial), sd = sd(commercial))
summery_manu <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(manufacturing), sd = sd(manufacturing))
summary_res <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(residential), sd = sd(residential))
summary_parks <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(park.x), sd = sd(park.x))
summary_percjobs <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(perc_jobs), sd = sd(perc_jobs))

#Total treated tracts  
data_matched %>% filter(treat == 1) %>% pull(zone_name) %>% unique() %>% length()

#Matched data summary table 
overall_avg <- data_matched %>%
  summarize(
    n_zones = n_distinct(zone_name),
    n_traffic = n(),
    mean_traffic = round(mean(total_traf), 2),
    sd_traffic = round(sd(total_traf), 2),
    mean_percblack = round(mean(perc_black), 2),
    sd_percblack = round(sd(perc_black), 2),
    mean_hisp = round(mean(perc_hisp), 2),
    sd_hisp = round(sd(perc_hisp), 2),
    mean_percpov = round(mean(perc_pov), 2),
    sd_percpov = round(sd(perc_pov), 2),
    mean_transit = round(mean(transit_stops), 2),
    sd_transit = round(sd(transit_stops), 2),
    mean_commercial = round(mean(commercial), 2),
    sd_commercial = round(sd(commercial), 2),
    mean_residential = round(mean(residential), 2),
    sd_residential = round(sd(residential), 2),
    mean_manufacturing = round(mean(manufacturing), 2),
    sd_manufacturing = round(sd(manufacturing), 2), 
    mean_jobs = round(mean(perc_jobs), 2),
    sd_jobs = round(sd(perc_jobs), 2),
    mean_popn = round(mean(popn), 2),
    sd_popn = round(sd(popn),2),
    mean_perctreated = round(mean(perc_treated), 2),
    sd_perctreated = round(sd(perc_treated), 2)
  ) %>% 
  mutate(Group = "Total") %>% 
  select(Group, everything())

summary_combined <- data_matched %>%
  group_by(Group) %>%
  summarize(
    n_zones = n_distinct(zone_name),
    n_traffic = n(),
    mean_traffic = round(mean(total_traf), 2),
    sd_traffic = round(sd(total_traf), 2),
    mean_percblack = round(mean(perc_black), 2),
    sd_percblack = round(sd(perc_black), 2),
    mean_hisp = round(mean(perc_hisp), 2),
    sd_hisp = round(sd(perc_hisp), 2),
    mean_percpov = round(mean(perc_pov), 2),
    sd_percpov = round(sd(perc_pov), 2),
    mean_transit = round(mean(transit_stops), 2),
    sd_transit = round(sd(transit_stops), 2),
    mean_commercial = round(mean(commercial), 2),
    sd_commercial = round(sd(commercial), 2),
    mean_residential = round(mean(residential), 2),
    sd_residential = round(sd(residential), 2),
    mean_manufacturing = round(mean(manufacturing), 2),
    sd_manufacturing = round(sd(manufacturing), 2), 
    mean_jobs = round(mean(perc_jobs), 2),
    sd_jobs = round(sd(perc_jobs), 2),
    mean_popn = round(mean(popn), 2),
    sd_popn = round(sd(popn), 2),
    mean_perctreated = round(mean(perc_treated), 2),
    sd_perctreated = round(sd(perc_treated), 2)
  ) %>% 
  select(Group, everything())

summary_table <- bind_rows(overall_avg, summary_combined) %>% 
  mutate(Group = ifelse(Group == "Control", "Comparison (Matched)", Group)) %>% 
  select(Group, everything()) %>% 
  #2 significant digits for all 
  mutate_at(vars(-Group), ~round(., 2)) %>%
  #traffic as integer/non negative
  mutate(mean_traffic = as.integer(mean_traffic), sd_traffic = as.integer(sd_traffic)) %>%
  rename(n = n_traffic) 

df_new <- summary_table
df_new$mean_traffic <- paste0(summary_table$mean_traffic, " (", summary_table$sd_traffic, ")")
df_new$mean_percblack <- paste0(summary_table$mean_percblack, " (", summary_table$sd_percblack, ")")
df_new$mean_hisp <- paste0(summary_table$mean_hisp, " (", summary_table$sd_hisp, ")")
df_new$mean_percpov <- paste0(summary_table$mean_percpov, " (", summary_table$sd_percpov, ")")
df_new$mean_percemp <- paste0(summary_table$mean_percemp, " (", summary_table$sd_percemp, ")")
df_new$mean_popn <- paste0(summary_table$mean_popn, " (", summary_table$sd_popn, ")")
df_new$mean_jobs <- paste0(summary_table$mean_jobs, " (", summary_table$sd_jobs, ")")
df_new$mean_transit <- paste0(summary_table$mean_transit, " (", summary_table$sd_transit, ")")
df_new$mean_commercial <- paste0(summary_table$mean_commercial, " (", summary_table$sd_commercial, ")")
df_new$mean_residential <- paste0(summary_table$mean_residential, " (", summary_table$sd_residential, ")")
df_new$mean_manufacturing <- paste0(summary_table$mean_manufacturing, " (", summary_table$sd_manufacturing, ")")

df_new <- df_new %>% select(Group, n, n_zones, mean_traffic, mean_percblack, mean_hisp,  mean_percpov, mean_jobs, mean_popn, mean_transit, mean_commercial, mean_residential, mean_manufacturing) %>% 
  #rename columns
  rename("Tracts" = "Group", "N" = "n", "Census Tracts" = "n_zones", "Mean Traffic (MADT) (sd)" = "mean_traffic", "Proportionate Black - Mean (sd)*" = "mean_percblack", "Proportionate Hispanic - Mean (sd)*" = "mean_hisp", "Proportionate Below Poverty - Mean (sd)*" = "mean_percpov", "Proproportion Office-Based Employment - Mean (sd)*" = "mean_jobs", "Population - Mean (sd)" = "mean_popn", "Transit stops - Mean (sd)" = "mean_transit", "Percentage Commercial - Mean (sd)" = "mean_commercial", "Percentage Residential - Mean (sd)" = "mean_residential", "Percentage Manufacturing - Mean (sd)" = "mean_manufacturing") %>% 
  #reorder the rows putt 3rd as 2nd 
  mutate(Group = factor(Tracts, levels = c("Total", "Treated", "Comparison (Matched)"))) %>% 
  arrange(Group) %>% 
  select(-Group)

#In long format 
df_long <- t(df_new) 

#save as csv_table 1
write.csv(df_long, "plots/table1.csv")

# Using kable to display the table (Table 1)
summary_plot <- kable(df_long, align = 'r', format = "html") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(1, bold = TRUE) 

#view table 
summary_plot

##Visual check for parallel trends (Matched Data)-------------------------------
#Plotting parallel trends

trend_data_matched <- data_matched %>%
  group_by(treat, my) %>% 
  summarise(mean_madt = mean(total_traf)) %>% 
  mutate(my = factor(my, levels = c("may19", "jun19",  "jul19", "aug19",  "may21", "jun21",  "jul21", "aug21"))) %>%
  mutate(treat = as.factor(treat)) 

#. Graphical representation of parallel trends of mean daily average traffic before and after treatment (Figure S2)
trendplot_matched <- trend_data_matched %>% 
  ggplot() +
  geom_line(aes(x = my, y = mean_madt, group = treat, color = treat)) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "month-year", y = "mean daily average traffic (census tract)", title = "Graphical Diagnostic for Parallel Trends (Observed Mean)") +
  scale_color_manual(values = c("black", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5), 
    axis.text = element_text(size = 12)
  )

trendplot_matched

#save parallel trend plot (matched)
ggsave("plots/parallentrends_matched.png", trendplot_matched, width = 6, height = 6, dpi = 300)

#Event study analysis (Matched Data)--------------------------------------------
matched_eventmodel <- feols(
  total_traf ~ i(treat, i.my, ref = 0, ref2 = "aug19") + i(my) + i(geoid),
  data = data_matched, cluster = ~zone_name
)

summary(matched_eventmodel)

coefs_event <- broom::tidy(matched_eventmodel) %>%
  # Select only event-study terms
  filter(str_detect(term, "treat::1:my")) %>% 
  # Create year column
  mutate(term = substr(term, nchar(term) - 4, nchar(term))) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error)

# Add 0 for omitted month
coefs_event <- coefs_event %>%
  add_row(
    term = "aug19",
    estimate = 0,
    std.error = 0,
    conf.low = NA,
    conf.high = NA
  ) %>% 
  mutate(term = factor(term, levels = c("may19", "jun19", "jul19", "aug19", "may21", "jun21", "jul21", "aug21")))

# Event study plot (Figure S3)
ggplot(coefs_event, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, color = "blue") +
  labs(x = "month-year (event time)", y = "Average Treatment Effect (estimate)", title = "Event Study for Main Model - Matched") +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5)) 

#save event study plot - Matched/All Traffic 
ggsave("plots/matched_eventstudy.png", width = 6, height = 6, dpi = 300)

#Density Plot for Matched Data (Figure S1)---------------------------------------------------

#group by Group summarize 
summary_matched <- data_matched %>% group_by(Group) %>% summarize(n = n(), mean = mean(total_traf), pop = mean(pop_den), mean_employ = mean(perc_emp), mean_black = mean(perc_black), mean_hisp = mean(perc_hisp), mean_pov = mean(perc_pov), employ = mean(emp)) %>% 
  mutate(Group = ifelse(Group == "Comparison", "Control", Group)) 

matched_density_plot <- data_matched %>% 
  mutate(Group = ifelse(Group == "Comparison", "Control", Group)) %>% 
  ggplot(aes(x = distance, color = Group, linetype = Group)) +
  geom_density(aes(fill = Group), alpha = 0.2, size = 0.8, adjust = 1.5) +
  #scale_fill_manual(values = c("grey", "salmon")) +
  #scale_color_manual(values = c("blue", "red")) +
  scale_linetype_manual(values = c("Treated" = "dotted", "Control" = "solid")) +
  labs(title = " Density of Propensity Scores (Treated and Control Groups)",
       x = "Propensity Score (Distance)", y = "Density") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)) # Center title

#View density plot
matched_density_plot

#Save density plat (matched) (Figure S1)
ggsave("plots/matched_density_plot.png", matched_density_plot, width = 6, height = 6, dpi = 300)

## Running DID Analysis (Matched Data)-----------------------------------------------------

#Model 1 - DiD analysis (matched data) - Total Traffic, no spillover variable, fixed effect for month-year and tract
did_matched1 <- feols(
  total_traf ~ treat:post | zone_name + my,
  data = data_matched, 
  cluster = ~ geoid)
summary(did_matched1)
tab_model(did_matched1)

#Model spillover variable - Matched data - Total Traffic 
did_matched2 <- feols(
  total_traf ~ treat:post + perc_treated:post | zone_name + my,
  data = data_matched, 
  cluster = ~ zone_name)
summary(did_matched2)
tab_model(did_matched2)

#mean percentage _treated for open street tracts 
traffic_spill %>% filter(treat == 1) %>% pull(perc_treated) %>% mean()

## Sensitivity Analysis: Active-streets only (Transportation Alternative Data) ------------------------------

#Loading census tracts (2020) with Active Streets Information 
ta_data <- read.csv("data/joins/activestreets_ta.csv") %>% clean_names()

#Joining with traffic data 
ta_traffic <- traffic_spill %>% 
  left_join(ta_data, by = "geoid") %>% 
  mutate(op_st_active = if_else(treat == 0, 0, op_st_active)) %>% 
  filter(!(treat == 1 & op_st_active == 0))

#joining covariates 
prop_data_ta <- ta_traffic %>% left_join(census_covar, by = "geoid") %>% 
  mutate(
    black = if_else(popn == 0, 0, black),
    hisp = if_else(popn == 0, 0, hisp),
    pov = if_else(popn == 0, 0, pov),
    perc_black = if_else(popn == 0, 0, perc_black),
    perc_hisp = if_else(popn == 0, 0, perc_hisp),
    perc_pov = if_else(popn == 0, 0, perc_pov)
  ) %>% 
  #filter(!is.na(perc_pov))  %>% 
  left_join(transit_stops, by = "geoid" ) %>%
  left_join(zoning, by = "geoid") %>% 
  rename(transit_stops = trnst_c, commercial = commrcl, manufacturing = mnfctrn, residential = resdntl) %>%
  #Joining LODES jobs by Workplace Area Characteristics (WAC)
  left_join(lodes_wac, by = "geoid") %>% 
  #Remove LODES NA values 
  #filter(!is.na(tot_jobs)) %>% 
  filter(!is.na(tot_jobs) | !is.na(off_jobs)) %>% 
  mutate(perc_jobs = off_jobs/tot_jobs)

#Filtering tracts with open streets that are not active streets, filtering contiguous tracts 
prop_data_ta <- prop_data_ta %>% filter(!(op_st_active == 0 & treat == 1)) %>% 
  filter (!(op_st_active == 0 & cont ==1))

#Matching using MatchIt package (one to one nearest neighbor)
m.out_ta <- matchit(op_st_active ~ perc_jobs + pop_den + perc_black + perc_hisp + perc_pov, data = prop_data_ta, method = "nearest") 

data_matched_ta <- match.data(m.out_ta)
data_matched_ta$Group <- ifelse(data_matched_ta$op_st_active == 1, "Treated", "Comparison") 

#Number of treated tracts  
data_matched_ta %>% filter(op_st_active == 1) %>% pull(zone_name) %>% unique() %>% length()
#Total tracts 
data_matched_ta %>% pull(zone_name) %>% unique() %>% length()

#names data_matched_ta 
names(data_matched_ta)

#Matched data summary table (Sensitivity analysis)
overall_avg_ta <- data_matched_ta %>%
  summarize(
    n_zones = n_distinct(zone_name),
    n_traffic = n(),
    mean_traffic = round(mean(total_traf), 2),
    sd_traffic = round(sd(total_traf), 2),
    mean_percblack = round(mean(perc_black), 2),
    sd_percblack = round(sd(perc_black), 2),
    mean_hisp = round(mean(perc_hisp), 2),
    sd_hisp = round(sd(perc_hisp), 2),
    mean_percpov = round(mean(perc_pov), 2),
    sd_percpov = round(sd(perc_pov), 2),
    mean_transit = round(mean(transit_stops), 2),
    sd_transit = round(sd(transit_stops), 2),
    mean_commercial = round(mean(commercial), 2),
    sd_commercial = round(sd(commercial), 2),
    mean_residential = round(mean(residential), 2),
    sd_residential = round(sd(residential), 2),
    mean_manufacturing = round(mean(manufacturing), 2),
    sd_manufacturing = round(sd(manufacturing), 2), 
    mean_jobs = round(mean(perc_jobs), 2),
    sd_jobs = round(sd(perc_jobs), 2),
    mean_popn = round(mean(popn), 2),
    sd_popn = round(sd(popn),2),
    mean_perctreated = round(mean(perc_treated), 2),
    sd_perctreated = round(sd(perc_treated), 2)
  ) %>% 
  mutate(Group = "Total") %>% 
  select(Group, everything())


summary_combined_ta <- data_matched_ta %>%
  group_by(Group) %>%
  group_by(Group) %>%
  summarize(
    n_zones = n_distinct(zone_name),
    n_traffic = n(),
    mean_traffic = round(mean(total_traf), 2),
    sd_traffic = round(sd(total_traf), 2),
    mean_percblack = round(mean(perc_black), 2),
    sd_percblack = round(sd(perc_black), 2),
    mean_hisp = round(mean(perc_hisp), 2),
    sd_hisp = round(sd(perc_hisp), 2),
    mean_percpov = round(mean(perc_pov), 2),
    sd_percpov = round(sd(perc_pov), 2),
    mean_transit = round(mean(transit_stops), 2),
    sd_transit = round(sd(transit_stops), 2),
    mean_commercial = round(mean(commercial), 2),
    sd_commercial = round(sd(commercial), 2),
    mean_residential = round(mean(residential), 2),
    sd_residential = round(sd(residential), 2),
    mean_manufacturing = round(mean(manufacturing), 2),
    sd_manufacturing = round(sd(manufacturing), 2), 
    mean_jobs = round(mean(perc_jobs), 2),
    sd_jobs = round(sd(perc_jobs), 2),
    mean_popn = round(mean(popn), 2),
    sd_popn = round(sd(popn), 2),
    mean_perctreated = round(mean(perc_treated), 2),
    sd_perctreated = round(sd(perc_treated), 2)
  ) %>% 
  select(Group, everything())

summary_table_ta <- bind_rows(overall_avg_ta, summary_combined_ta) %>% 
  mutate(Group = ifelse(Group == "Control", "Comparison (Matched)", Group)) %>% 
  select(Group, everything()) %>% 
  #2 significant digits for all 
  mutate_at(vars(-Group), ~round(., 2)) %>%
  #traffic as integer/non negative
  mutate(mean_traffic = as.integer(mean_traffic), sd_traffic = as.integer(sd_traffic)) %>%
  rename(n = n_traffic) 

df_new_ta <- summary_table_ta
df_new_ta$mean_traffic <- paste0(summary_table_ta$mean_traffic, " (", summary_table_ta$sd_traffic, ")")
df_new_ta$mean_percblack <- paste0(summary_table_ta$mean_percblack, " (", summary_table_ta$sd_percblack, ")")
df_new_ta$mean_hisp <- paste0(summary_table_ta$mean_hisp, " (", summary_table_ta$sd_hisp, ")")
df_new_ta$mean_percpov <- paste0(summary_table_ta$mean_percpov, " (", summary_table_ta$sd_percpov, ")")
df_new_ta$mean_percemp <- paste0(summary_table_ta$mean_percemp, " (", summary_table_ta$sd_percemp, ")")
df_new_ta$mean_popn <- paste0(summary_table_ta$mean_popn, " (", summary_table_ta$sd_popn, ")")
df_new_ta$mean_jobs <- paste0(summary_table_ta$mean_jobs, " (", summary_table_ta$sd_jobs, ")")
df_new_ta$mean_transit <- paste0(summary_table_ta$mean_transit, " (", summary_table_ta$sd_transit, ")")
df_new_ta$mean_commercial <- paste0(summary_table_ta$mean_commercial, " (", summary_table_ta$sd_commercial, ")")
df_new_ta$mean_residential <- paste0(summary_table_ta$mean_residential, " (", summary_table_ta$sd_residential, ")")
df_new_ta$mean_manufacturing <- paste0(summary_table_ta$mean_manufacturing, " (", summary_table_ta$sd_manufacturing, ")")

df_new_ta <- df_new_ta %>% select(Group, n, n_zones, mean_traffic, mean_percblack, mean_hisp,  mean_percpov, mean_jobs, mean_popn, mean_transit, mean_commercial, mean_residential, mean_manufacturing) %>% 
  #rename columns
  rename("Tracts" = "Group", "N" = "n", "Census Tracts" = "n_zones", "Mean Traffic (MADT) (sd)" = "mean_traffic", "Proportionate Black - Mean (sd)*" = "mean_percblack", "Proportionate Hispanic - Mean (sd)*" = "mean_hisp", "Proportionate Below Poverty - Mean (sd)*" = "mean_percpov", "Proproportion Office-Based Employment - Mean (sd)*" = "mean_jobs", "Population - Mean (sd)" = "mean_popn", "Transit stops - Mean (sd)" = "mean_transit", "Percentage Commercial - Mean (sd)" = "mean_commercial", "Percentage Residential - Mean (sd)" = "mean_residential", "Percentage Manufacturing - Mean (sd)" = "mean_manufacturing") %>% 
  #reorder the rows putt 3rd as 2nd 
  mutate(Group = factor(Tracts, levels = c("Total", "Treated", "Comparison (Matched)"))) %>% 
  arrange(Group) %>% 
  select(-Group)

df_new_ta <- df_new_ta %>%
  #reorder the rows putt 3rd as 2nd 
  mutate(Group = factor(Tracts, levels = c("Total", "Treated", "Comparison (Matched)"))) %>% 
  arrange(Group) %>% 
  select(-Group)

#In long format 
df_long_ta <- t(df_new_ta) 

#save TA sensitivity analsis summary table as csv
write.csv(df_long_ta, "plots/ta_analysis_covariate_summary_table.csv")

# Using kable to display the table
summary_plot_ta<- kable(df_long_ta, align = 'r', format = "html") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(1, bold = TRUE) 

#View summary statistics table (TA sensitivity analysis)
summary_plot_ta

#Sensitivity Analysis Visual check for parallel trends (TA Matched Data)
#Plotting parallel trends-

trend_data_matched_ta <- data_matched_ta %>%
  group_by(op_st_active, my) %>% 
  summarise(mean_madt = mean(total_traf)) %>% 
  mutate(my = factor(my, levels = c("may19", "jun19",  "jul19", "aug19",  "may21", "jun21",  "jul21", "aug21"))) %>%
  mutate(op_st_active = as.factor(op_st_active)) 

trendplot_matched_ta <- trend_data_matched_ta %>% 
  ggplot() +
  geom_line(aes(x = my, y = mean_madt, group = op_st_active, color = op_st_active)) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "month-year", y = "mean daily average traffic (census tract)", title = "Visual check for parallel trends assumption (all traffic)") +
  scale_color_manual(values = c("black", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5), 
    axis.text = element_text(size = 12)
  )

trendplot_matched_ta

#save parallel trend plot (TA matched data)
ggsave("plots/parallentrends_matched_ta.png", trendplot_matched_ta, width = 6, height = 6, dpi = 300)

#Event study analysis (TA Matched Data)
matched_eventmodel_ta <- feols(
  total_traf ~ i(op_st_active, i.my, ref = 0, ref2 = "aug19") + i(my) + i(geoid),
  data = data_matched_ta, cluster = ~zone_name
)

summary(matched_eventmodel_ta)

coefs_event_ta <- broom::tidy(matched_eventmodel_ta) %>%
  # Select only event-study terms
  filter(str_detect(term, "op_st_active::1:my")) %>% 
  # Create year column
  mutate(term = substr(term, nchar(term) - 4, nchar(term))) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error)

# Add 0 for omitted month
coefs_event_ta <- coefs_event_ta %>%
  add_row(
    term = "aug19",
    estimate = 0,
    std.error = 0,
    conf.low = NA,
    conf.high = NA
  ) %>% 
  mutate(term = factor(term, levels = c("may19", "jun19", "jul19", "aug19", "may21", "jun21", "jul21", "aug21")))

ggplot(coefs_event_ta, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, color = "blue") +
  labs(x = "month-year (event time)", y = "Treatment Effect and 95% CI", title = "Event Study for Main Model - Active Streets Only") +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5)) 

#save event study plot - Matched/All Traffic 
ggsave("plots/matched_eventstudy_ta.png", width = 6, height = 6, dpi = 300)

#group by Group summarize 
summary_matched_ta <- data_matched_ta %>% group_by(Group) %>% summarize(n = n(), mean = mean(total_traf), pop = mean(pop_den), mean_employ = mean(perc_emp), mean_black = mean(perc_black), mean_hisp = mean(perc_hisp), mean_pov = mean(perc_pov), employ = mean(emp))

#Density Plot for Matched Data (TA)
matched_density_plot_ta <- ggplot(data_matched_ta, aes(x = distance, color = Group, linetype = Group)) +
  geom_density(aes(fill = Group), alpha = 0.2, size = 0.8, adjust = 1.5) +
  labs(title = " Density of Propensity Scores (Treated and Control Groups)",
       x = "Propensity Score (Distance)", y = "Density") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)) # Center title

#View density plot (TA)
matched_density_plot_ta

#DID Analysis Active Streets Only (Matched Data)

#DiD analysis (TA matched data) - Total Traffic
did_matched1_ta <- feols(
  total_traf ~ op_st_active:post | zone_name + my,
  data = data_matched_ta, 
  cluster = ~ geoid)
summary(did_matched1_ta)
tab_model(did_matched1_ta)

#  spillover variable - Matched data - Total Traffic 
did_matched2_ta <- feols(
  total_traf ~ op_st_active:post + perc_treated:post | zone_name + my,
  data = data_matched_ta, 
  cluster = ~ zone_name)
summary(did_matched2_ta)
tab_model(did_matched2_ta)

