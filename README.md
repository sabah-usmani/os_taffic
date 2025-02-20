# NYC_open_streets_and_traffic

This repository contains the code for the analysis of the open streets impact on traffic volume in New York City. The traffic data (simulated) provided is for code review purposes only. 

## Scripts

The repository contains the following scripts:
1) prepare_traffic_data.R: This script prepares the traffic data for analysis. It reads the compiles and cleans the data. It uploads both start_end and pass through traffic data and combines and cleans this data at the census tract level for nyc.
2) prepare_covariate_data.R: This script prepares the covariate data for analysis. It reads the compiles and cleans the data. It uploads the covariate data and combines and cleans this data at the census tract level for nyc. 
2) did_analysis.R: This script performs the analysis on the data. 
3) sensitivity_analysis.R: This script performs the sensitivity analysis (Generalized Additive Mixed Model) on the data.


