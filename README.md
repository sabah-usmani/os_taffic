# NYC_open_streets_and_traffic

This repository contains the code for the analysis of the open streets impact on traffic volume in New York City. The traffic data (simulated) provided is for code review purposes only. Open Streets locations and select covariate data (downloaded from NYC Open Data) are available on GitHub and via NYC Open Data (NYC Open Data, 2021). ACS data are publicly available and were obtained from the U.S. Census Bureau (US Census Bureau, 2018-2022). Employment data were sourced from the U.S. Census Bureau’s LEHD Program website (US Census Bureau, 2019). Zoning data is available through NYC’s Zoning and Land Use Map (NYC Planning, 2024). StreetLight traffic data are available upon request from StreetLight (StreetLight, 2021). All relevant code used for analysis is available on GitHub.

## Scripts

The repository contains the following scripts:
1) prepare_traffic_data.R: This script prepares the traffic data for analysis. It reads the compiles and cleans the data. It uploads both start_end and pass through traffic data and combines and cleans this data at the census tract level for nyc.
2) prepare_covariate_data.R: This script prepares the covariate data for analysis. It reads the compiles and cleans the data. It uploads the covariate data and combines and cleans this data at the census tract level for nyc. 
2) did_analysis.R: This script performs the analysis on the data. 
3) sensitivity_analysis.R: This script performs the sensitivity analysis (Generalized Additive Mixed Model) on the data.


