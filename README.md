# Assessing Air Quality in Lombardia, Italy through Time Series Analysis: Implications for Public Health and Policy

<div style="text-align: center;">
    <img src="assets/air_quality_cover_landscape.jpeg" alt="repo_cover" style="width:70%">
</div>

## Description
This project aims to evaluate the air quality in Lombardia, Italy, using a comprehensive time series analysis. The data for this study will be derived from sensors located at various stations across the region, which can be accessed via the regional website.

The primary focus is to underscore the significance of air quality on public health. By analyzing the trends and patterns in air quality data over time, it's possible to identify periods of high pollution and correlate these with potential health risks. This analysis will provide valuable insights into how air quality fluctuations may impact the respiratory health of the region's inhabitants.

Furthermore, the project seeks to utilize these findings to inform policy-making. By identifying key contributors to poor air quality and periods of high pollution, targeted measures can be proposed to improve air quality. This could include suggestions for traffic management, industrial regulations, or public health initiatives. The time series analysis approach allows for a dynamic understanding of air quality trends, making it a valuable tool for predicting future scenarios and planning proactive measures.

## Data
The data for this project are retrieved using the <a href="https://cran.r-project.org/web/packages/ARPALData/index.html
">ARPALData</a> R package, which is specifically designed for accessing, managing, and analyzing air quality and weather data from Regione Lombardia's open database (<https://www.dati.lombardia.it/>). This data is collected by ARPA Lombardia (Lombardia Environmental Protection Agency), Italy, through its extensive ground monitoring network (<https://www.arpalombardia.it/>).

The data quality, including aspects such as missing values, exported values, and graphical mapping, has been thoroughly checked in collaboration with members of the ARPA Lombardia's office for air quality control. The package provides access to historical data since 1989 for weather variables and 1968 for air quality variables, with updates occurring on a daily basis by the regional agency.

The meteorological variables available for analysis include temperature (in Celsius degrees), rainfall (in mm), wind speed (in m/s), wind direction (in degrees), and relative humidity.

In terms of airborne pollutant concentrations, the dataset includes measurements for NO2, NOx, PM10, PM2.5, Ozone, Arsenic, Benzene, Benzo-a-pirene, Ammonia, Sulfur Dioxide, Black Carbon, CO, Nikel, Cadmium, and Lead.

Additionally, information on both station types, including their location and altitude, is available.


## References
```
@article{maranzano2024arpaldata,
  title={ARPALData: an R package for retrieving and analyzing air quality and weather data from ARPA Lombardia (Italy)},
  author={Maranzano, Paolo and Algieri, Andrea},
  journal={Environmental and Ecological Statistics},
  pages={1--32},
  year={2024},
  publisher={Springer}
}
```
