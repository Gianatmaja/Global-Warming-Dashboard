# Global-Warming-Dashboard
An informative dashboard on global warming, made using R's ggplot2, plotly, and flexdashboard.

This dashboard is created to show the trends in various aspects of global warming, such as the rise
in global land temperature, sea level, emissions of the greenhouse gases and the reported extreme
weather events.

The dashboard is published on RPubs and can be accessed [here](https://rpubs.com/Ga25/654326).

## Note
There are five datasets used for the dashboard. They are:
- AverageGlobalTemp.txt
- CO2_Emissions.csv
- Sea_level_GMSL.csv
- Methane_Emissions.csv
- NO_Emissions.csv
- Extreme_temp.csv

The codes to knit the dashboard is in the Dashboard.Rmd file while the codes for the plotting alone 
is in the Rcode_only.R file. The packages used are: 
- dplyr, tibble, countrycode, tidyr (For data wrangling)
- ggplot2, ggthemes, plotly, viridis, RColorBrewer, ggsci (For visualisation)
- flexdashboard (to form a dashboard)
