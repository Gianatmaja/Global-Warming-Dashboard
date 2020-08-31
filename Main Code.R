#Global Temperature Data Set (Source: berkeleyearth.org/data/)
#Baseline of anomaly as follows:
#14.48 +/- 0.06 for high
#8.70 +/- 0.06 for mean
#3.00 +/- 0.07 for low

#Run 2 files/ source - go back to this later

library(ggplot2)
library(ggthemes)
library(plotly)
library(viridis)
library(RColorBrewer)
library(ggsci)


#Temperature
P1 = ggplot(data = GloTemp) + geom_line(aes(x = Year, y = Avg)) +
  geom_line(aes(x = Year, y = High)) + geom_line(aes(x = Year, y = Low)) + 
  theme_minimal()
P1
P2 = ggplot(data = GloTemp) + geom_point(aes(x = Year, y = Avg, size = Difference, col = Trend), alpha = I(4/5)) + 
  geom_smooth(aes(x = Year, y = Avg), col = "black", se = F, lty = 2) + theme_hc() +
  theme(axis.title = element_text(face = "bold")) +  xlab("Year\n") + 
  ylab("Temperature\n")  + scale_color_manual(values = brewer.pal(3,"RdYlBu")[c(1,3)])
P2
ggplotly(P2) %>% 
  layout(title = list(text = paste0("Average Global Land Temperature (1850 - 2013)", "<br>", "<sup>", "Measured in degrees celcius; Raw data source: berkeleyearth.org/data/", "</sup>"))) %>%
  layout(title = list(orientation = "h", x = 0.01, y = 0.95)) %>%
  layout(legend = list(orientation = "h", x = 0.3, y = -0.25))

#Carbon dioxide
P3 = ggplot(data = CO2.Emi.continent, aes(x = year, y = total, fill = continent, group = continent)) + 
  geom_area() + scale_x_discrete(breaks = seq(1860,2010,20)) +
  theme_minimal() + xlab("Year\n") + ylab("Total\n") + labs(fill = "Continent") +
  theme(axis.title = element_text(face = "bold")) + scale_fill_npg()
P3
ggplotly(P3) %>% 
  layout(title = list(text = paste0("CO2 Emissions (1860 - 2014)", "<br>", "<sup>", "Measured in kilotonnes; Raw data source: gapminder.org/data/", "</sup>"))) %>%
  layout(title = list(orientation = "h", x = 0.01, y = 0.95)) %>%
  layout(legend = list(orientation = "h", x = 0.08, y = -0.25))

#Methane
P4 = ggplot(data = Methane.Emi.continent, aes(x = year, y = total, col = continent, group = 1)) + 
  geom_point() + scale_x_discrete(breaks = seq(1860,2010,20)) +
  xlab("Year\n") + ylab("Total\n") + labs(fill = "Continent") +
  theme(axis.title = element_text(face = "bold"))
P4
ggplotly(P4)

#NO
P5 = ggplot(data = NO_Emi.continent) + geom_line(aes(x = year, y = total, col = continent, group = continent)) + 
  scale_x_discrete(breaks = seq(1860,2010,20)) +
  xlab("Year\n") + ylab("Total\n") + labs(fill = "Continent") +
  theme(axis.title = element_text(face = "bold"))
P5

#NO and CH4
P6 = ggplot(data = NO_CH4_Year) + geom_line(aes(x = Year, y = Emission, col = Gas, group = Gas)) +
  scale_x_discrete(breaks = seq(1970,2010,10)) + scale_y_discrete(breaks = seq(2000000,8000000,2000000)) +
  xlab("Year\n") + ylab("Emission\n") + theme_excel_new()
P6
ggplotly(P6)%>% 
  layout(title = list(text = paste0("Methane & NO Emissions (1979 - 2012)", "<br>", "<sup>", "Measured in kilotonnes; Raw data source: data.worldbank.org/", "</sup>"))) %>%
  layout(title = list(orientation = "h", x = 0.01, y = 0.95)) %>%
  layout(legend = list(orientation = "h", x = 0.08, y = -0.25))

#Sea level (global)
P7 = ggplot(data = Mean_Sea_Level) + geom_bar(aes(x = Year, y = Cumulative_change), stat = "identity", fill = "lightblue2") +
  geom_line(aes(x = Year, y = Cumulative_change), col = "red3") + geom_point(aes(x = Year, y = Cumulative_change, One_year_rise = One_year_rise)) + xlab("Year\n") + ylab("Change\n") + theme_stata()
P7
ggplotly(P7) %>% 
  layout(title = list(text = paste0("Global Cumulative Sea Level Change (1993 - 2015)", "<br>", "<sup>", "Measured in mm; Raw data source: datahub.io/", "</sup>"))) %>%
  layout(title = list(orientation = "h", x = 0.01, y = 0.97))

#Extreme temperature
P8 = ggplot(data = ET_Final) + geom_bar(aes(x = Period, y = Number, Percentage = Percentage, fill = Category, group = Category), stat = "identity", alpha = I(3/5)) +
   ylab("Number of countries\n") + scale_fill_futurama() + theme_pander()
P8
ggplotly(P8) %>% 
  layout(title = list(text = paste0("Number of Countries Reporting Extreme Temperature", "<br>", "<sup>", "Raw data source: gapminder.org/data/", "</sup>"))) %>%
  layout(title = list(orientation = "h", x = -0.03, y = 0.975)) %>%
  layout(legend = list(orientation = "h", x = 0.27, y = -0.2))

#Forest area
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(maps)

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", macolour = "white")

# Retrievethe map data
some.maps <- map_data("world", region = Country)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

left_join(region.lab.data)

ggplot(some.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")


region_map <- map_data("world")
forest_map <- left_join(region_map, Forest_data, by = "region")
typeof(forest_map$Forest_change)

ggplot(forest_map)+
  geom_polygon(aes(x = long, y = lat, group = Change, fill = Change), col = "white") +
  scale_fill_viridis_c(option = "D")

dim(forest_map)






