#Data Cleaaning file should be run first

#Global Temperature Data Set (Source: berkeleyearth.org/data/)
#Baseline of anomaly as follows:
#14.48 +/- 0.06 for high
#8.70 +/- 0.06 for mean
#3.00 +/- 0.07 for low

library(dplyr)
library(tibble)
library(countrycode)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(viridis)
library(RColorBrewer)
library(ggsci)

#Global Land Temp
#Wrangling
which(AvgGloTemp$Year >= 1850)[1]
which(HighGloTemp$Year >= 1850)[1]
which(LowGloTemp$Year >= 1850)[1]

dim(AvgGloTemp)[1]
dim(HighGloTemp)[1]
dim(LowGloTemp)[1]

AvgGloTemp = AvgGloTemp[1275:3239,]
HighGloTemp = HighGloTemp[205:2168,]
LowGloTemp = LowGloTemp[205:2168,]

head(AvgGloTemp)
head(HighGloTemp)
head(LowGloTemp)

AvgGloTemp_yearly =  AvgGloTemp %>% group_by(Year) %>% summarise(Avg = mean(Anomaly))
HighGloTemp_yearly =  HighGloTemp %>% group_by(Year) %>% summarise(High = mean(Anomaly))
LowGloTemp_yearly =  LowGloTemp %>% group_by(Year) %>% summarise(Low = mean(Anomaly))

AvgGloTemp_yearly$Avg = AvgGloTemp_yearly$Avg + 8.7
HighGloTemp_yearly$High = HighGloTemp_yearly$High + 14.48
LowGloTemp_yearly$Low = LowGloTemp_yearly$Low + 3

AvgGloTemp_yearly
HighGloTemp_yearly
LowGloTemp_yearly

GloTemp = left_join(AvgGloTemp_yearly, HighGloTemp_yearly, by = "Year") %>%
  left_join(., LowGloTemp_yearly, by = "Year")
GloTemp = AvgGloTemp_yearly

length(GloTemp$Avg)
Increase = GloTemp$Avg[2:164] - GloTemp$Avg[1:163]
GloTemp$Difference = c(0,Increase)
for (i in 1:164){
  if(GloTemp$Difference[i] <= 0){
    GloTemp$Trend[i] = "Not increasing"
  }else{
    GloTemp$Trend[i] = "Increasing"
  }
}
GloTemp$Difference = abs(GloTemp$Difference)
GloTemp

#Plot
P1 = ggplot(data = GloTemp) + geom_line(aes(x = Year, y = Avg)) +
  geom_line(aes(x = Year, y = High)) + geom_line(aes(x = Year, y = Low)) + 
  theme_minimal()
P1
P2 = ggplot(data = GloTemp) + geom_point(aes(x = Year, y = Avg, size = Difference, col = Trend), alpha = I(4/5)) + 
  geom_smooth(aes(x = Year, y = Avg), col = "black", se = F, lty = 2) + theme_hc() +
  theme(axis.title = element_text(face = "bold")) +  xlab("Year\n") + 
  ylab("Temperature\n")  + scale_color_manual(values = brewer.pal(3,"RdYlBu")[c(1,3)])
P2
Plot1 = ggplotly(P2) %>% 
  layout(title = list(text = paste0("Average Global Land Temperature (1850 - 2013)", "<br>", "<sup>", "Measured in degrees celcius; Raw data source: berkeleyearth.org/data/", "</sup>"))) %>%
  layout(title = list(orientation = "h", x = 0.01, y = 0.95)) %>%
  layout(legend = list(orientation = "h", x = 0.3, y = -0.25))

#Sea Level
#Wrangling
Month = rep(1:12,23)
Month = Month[1:266]
Year = rep(1993:2015,each=12)
Year = Year[1:266]
Mean_Sea_Level = data.frame(cbind(Year, Month, Sea_Level$GMSL))
names(Mean_Sea_Level) = c("Year", "Month", "GMSL")
Mean_Sea_Level = Mean_Sea_Level %>% group_by(Year) %>% summarise(Change = last(GMSL))
Rise = Mean_Sea_Level$Change - Mean_Sea_Level$Change[c(1,1:22)]
Mean_Sea_Level = data.frame(cbind(Mean_Sea_Level$Year, Mean_Sea_Level$Change, Rise))
Mean_Sea_Level$Rise[1] = NA
names(Mean_Sea_Level) = c("Year", "Cumulative_change", "One_year_rise")

#Plot
#Sea level (global)
P2 = ggplot(data = Mean_Sea_Level) + geom_bar(aes(x = Year, y = Cumulative_change), stat = "identity", fill = "steelblue2") +
  geom_line(aes(x = Year, y = Cumulative_change), col = "royalblue4") + geom_point(aes(x = Year, y = Cumulative_change, One_year_rise = One_year_rise)) + xlab("Year\n") + ylab("Change\n") + theme_minimal()
P2
Plot2 = ggplotly(P2) %>% 
  layout(title = list(text = paste0("Global Cumulative Sea Level Change (1993 - 2015)", "<br>", "<sup>", "Measured in mm; Raw data source: datahub.io/", "</sup>"))) %>%
  layout(title = list(orientation = "h", x = 0.01, y = 0.97))

#Carbon dioxide data
#Wrangling
CO2.Emi.2 = CO2.Emi[,c(1,111:265)]
dim(CO2.Emi)
head(CO2.Emi.2)
CO2.Emi.2[,1:10]

CO2.Emi.2$continent = countrycode(sourcevar = CO2.Emi.2[,"country"], origin = "country.name", destination = "continent")
dim(CO2.Emi.2)
CO2.Emi.2 = CO2.Emi.2[,c(1,157,2:156)]

CO2.Emi.tidy = CO2.Emi.2 %>%
  gather(year, emission, '1860':'2014')

CO2.Emi.tidy[is.na(CO2.Emi.tidy)] = 0
CO2.Emi.continent = CO2.Emi.tidy %>%  group_by(year, continent) %>% summarise(total = sum(emission))

#Plot
#Carbon dioxide
P3 = ggplot(data = CO2.Emi.continent, aes(x = year, y = total, fill = continent, group = continent)) + 
  geom_area() + scale_x_discrete(breaks = seq(1860,2010,20)) +
  theme_minimal() + xlab("Year\n") + ylab("Total\n") + labs(fill = "Continent") +
  theme(axis.title = element_text(face = "bold")) + scale_fill_npg()
P3
Plot3 = ggplotly(P3) %>% 
  layout(title = list(text = paste0("CO2 Emissions (1860 - 2014)", "<br>", "<sup>", "Measured in kilotonnes; Raw data source: gapminder.org/data/", "</sup>"))) %>%
  layout(title = list(orientation = "h", x = 0.01, y = 0.95)) %>%
  layout(legend = list(orientation = "h", x = 0.08, y = -0.25))

#Methane & NO
#Wrangling
#Methane data
head(Methane.Emi)
dim(Methane.Emi)

Methane.Emi$continent = countrycode(sourcevar = Methane.Emi[,"Country Name"], origin = "country.name", destination = "continent")
dim(Methane.Emi)
Methane.Emi = Methane.Emi[,c(1,66,2:65)]

Index = which(is.na(Methane.Emi$continent) == T)
Methane.Emi = Methane.Emi[-Index,]

Methane.Emi.tidy = Methane.Emi %>%
  gather(year, emission, '1960':'2019')
head(Methane.Emi.tidy)
Methane.Emi.tidy = Methane.Emi.tidy[,c(1,2,7,8)]

Methane.Emi.tidy[is.na(Methane.Emi.tidy)] = 0

Methane.Emi.tidy = Methane.Emi.tidy %>% group_by(year) %>% summarise(emission = sum(emission))
Methane.Emi.tidy = Methane.Emi.tidy[11:53,]

#NO data
head(NO_Emi)
dim(NO_Emi)

NO_Emi$continent = countrycode(sourcevar = NO_Emi[,"Country Name"], origin = "country.name", destination = "continent")
dim(NO_Emi)
NO_Emi = NO_Emi[,c(1,66,2:65)]

Index2 = which(is.na(NO_Emi$continent) == T)
NO_Emi = NO_Emi[-Index2,]

NO_Emi.tidy = NO_Emi %>%
  gather(year, emission, '1960':'2019')
head(NO_Emi.tidy)
NO_Emi.tidy = NO_Emi.tidy[,c(1,2,7,8)]

NO_Emi.tidy[is.na(NO_Emi.tidy)] = 0

NO_Emi.tidy = NO_Emi.tidy %>% group_by(year) %>% summarise(emission = sum(emission))
NO_Emi.tidy = NO_Emi.tidy[11:53,]

#Joined
A = Methane.Emi.tidy$emission
B = NO_Emi.tidy$emission

X = rep(1970:2012,each = 2)
Y = vector("numeric", 86)
Z = rep(c("Methane","NO"), 43)

for (i in 1:86){
  if(i %% 2 == 0){
    a = i/2
    Y[i] = B[a]
  }else{
    a = (i+1)/2
    Y[i] = A[a]
  }
}

NO_CH4_Year = cbind(X,Y,Z)
NO_CH4_Year = data.frame(NO_CH4_Year)
names(NO_CH4_Year) = c("Year","Emission","Gas")

#Plot
#Methane
#NO and CH4
P6 = ggplot(data = NO_CH4_Year) + geom_line(aes(x = Year, y = Emission, col = Gas, group = Gas)) +
  scale_x_discrete(breaks = seq(1970,2010,10)) + scale_y_discrete(breaks = seq(2000000,8000000,2000000)) +
  xlab("Year\n") + ylab("Emission\n") + theme_excel_new()
P6
Plot4 = ggplotly(P6)%>% 
  layout(title = list(text = paste0("Methane & NO Emissions (1979 - 2012)", "<br>", "<sup>", "Measured in kilotonnes; Raw data source: data.worldbank.org/", "</sup>"))) %>%
  layout(title = list(orientation = "h", x = 0.01, y = 0.95)) %>%
  layout(legend = list(orientation = "h", x = 0.08, y = -0.25))

#Extreme temperature
#Wrangling
head(Extreme_temp)
dim(Extreme_temp)
Extreme_temp_matrix = matrix(0,68,2)
for (i in 1:39){
  Index3 = which(is.na(Extreme_temp[,i]) == T)
  Extreme_temp[Index3,i] = 0
}
for (j in 1:68){
  Extreme_temp_matrix[j,1] = sum(Extreme_temp[j,2:30])
}
for (j in 1:68){
  Extreme_temp_matrix[j,2] = sum(Extreme_temp[j,31:39])
}
A = length(which(Extreme_temp_matrix[,1] == 0))
B = length(which(Extreme_temp_matrix[,2] == 0))
A;B
Count_ET_matrix = matrix(c(A,B,68-A,68-B), nrow = 2, ncol = 2, byrow = T)
#first row = countries not reporting (1971-1999 for 1st column and 2000-2008 for 2nd column)
#second row = countries reporting (1971-1999 for 1st column and 2000-2008 for 2nd column)

Period = rep(c("1971-1999","2000-2008"),each = 2)
Number = c(35,33,57,11)
Prop = c(35/68,33/68,57/68,11/68)
Percentage = round(Prop*100,0)
Category = rep(c("Reporting", "Not reporting"),2)
ET_Final = data.frame(cbind(Period, Number, Percentage, Category))
ET_Final$Number = as.integer(as.character(ET_Final$Number))

#Plot
#Extreme temperature
P8 = ggplot(data = ET_Final) + geom_bar(aes(x = Period, y = Number, Percentage = Percentage, fill = Category, group = Category), stat = "identity", alpha = I(3/5)) +
  ylab("Number of countries\n") + scale_fill_futurama() + theme_pander()
P8
Plot5 = ggplotly(P8) %>% 
  layout(title = list(text = paste0("Number of Countries Reporting Extreme Temperature", "<br>", "<sup>", "Raw data source: gapminder.org/data/", "</sup>"))) %>%
  layout(title = list(orientation = "h", x = -0.03, y = 0.975)) %>%
  layout(legend = list(orientation = "h", x = 0.27, y = -0.2))



