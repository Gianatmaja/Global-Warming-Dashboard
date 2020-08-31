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

#Sea Level
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

#Carbon dioxide data
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

#Extreme temperature
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

#Forest data
ForestLand
Forest_change = (ForestLand$`2015` - ForestLand$`1990`)/ForestLand$`1990`
Index4 = which(is.na(Forest_change) == T)
Forest_change[Index4] = 0

Change =rep(0,192)
for(a in 1:length(Forest_change)){
  Test = Forest_change[a]
  if(Test<0){
    Change[a] = "Decreasing"
  }else{
    Change[a] = "Not decreasing"
  }
}

region = as.character(ForestLand$country)
Change = as.character(Change)
Forest_change = as.numeric(Forest_change)
Forest_data = data.frame(cbind(region, Forest_change, Change))
dim(Forest_data)





