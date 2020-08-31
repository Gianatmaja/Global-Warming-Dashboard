#Mean, high, and low monthly anomalies of average global temperature
#Source: berkeleyearth.org/data/
#Baseline of anomaly as follows:
#14.48 +/- 0.06 for high
#8.70 +/- 0.06 for mean
#3.00 +/- 0.07 for low

#Scatter plot

AvgGloTemp = read.table("AverageGlobalTemp.txt", sep = "", skip = 69)  
AvgGloTemp = AvgGloTemp[,1:3]
names(AvgGloTemp) = c("Year","Month","Anomaly")
head(AvgGloTemp)

HighGloTemp = read.table("HighGlobalTemp.txt", sep = "", skip = 69)  
HighGloTemp = HighGloTemp[,1:3]
names(HighGloTemp) = c("Year","Month","Anomaly")
head(HighGloTemp)

LowGloTemp = read.table("LowGlobalTemp.txt", sep = "", skip = 69)  
LowGloTemp = LowGloTemp[,1:3]
names(LowGloTemp) = c("Year","Month","Anomaly")
head(LowGloTemp)

#Global mean sea level
#Source: datahub.io/
#Cumulative change since 1880, measured in mm

Sea_Level = read.csv("Sea_Level_GMSL.csv", header = T, check.names = F)
Sea_Level

#Extreme temperature
#Source: gapminder.org/data/
#Units: People affected
Extreme_temp = read.csv("Extreme_temp.csv", skip = 1, header = F,check.names = F)
Extreme_temp

#Yearly CO2 emissions by country from burning of fossil fuels
#Source: gapminder.org/data/
#Note: in 1000 metric tonnes of CO2

CO2.Emi = read.csv("CO2_Emissions.csv", header = T, check.names = F)
head(CO2.Emi)
dim(CO2.Emi)

#Yearly methane and nitrous oxide emissions by country
#Source: data.worldbank.org/
#Note: in kilo tonnes of CO2 equivalent

Methane.Emi = read.csv("Methane_Emissions.csv",skip = 4, header = T, check.names = F)
head(Methane.Emi)
dim(Methane.Emi)

NO_Emi = read.csv("NO_Emissions.csv", skip = 4, header = T, check.names = F)
head(NO_Emi)
dim(NO_Emi)


#Forest Total Area (in ha)
#Source: gapminder.org/data/
#Note: includes land > 0.5 ha, trees > 5m, canopy cover > 10%
#Note: does not include land used for agricultural or urban area

#Map of world

ForestLand = read.csv("Forest_Area.csv", header = T, check.names = F)
head(ForestLand)
dim(ForestLand)




