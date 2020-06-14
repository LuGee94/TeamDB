#----------------<prepare>----------------------------------------------------------------------#
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("readxl")

library(tidyverse) # -> standard library for Data Science in R: Plots, data wrangling and modeling
library(lubridate) # -> For working with dates and time
library(readxl) # -> reading xlsx files

rm(list=ls())
#----------------</prepare>---------------------------------------------------------------------#



#----------------<read corona data>-------------------------------------------------------------#
#read csv-file corona time series  2020_05_29_CoronaData.csv
CoronaData <- read.csv("/Users/niklaswagner/Desktop/Datenquellen/Corona_Zeitreihen/2020_05_29_CoronaData.csv")
CoronaData <- as_tibble(CoronaData)
view(CoronaData)

#----------------</read corona data>------------------------------------------------------------#



#----------------<read additional data>---------------------------------------------------------#
#read Health_expenditure_per_Capita.xls
HealthExpenditure <- read_excel("/Users/niklaswagner/Desktop/Datenquellen/Gesundheitsausgaben pro Kopf/Health_expenditure_per_Capita.xls")
HealthExpenditure <- as_tibble(HealthExpenditure)
#View(HealthExpenditure)

#read HappynessIndex Index.xlsx
HappynessIndex <- read_excel("/Users/niklaswagner/Desktop/Datenquellen/Happyness_Index/Index.xlsx")
HappynessIndex <- as_tibble(HappynessIndex)
#View(HappynessIndex)

#read life expectancy - API_SP.DYN.LE00.IN_DS2_en_excel_v2_1120941.xls
LifeExpectancy <- read_excel("/Users/niklaswagner/Desktop/Datenquellen/Lebenserwartung pro Land/API_SP.DYN.LE00.IN_DS2_en_excel_v2_1120941.xls")
LifeExpectancy <- as_tibble(LifeExpectancy)
#View(LifeExpectancy)

#read API_SH.MED.NUMW.xlsx
Nurses_per_1000 <- read_excel("/Users/niklaswagner/Desktop/Datenquellen/Nurses per 1000/API_SH.MED.NUMW.xlsx")
Nurses_per_1000 <- as_tibble(Nurses_per_1000)
#View(Nurses_per_1000)

#read physicians/1000 API_SH.MED.PHYS.xlsx
Physicians_per_1000 <- read_excel("/Users/niklaswagner/Desktop/Datenquellen/Physicians per 1000/API_SH.MED.PHYS.xlsx")
Physicians_per_1000 <- as_tibble(Physicians_per_1000)
#View(Physicians_per_1000)
#----------------</read additional data>--------------------------------------------------------#



#----------------<prep corona data>-------------------------------------------------------------#
#read csv-file corona time series  2020_05_29_CoronaData.csv

# World contains sum, filter that out
CoronaData <- filter(CoronaData, location != "World")
CoronaData$date <- as.Date(CoronaData$date, format = "%Y-%m-%d")
#----------------</prep corona data>------------------------------------------------------------#



#----------------<prep additional data>---------------------------------------------------------#

#rename columns Health Expenditure to identify source of feature later
#delete column because we dont need it
names(HealthExpenditure)[names(HealthExpenditure) == "2018"] <- "Health_Expenditure_2018"
HealthExpenditure$'Country Name' <- NULL

#rename columns HappynessIndex to identify source of feature later
names(HappynessIndex)[names(HappynessIndex) == "Whisker-high"] <- "HI-Whisker-high"
names(HappynessIndex)[names(HappynessIndex) == "Whisker-low"] <- "HI-Whisker-low"
names(HappynessIndex)[names(HappynessIndex) == "Dystopia (1.88) + residual"] <- "HI-Dystopia (1.88) + residual"
names(HappynessIndex)[names(HappynessIndex) == "Explained by: Freedom to make life choices"] <- "HI-Explained by: Freedom to make life choices"
names(HappynessIndex)[names(HappynessIndex) == "Explained by: Generosity"] <- "HI-Explained by: Generosity"
names(HappynessIndex)[names(HappynessIndex) == "Explained by: GDP per capita"] <- "HI-Explained by: GDP per capita"
names(HappynessIndex)[names(HappynessIndex) == "Explained by: Perceptions of corruption"] <- "HI-Explained by: Perceptions of corruption"
names(HappynessIndex)[names(HappynessIndex) == "Explained by: Social support"] <- "HI-Explained by: Social support"
names(HappynessIndex)[names(HappynessIndex) == "Explained by: Healthy life expectancy"] <- "HI-Explained by: Healthy life expectancy"

#rename columns LifeExpectancy to identify source of feature later
names(LifeExpectancy)[names(LifeExpectancy) == "2019"] <- "Life-Expectancy 2019"
LifeExpectancy$'Country Name' <- NULL

#rename columns Nurses_per_1000 to identify source of feature later
names(Nurses_per_1000)[names(Nurses_per_1000) == "2019"] <- "Nurses/1000 2019"
Nurses_per_1000$'Country Name' <- NULL

#rename columns Physicians_per_1000 to identify source of feature later
names(Physicians_per_1000)[names(Physicians_per_1000) == "2019"] <- "Physicians/1000 2019"
Physicians_per_1000$'Country Name' <- NULL

#----------------</prep additional data>--------------------------------------------------------#



#----------------<Merge Corona Data with additional Data>---------------------------------------#

CoronaData_Joined <- as_tibble(CoronaData)
CoronaData_Joined <- CoronaData_Joined %>%
            left_join(HealthExpenditure, c("iso_code" = "Country Code"))  %>%
            left_join(HappynessIndex, c("location" = "Country")) %>%
            left_join(LifeExpectancy, c("iso_code" = "Country Code")) %>%
            left_join(Nurses_per_1000, c("iso_code" = "Country Code")) %>%
            left_join(Physicians_per_1000, c("iso_code" = "Country Code"))

view(CoronaData_Joined)

#----------------</Merge Corona Data with additional Data>--------------------------------------#


