#----------------<prepare>----------------------------------------------------------------------#
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("readxl")
#install.packages("psych")
#install.packages("dplyr")
install.packages("ggdendro")
install.packages("cluster")
install.packages("broom")
install.packages("ggplot2")

library(tidyverse) # -> standard library for Data Science in R: Plots, data wrangling and modeling
library(lubridate) # -> For working with dates and time
library(readxl) # -> reading xlsx files
library(psych)
library(dplyr)
library(ggdendro)
library(cluster)
library(broom)
library(ggplot2)

rm(list=ls())
#----------------</prepare>---------------------------------------------------------------------#



#----------------<read corona data>-------------------------------------------------------------#
#read csv-file corona time series  2020_05_29_CoronaData.csv
CoronaData <- read.csv("/Users/lukasgerspers/Google Drive/STUDIUM/Master/4. Semester/Data Science 1/DS1 Project/Datenquellen/Corona_Zeitreihen/2020_05_29_CoronaData.csv")
CoronaData <- as_tibble(CoronaData)
#view(CoronaData)

#----------------</read corona data>------------------------------------------------------------#



#----------------<read additional data>---------------------------------------------------------#
#read Health_expenditure_per_Capita.xls
HealthExpenditure <- read_excel("/Users/lukasgerspers/Google Drive/STUDIUM/Master/4. Semester/Data Science 1/DS1 Project/Datenquellen/Gesundheitsausgaben pro Kopf/Health_expenditure_per_Capita.xls")
HealthExpenditure <- as_tibble(HealthExpenditure)
#View(HealthExpenditure)

#read HappynessIndex Index.xlsx
HappynessIndex <- read_excel("/Users/lukasgerspers/Google Drive/STUDIUM/Master/4. Semester/Data Science 1/DS1 Project/Datenquellen/Happyness_Index/Index.xlsx")
HappynessIndex <- as_tibble(HappynessIndex)
#View(HappynessIndex)

#read life expectancy - API_SP.DYN.LE00.IN_DS2_en_excel_v2_1120941.xls
LifeExpectancy <- read_excel("/Users/lukasgerspers/Google Drive/STUDIUM/Master/4. Semester/Data Science 1/DS1 Project/Datenquellen/Lebenserwartung pro Land/API_SP.DYN.LE00.IN_DS2_en_excel_v2_1120941.xls")
LifeExpectancy <- as_tibble(LifeExpectancy)
#View(LifeExpectancy)

#read API_SH.MED.NUMW.xlsx
Nurses_per_1000 <- read_excel("/Users/lukasgerspers/Google Drive/STUDIUM/Master/4. Semester/Data Science 1/DS1 Project/Datenquellen/Nurses per 1000/API_SH.MED.NUMW.xlsx")
Nurses_per_1000 <- as_tibble(Nurses_per_1000)
#View(Nurses_per_1000)

#read physicians/1000 API_SH.MED.PHYS.xlsx
Physicians_per_1000 <- read_excel("/Users/lukasgerspers/Google Drive/STUDIUM/Master/4. Semester/Data Science 1/DS1 Project/Datenquellen/Physicians per 1000/API_SH.MED.PHYS.xlsx")
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

popdensity <- select(CoronaData_Joined, date, total_deaths)
ggplot(popdensity, aes(date, total_deaths)) + geom_point()



#----------------<Transform and Calculate Corona Data>------------------------------------------#

#obsolete Feature
CoronaData$'test_units' <- NULL


#create columns to learn something about infection boundary -100 -1000 - 10000 -100000
Running_CoronaData <- CoronaData_Joined %>%
  group_by(location) %>%
  mutate( cases100= case_when(total_cases < 100 ~ "<100",
                              total_cases >= 100  ~ ">=100")) %>%
  group_by(location, cases100) %>%
  mutate( day_from_100th_infection= case_when(cases100 == "<100" ~ as.integer(0),
                                              cases100 == ">=100"  ~ row_number())) %>%
  mutate( cases1000= case_when(total_cases < 1000 ~ "<1000",
                               total_cases >= 1000 ~ ">=1000")) %>%
  group_by(location, cases1000) %>%
  mutate( day_from_1000th_infection = case_when(cases1000 == "<1000" ~ as.integer(0),
                                                cases1000 == ">=1000" ~ row_number())) %>%
  mutate( cases10000= case_when(total_cases < 10000 ~ "<10000",
                                total_cases >= 10000 ~ ">=10000")) %>%
  group_by(location, cases10000) %>%
  mutate( day_from_10000th_infection = case_when(cases10000 == "<10000" ~ as.integer(0),
                                                 cases10000 == ">=10000" ~ row_number())) %>%
  mutate( cases100000= case_when(total_cases < 100000 ~ "<100000",
                                 total_cases >= 100000 ~ ">=100000")) %>%
  group_by(location, cases100000) %>%
  mutate( day_from_100000th_infection = case_when(cases100000 == "<100000" ~ as.integer(0),
                                                  cases100000 == ">=100000" ~ row_number()))

#get max date corona dataset
max_data_corona_dataset <- max(Running_CoronaData$date)

#filter data to the most actual date
current_coronadata <- filter(Running_CoronaData, date == max_data_corona_dataset)
#remove data <= 100 infections
current_coronadata <- filter(current_coronadata, day_from_100th_infection != 0)


#get the range in days between 100 and 1000 infections
CoronaData_1000 <- filter(Running_CoronaData, day_from_1000th_infection == 1) %>%
  ungroup() %>%
  select(location, day_from_100th_infection)
names(CoronaData_1000)[names(CoronaData_1000) == "day_from_100th_infection"] <- "days_100_to_1000_infections"
View(CoronaData_1000)

#get the range in days between 100 and 10000 / 1000 and 10000 infections
CoronaData_10000<- filter(Running_CoronaData, day_from_10000th_infection == 1) %>%
  ungroup() %>%
  select(location, day_from_100th_infection, day_from_1000th_infection)
names(CoronaData_10000)[names(CoronaData_10000) == "day_from_100th_infection"] <- "days_100_to_10000_infections"
names(CoronaData_10000)[names(CoronaData_10000) == "day_from_1000th_infection"] <- "days_1000_to_10000_infections"

#get the range in days between 100 and 100000 / 1000 and 100000 / 10000 and 100000 infections
CoronaData_100000<- filter(Running_CoronaData, day_from_100000th_infection == 1) %>%
  ungroup() %>%
  select(location, day_from_100th_infection, day_from_1000th_infection, day_from_10000th_infection)
names(CoronaData_100000)[names(CoronaData_100000) == "day_from_100th_infection"] <- "days_100_to_100000_infections"
names(CoronaData_100000)[names(CoronaData_100000) == "day_from_1000th_infection"] <- "days_1000_to_100000_infections"
names(CoronaData_100000)[names(CoronaData_100000) == "day_from_10000th_infection"] <- "days_10000_to_100000_infections"

#join the ranges between the different boundaries to out current_date_data
current_coronadata <- current_coronadata %>%
  left_join(CoronaData_1000, c("location" = "location"))  %>%
  left_join(CoronaData_10000, c("location" = "location"))  %>%
  left_join(CoronaData_100000, c("location" = "location"))  

attach(current_coronadata)


view(current_coronadata)

#filter cases >= 10000 to select only significant countries with an impact on following regression
#most countries with cases <= 10000 are small countries where we have NA-Values
current_coronadata_filter_10000 <- filter(current_coronadata, total_cases >= 10000)


#filter irrelevant columns out

current_coronadata_filter_10000$'tests_units' <- NULL
current_coronadata_filter_10000$'total_tests' <- NULL
current_coronadata_filter_10000$'new_tests' <- NULL
current_coronadata_filter_10000$'total_tests_per_thousand' <- NULL
current_coronadata_filter_10000$'new_tests_per_thousand' <- NULL
current_coronadata_filter_10000$'new_tests_smoothed' <- NULL
current_coronadata_filter_10000$'new_tests_smoothed_per_thousand' <- NULL
current_coronadata_filter_10000$'stringency_index' <- NULL
current_coronadata_filter_10000$'aged_70_older' <- NULL
current_coronadata_filter_10000$'extreme_poverty' <- NULL
current_coronadata_filter_10000$'handwashing_facilities' <- NULL
current_coronadata_filter_10000$'cases100' <- NULL
current_coronadata_filter_10000$'cases1000' <- NULL
current_coronadata_filter_10000$'cases10000' <- NULL
current_coronadata_filter_10000$'cases100000' <- NULL

current_coronadata_filter_10000$procentual_cases_population <- ( current_coronadata_filter_10000$total_cases / current_coronadata_filter_10000$population ) * 100

current_coronadata_filter_10000$population <- NULL

view(current_coronadata_filter_10000)



#-------------------#
test2 <- select(current_coronadata_filter_10000, location, total_deaths_per_million, total_cases_per_million) %>%
group_by(location)
show(test2)

ggplot(test1, aes(total_cases_per_million, total_deaths_per_million)) + geom_point()

h.cluster <- test2 %>% 
dist(., method = "euclidean") %>% 
hclust(., method = "ward.D")
ggdendrogram(h.cluster)


multi.clust <- data.frame(k = 1:6) %>%
group_by(k) %>%
do(clust = kmeans(test2, .$k)) 

sumsq.clust <- multi.clust %>%
group_by(k) %>%
do(glance(.$clust[[1]]))

ggplot(sumsq.clust, aes(k, tot.withinss)) + geom_line() + geom_point()

