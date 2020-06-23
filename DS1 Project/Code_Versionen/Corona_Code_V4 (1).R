#----------------<prepare>----------------------------------------------------------------------#
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("readxl")
#install.packages("psych")
#install.packages("factoextra")
#install.packages("dbscan")
#install.packages("data.table")


library(tidyverse) # -> standard library for Data Science in R: Plots, data wrangling and modeling
library(lubridate) # -> For working with dates and time
library(readxl) # -> reading xlsx files
library(psych) # -> cluster analyse
library(factoextra) # -> clustering algorithms & visualization
library(dbscan) # -> librarby for dbscan
library(data.table)
library(dplyr) # -> visualization
library(utils)

rm(list=ls())
options(scipen = 5)

#----------------</prepare>---------------------------------------------------------------------#



#----------------<read corona data>-------------------------------------------------------------#
#read csv-file corona time series  2020_05_29_CoronaData.csv
CoronaData <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"))
CoronaData <- as_tibble(CoronaData)
View(CoronaData)
#----------------</read corona data>------------------------------------------------------------#



#----------------<read additional data>---------------------------------------------------------#
#read Health_expenditure_per_Capita
HealthExpenditure <- read.csv("https://raw.githubusercontent.com/LuGee94/TeamDB/master/DS1%20Project/Datenquellen/Gesundheitsausgaben%20pro%20Kopf/Health_expenditure_per_Capita.csv", check.names = FALSE)
HealthExpenditure <- as_tibble(HealthExpenditure)
#View(HealthExpenditure)

#read HappynessIndex Index
HappynessIndex <- read.csv("https://raw.githubusercontent.com/LuGee94/TeamDB/master/DS1%20Project/Datenquellen/Happyness_Index/happynessindex.csv", check.names = FALSE)
HappynessIndex <- as_tibble(HappynessIndex)
#View(HappynessIndex)

#read life expectancy - API_SP.DYN.LE00.IN_DS2_en_excel_v2_1120941
LifeExpectancy <- read.csv("https://raw.githubusercontent.com/LuGee94/TeamDB/master/DS1%20Project/Datenquellen/Lebenserwartung%20pro%20Land/life_expectancy_2019.csv", check.names = FALSE)
LifeExpectancy <- as_tibble(LifeExpectancy)
#View(LifeExpectancy)

#read API_SH.MED.NUMW
Nurses_per_1000 <- read.csv("https://raw.githubusercontent.com/LuGee94/TeamDB/master/DS1%20Project/Datenquellen/Nurses%20per%201000/nurses_per_1000.csv", check.names = FALSE)
Nurses_per_1000 <- as_tibble(Nurses_per_1000)
#View(Nurses_per_1000)

#read physicians/1000 API_SH.MED.PHYS
Physicians_per_1000 <- read.csv("https://raw.githubusercontent.com/LuGee94/TeamDB/master/DS1%20Project/Datenquellen/Physicians%20per%201000/physicians_per_1000.csv", check.names = FALSE)
Physicians_per_1000 <- as_tibble(Physicians_per_1000)
#View(Physicians_per_1000)
#----------------</read additional data>--------------------------------------------------------#



#----------------<prep corona data>-------------------------------------------------------------#
#read csv-file corona time series  2020_05_29_CoronaData.csv

# Column World contains sumarized values -> rows
CoronaData <- filter(CoronaData, location != "World")

# Transform Date
CoronaData$date <- as.Date(CoronaData$date, format = "%Y-%m-%d")
#----------------</prep corona data>------------------------------------------------------------#



#----------------<prep additional data>---------------------------------------------------------#
#rename columns Health Expenditure to identify source of feature later
names(HealthExpenditure)[names(HealthExpenditure) == "2018"] <- "Health_Expenditure_2018"

#delete column "Country Name" because we dont need it
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

#delete column "Country Name" because we dont need it
LifeExpectancy$'Country Name' <- NULL

#rename columns Nurses_per_1000 to identify source of feature later
names(Nurses_per_1000)[names(Nurses_per_1000) == "2019"] <- "Nurses/1000 2019"

#delete column "Country Name" because we dont need it
Nurses_per_1000$'Country Name' <- NULL

#rename columns Physicians_per_1000 to identify source of feature later
names(Physicians_per_1000)[names(Physicians_per_1000) == "2019"] <- "Physicians/1000 2019"

#delete column "Country Name" because we dont need it
Physicians_per_1000$'Country Name' <- NULL

#obsolete column
CoronaData$'test_units' <- NULL
CoronaData$continent <- NULL
#----------------</prep additional data>--------------------------------------------------------#



#----------------<Merge additional Data to corona data>---------------------------------------#
CoronaData_Joined <- as_tibble(CoronaData)
CoronaData_Joined <- CoronaData_Joined %>%
  left_join(HealthExpenditure, c("iso_code" = "Country Code"))  %>%
  left_join(HappynessIndex, c("location" = "Country")) %>%
  left_join(LifeExpectancy, c("iso_code" = "Country Code")) %>%
  left_join(Nurses_per_1000, c("iso_code" = "Country Code")) %>%
  left_join(Physicians_per_1000, c("iso_code" = "Country Code"))
#----------------</Merge Corona Data with additional Data>--------------------------------------#



#----------------<Transform and Calculate Corona Data>------------------------------------------#
#create columns to learn something about infection boundary -100 -1000 - 10000 -20000 -100000
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
  mutate( cases20000= case_when(total_cases < 20000 ~ "<20000",
                                total_cases >= 20000 ~ ">=20000")) %>%
  group_by(location, cases20000) %>%
  mutate( day_from_20000th_infection = case_when(cases20000 == "<20000" ~ as.integer(0),
                                                 cases20000 == ">=20000" ~ row_number())) %>%
  mutate( cases100000= case_when(total_cases < 100000 ~ "<100000",
                                 total_cases >= 100000 ~ ">=100000")) %>%
  group_by(location, cases100000) %>%
  mutate( day_from_100000th_infection = case_when(cases100000 == "<100000" ~ as.integer(0),
                                                  cases100000 == ">=100000" ~ row_number())) %>%
  ungroup()

view(Running_CoronaData)

#get max date corona dataset
max_data_corona_dataset <- filter(Running_CoronaData, Running_CoronaData$date =="2020-06-20")
#filter data to the most actual date
current_coronadata <- max_data_corona_dataset

#remove data <= 100 infections
current_coronadata <- filter(current_coronadata, day_from_100th_infection != 0)

#get the range in days between 100 and 1000 infections
CoronaData_1000 <- filter(Running_CoronaData, day_from_1000th_infection == 1) %>%
  ungroup() %>%
  select(location, day_from_100th_infection)
names(CoronaData_1000)[names(CoronaData_1000) == "day_from_100th_infection"] <- "days_100_to_1000_infections"

#get the range in days between 100 and 10000 / 1000 and 10000 infections
CoronaData_10000<- filter(Running_CoronaData, day_from_10000th_infection == 1) %>%
  ungroup() %>%
  select(location, day_from_100th_infection, day_from_1000th_infection)
names(CoronaData_10000)[names(CoronaData_10000) == "day_from_100th_infection"] <- "days_100_to_10000_infections"
names(CoronaData_10000)[names(CoronaData_10000) == "day_from_1000th_infection"] <- "days_1000_to_10000_infections"

#get the range in days between 100 and 20000 / 1000 and 20000 / 10000 and 20000 infections
CoronaData_20000<- filter(Running_CoronaData, day_from_20000th_infection == 1) %>%
  ungroup() %>%
  select(location, day_from_100th_infection, day_from_1000th_infection, day_from_10000th_infection)
names(CoronaData_20000)[names(CoronaData_20000) == "day_from_100th_infection"] <- "days_100_to_20000_infections"
names(CoronaData_20000)[names(CoronaData_20000) == "day_from_1000th_infection"] <- "days_1000_to_20000_infections"
names(CoronaData_20000)[names(CoronaData_20000) == "day_from_10000th_infection"] <- "days_10000_to_20000_infections"

#get the range in days between 100 and 100000 / 1000 and 100000 / 10000 and 100000 infections
CoronaData_100000<- filter(Running_CoronaData, day_from_100000th_infection == 1) %>%
  ungroup() %>%
  select(location, day_from_100th_infection, day_from_1000th_infection, day_from_10000th_infection, day_from_20000th_infection)
names(CoronaData_100000)[names(CoronaData_100000) == "day_from_100th_infection"] <- "days_100_to_100000_infections"
names(CoronaData_100000)[names(CoronaData_100000) == "day_from_1000th_infection"] <- "days_1000_to_100000_infections"
names(CoronaData_100000)[names(CoronaData_100000) == "day_from_10000th_infection"] <- "days_10000_to_100000_infections"
names(CoronaData_100000)[names(CoronaData_100000) == "day_from_20000th_infection"] <- "days_20000_to_100000_infections"

#join the ranges between the different boundaries to current_data
current_coronadata <- current_coronadata %>%
  left_join(CoronaData_1000, c("location" = "location"))  %>%
  left_join(CoronaData_10000, c("location" = "location"))  %>%
  left_join(CoronaData_20000, c("location" = "location")) %>%
  left_join(CoronaData_100000, c("location" = "location"))  

#view(current_coronadata)

#filter cases >= 20000 to select only significant countries with an impact on following regression
#most countries with cases <= 20000 are small countries where we have NA-Values
current_coronadata_filter_20000 <- filter(current_coronadata, total_cases >= 20000)

#filter irrelevant columns out
current_coronadata_filter_20000$'tests_units' <- NULL
current_coronadata_filter_20000$'total_tests' <- NULL
current_coronadata_filter_20000$'new_tests' <- NULL
current_coronadata_filter_20000$'total_tests_per_thousand' <- NULL
current_coronadata_filter_20000$'new_tests_per_thousand' <- NULL
current_coronadata_filter_20000$'new_tests_smoothed' <- NULL
current_coronadata_filter_20000$'new_tests_smoothed_per_thousand' <- NULL
current_coronadata_filter_20000$'stringency_index' <- NULL
current_coronadata_filter_20000$'aged_70_older' <- NULL
current_coronadata_filter_20000$'extreme_poverty' <- NULL
current_coronadata_filter_20000$'handwashing_facilities' <- NULL
current_coronadata_filter_20000$population <- NULL
current_coronadata_filter_20000$'cases100' <- NULL
current_coronadata_filter_20000$'cases1000' <- NULL
current_coronadata_filter_20000$'cases10000' <- NULL
current_coronadata_filter_20000$'cases100000' <- NULL
current_coronadata_filter_20000$cases20000 <- NULL
view(current_coronadata_filter_20000)
#----------------</Transform and Calculate Corona Data>-----------------------------------------#


#----------------<Regression analysis- number of deaths>----------------------------------------------#
Corona_day50 <- filter(Running_CoronaData, day_from_100th_infection == 50 & total_cases > 3000)
Corona_day75 <- filter(Running_CoronaData, day_from_100th_infection == 75 & total_cases > 5000)
# -> Probieren: Filter auf aktuellsten Tag Fälle > 20k
# Select variables for regression; Predictors prelimited due to exploratory analyisis and testing some regressions
Coronaa_day50_Reg_death <- select(Corona_day50, location, total_cases_per_million, total_cases, total_deaths, total_deaths_per_million, population_density, gdp_per_capita, life_expectancy, `Happiness score`, `Nurses/1000 2019`, `Physicians/1000 2019`, `HI-Dystopia (1.88) + residual`, `HI-Explained by: Freedom to make life choices`, `HI-Explained by: Generosity`, `HI-Explained by: Healthy life expectancy`, `HI-Explained by: Perceptions of corruption`, `HI-Explained by: Social support`)
lin_reg_cases_mil_death <- lm (total_deaths_per_million ~ population_density + gdp_per_capita + life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand + median_age, data = Corona_day50)
lin_reg_cases_total_death <- lm (total_deaths ~ population_density + gdp_per_capita + life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand + median_age, data = Corona_day50)
summary(lin_reg_cases_mil_death) #-> Negatice Influence of diabetes prevalence & hospital beds, positive of median age
summary(lin_reg_cases_total_death) #-> Strong positive inf. of nurses + Median Age, negative o. hospital beds

#Try some interaction variables:
lin_reg_cases_mil_int_death <- lm(total_deaths_per_million ~ gdp_per_capita*`Physicians/1000 2019` + population_density + hospital_beds_per_thousand + `Nurses/1000 2019`:hospital_beds_per_thousand, data= Corona_day50)
summary(lin_reg_cases_mil_int_death) #-> No clear findings from interactions, even after testing several variables

# Day 75
lin_reg_cases_mil_75_death <- lm (total_deaths_per_million ~ population_density + median_age + gdp_per_capita + life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019`, data = Corona_day75)
summary(lin_reg_cases_mil_75_death)
# -> Observing day 75 does not lead to additional insights

#Testing only happiness index factors:
lin_reg_cases_HI_only_death <- lm(total_deaths_per_million ~ `HI-Dystopia (1.88) + residual` + `HI-Explained by: Freedom to make life choices`+ `HI-Explained by: Generosity`+ `HI-Explained by: Healthy life expectancy` + `HI-Explained by: Perceptions of corruption` + `HI-Explained by: Social support` + `HI-Explained by: GDP per capita`, data = Corona_day50)
summary(lin_reg_cases_HI_only_death)

#Current Date instead of day 75
lin_reg_cases_mil_today_death <- lm (total_deaths_per_million ~ population_density + gdp_per_capita + life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand, data = current_coronadata_filter_5000)
summary(lin_reg_cases_mil_today_death) #-> Again strong negative influence of diabetes prevalence + hospital beds

plot(Corona_day50$hospital_beds_per_thousand, Corona_day50$total_deaths_per_million)
plot(Corona_day50$diabetes_prevalence, Corona_day50$total_deaths_per_million)
#----------------</Regression analysis - death rate>---------------------------------------------#




#----------------<Regression analysis- infection rate>-------------------------------------------#
Corona_day50 <- filter(Running_CoronaData, day_from_100th_infection == 50 & total_cases > 3000)
Corona_day75 <- filter(Running_CoronaData, day_from_100th_infection == 75 & total_cases > 5000)
# -> Probieren: Filter auf aktuellsten Tag Fälle > 20k
# Select variables for regression
Coronaa_day50_Reg <- select(Corona_day50, location, total_cases_per_million, total_cases, total_deaths, total_deaths_per_million, population_density, gdp_per_capita, life_expectancy, `Happiness score`, `Nurses/1000 2019`, `Physicians/1000 2019`, `HI-Dystopia (1.88) + residual`, `HI-Explained by: Freedom to make life choices`, `HI-Explained by: Generosity`, `HI-Explained by: Healthy life expectancy`, `HI-Explained by: Perceptions of corruption`, `HI-Explained by: Social support`, median_age)
lin_reg_cases_mil <- lm (total_cases_per_million ~ population_density + median_age + gdp_per_capita + life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand, data = Corona_day50)
lin_reg_cases_tot <- lm (total_cases ~ population_density + gdp_per_capita + median_age +life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand, data = Corona_day50)
summary(lin_reg_cases_mil)
#-> Significant influence of GDP; Happiness Score, Physicians, diabetes, hospital beds, pop_density
summary(lin_reg_cases_tot)  #Only nurses; probably observing total cases absolutely is not wothwhile
#Try some interaction variables:
lin_reg_cases_mil_int <- lm(total_cases_per_million ~ gdp_per_capita*`Physicians/1000 2019` + population_density + hospital_beds_per_thousand, data= Corona_day50)
summary(lin_reg_cases_mil_int)

# Day 75
lin_reg_cases_mil_75 <- lm (total_cases_per_million ~ population_density + gdp_per_capita + median_age +life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019`, data = Corona_day75)
summary(lin_reg_cases_mil_75)
# -> Happiness Index significant
#Testing only happiness index factors:
lin_reg_cases_HI_only <- lm(total_cases_per_million ~ `HI-Dystopia (1.88) + residual` + `HI-Explained by: Freedom to make life choices`+ `HI-Explained by: Generosity`+ `HI-Explained by: Healthy life expectancy` + `HI-Explained by: Perceptions of corruption` + `HI-Explained by: Social support` + `HI-Explained by: GDP per capita`, data = Corona_day50)
summary(lin_reg_cases_HI_only)

#Current Date
lin_reg_cases_mil_today <- lm (total_cases_per_million ~ population_density + gdp_per_capita + median_age +life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand, data = current_coronadata_filter_5000)
summary(lin_reg_cases_mil_today)
#----------------</Regression analysis - infection rate>-----------------------------------------#


#----------------<Clustering kmeans - cases and deaths per million>------------------------------#
#cluster based on total cases per million and total deaths per million
cluster_km_coronadata <- select(current_coronadata_filter_20000,'location','total_cases_per_million','total_deaths_per_million')
cluster_km_coronadata <- as.data.frame(cluster_km_coronadata)

#define the row name as location and delete column location
row.names(cluster_km_coronadata) <- cluster_km_coronadata$location
cluster_km_coronadata$location <- NULL

#view(cluster_km_coronadata)

#set seed to make analysis reproducible
set.seed(123)

# define different k sizes (2-9) for the k mean algorithm
km2 <- kmeans(cluster_km_coronadata, 2, iter.max = 25, nstart = 25)
km3 <- kmeans(cluster_km_coronadata, 3, iter.max = 25, nstart = 25)
km4 <- kmeans(cluster_km_coronadata, 4, iter.max = 25, nstart = 25)
km5 <- kmeans(cluster_km_coronadata, 5, iter.max = 25, nstart = 25)
km6 <- kmeans(cluster_km_coronadata, 6, iter.max = 25, nstart = 25)
km7 <- kmeans(cluster_km_coronadata, 7, iter.max = 25, nstart = 25)
km8 <- kmeans(cluster_km_coronadata, 8, iter.max = 25, nstart = 25)
km9 <- kmeans(cluster_km_coronadata, 9, iter.max = 25, nstart = 25)

View(cluster_km_coronadata)

# get withinss to barplot variance
wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss),
         + sum(km5$withinss), sum(km6$withinss), sum(km7$withinss), sum(km8$withinss), sum(km9$withinss))
names(wss) <- 2:9
barplot(wss)

#Barplot shows the variance within a cluster
#A general rule -> search for an "elbow" an thats the number of clusters you should use
#Therefore we considered 3 or 4 clusters

#Visualization of clusters 
fviz_cluster(km3, data = cluster_km_coronadata)
fviz_cluster(km4, data = cluster_km_coronadata)

#outlier by qatar, filter that out
cluster_km_coronadata_filtered <- select(current_coronadata_filter_20000,'location','total_cases_per_million','total_deaths_per_million')
cluster_km_coronadata_filtered <- filter(cluster_km_coronadata_filtered, location != "Qatar")
cluster_km_coronadata_filtered <- as.data.frame(cluster_km_coronadata_filtered)

#define the row name as location and delete column location
row.names(cluster_km_coronadata_filtered) <- cluster_km_coronadata_filtered$location
cluster_km_coronadata_filtered$location <- NULL

# define different k sizes (2-9) for the k mean algorithm 
km2_filtered <- kmeans(cluster_km_coronadata_filtered, 2, iter.max = 25, nstart = 25)
km3_filtered <- kmeans(cluster_km_coronadata_filtered, 3, iter.max = 25, nstart = 25)
km4_filtered <- kmeans(cluster_km_coronadata_filtered, 4, iter.max = 25, nstart = 25)
km5_filtered <- kmeans(cluster_km_coronadata_filtered, 5, iter.max = 25, nstart = 25)
km6_filtered <- kmeans(cluster_km_coronadata_filtered, 6, iter.max = 25, nstart = 25)
km7_filtered <- kmeans(cluster_km_coronadata_filtered, 7, iter.max = 25, nstart = 25)
km8_filtered <- kmeans(cluster_km_coronadata_filtered, 8, iter.max = 25, nstart = 25)
km9_filtered <- kmeans(cluster_km_coronadata_filtered, 9, iter.max = 25, nstart = 25)

#
wss_filtered <- c(sum(km2_filtered$withinss), sum(km3_filtered$withinss), sum(km4_filtered$withinss),
         + sum(km5_filtered$withinss), sum(km6_filtered$withinss), sum(km7_filtered$withinss), sum(km8_filtered$withinss), sum(km9_filtered$withinss))
names(wss_filtered) <- 2:9
barplot(wss_filtered)

#Barplot shows an elbow for 3 Clusters - 
#For further analysis we still consider 4 clusters 

#Visualization of clusters 
fviz_cluster(km3_filtered, data = cluster_km_coronadata_filtered)
fviz_cluster(km4_filtered, data = cluster_km_coronadata_filtered)

#----------------</Clustering kmeans - cases and deaths per million>-----------------------------#



#----------------<Clustering kmeans - infection spreading>-----------------------------#

#Data set including the development of infections
cluster_corona_development <- select(current_coronadata_filter_20000,'location','days_100_to_1000_infections','days_100_to_10000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections')
cluster_corona_development  <- as.data.frame(cluster_corona_development)


row.names(cluster_corona_development) <- cluster_corona_development$location
cluster_corona_development$location <- NULL
cluster_corona_development$total_cases <- NULL

#examine the correlation between the time developments
cor(cluster_corona_development)

#because theres correlation between column 2 and 3, we remove column 2, because the correlation between column1&column3 < column1&column2
cluster_corona_development$days_100_to_10000_infections <- NULL

# define different k sizes (2-9) for the k mean algorithm 
km2_dev <- kmeans(cluster_corona_development, 2, iter.max = 25, nstart = 25)
km3_dev <- kmeans(cluster_corona_development, 3, iter.max = 25, nstart = 25)
km4_dev <- kmeans(cluster_corona_development, 4, iter.max = 25, nstart = 25)
km5_dev <- kmeans(cluster_corona_development, 5, iter.max = 25, nstart = 25)
km6_dev <- kmeans(cluster_corona_development, 6, iter.max = 25, nstart = 25)
km7_dev <- kmeans(cluster_corona_development, 7, iter.max = 25, nstart = 25)
km8_dev <- kmeans(cluster_corona_development, 8, iter.max = 25, nstart = 25)
km9_dev <- kmeans(cluster_corona_development, 9, iter.max = 25, nstart = 25)

wss_dev <- c(sum(km2_dev$withinss), sum(km3_dev$withinss), sum(km4_dev$withinss),
                   + sum(km5_dev$withinss), sum(km6_dev$withinss), sum(km7_dev$withinss), sum(km8_dev$withinss), sum(km9_dev$withinss))
names(wss_dev) <- 2:9
barplot(wss_dev)

#There is no unambiguous amount of clusters - the most obvious is 3 clusters
# 4 clusters is still in consideration

#Visualization of clusters 
fviz_cluster(km3_dev, data = cluster_corona_development)
fviz_cluster(km4_dev, data = cluster_corona_development)
#----------------</Clustering kmeans - infection spreading>-----------------------------#


#----------------<Clustering kmeans - Clustervariables 1 & 2 together>------------------#
#lets try all of these variables
cluster_20000 <- select(current_coronadata_filter_20000, 'location', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections' )
cluster_20000 <- as.data.frame(cluster_20000)
row.names(cluster_20000) <- cluster_20000$location
cluster_20000$location <- NULL


#Is there a correlation between variables? 
cor(cluster_20000)


# define different k sizes (2-9) for the k mean algorithm 
km2 <- kmeans(cluster_20000, 2, iter.max = 25, nstart = 25)
km3 <- kmeans(cluster_20000, 3, iter.max = 25, nstart = 25)
km4 <- kmeans(cluster_20000, 4, iter.max = 25, nstart = 25)
km5 <- kmeans(cluster_20000, 5, iter.max = 25, nstart = 25)
km6 <- kmeans(cluster_20000, 6, iter.max = 25, nstart = 25)
km7 <- kmeans(cluster_20000, 7, iter.max = 25, nstart = 25)
km8 <- kmeans(cluster_20000, 8, iter.max = 25, nstart = 25)
km9 <- kmeans(cluster_20000, 9, iter.max = 25, nstart = 25)

wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss),
         + sum(km5$withinss), sum(km6$withinss), sum(km7$withinss), sum(km8$withinss), sum(km9$withinss))
names(wss) <- 2:9
barplot(wss)


#Visualization of clusters 
fviz_cluster(km3, data = cluster_20000)
fviz_cluster(km4, data = cluster_20000)

#Qatar is a statistical outlier, for better clustering results we filter that out
cluster_20000 <- select(current_coronadata_filter_20000, 'location', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections' )
cluster_20000 <- as.data.frame(cluster_20000)
cluster_20000 <- filter(cluster_20000, location != "Qatar")
row.names(cluster_20000) <- cluster_20000$location
cluster_20000$location <- NULL


km2 <- kmeans(cluster_20000, 2, iter.max = 25, nstart = 25)
km3 <- kmeans(cluster_20000, 3, iter.max = 25, nstart = 25)
km4 <- kmeans(cluster_20000, 4, iter.max = 25, nstart = 25)
km5 <- kmeans(cluster_20000, 5, iter.max = 25, nstart = 25)
km6 <- kmeans(cluster_20000, 6, iter.max = 25, nstart = 25)
km7 <- kmeans(cluster_20000, 7, iter.max = 25, nstart = 25)
km8 <- kmeans(cluster_20000, 8, iter.max = 25, nstart = 25)
km9 <- kmeans(cluster_20000, 9, iter.max = 25, nstart = 25)

wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss),
         + sum(km5$withinss), sum(km6$withinss), sum(km7$withinss), sum(km8$withinss), sum(km9$withinss))
names(wss) <- 2:9
barplot(wss)


#Visualization of clusters 
fviz_cluster(km3, data = cluster_20000)
fviz_cluster(km4, data = cluster_20000)

#result looks aren´t satisfying
#The Values arent standardized - Fix it


cluster_20000 <- select(current_coronadata_filter_20000, 'location', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections' )
cluster_20000 <- as.data.frame(cluster_20000)
cluster_20000 <- filter(cluster_20000, location != "Qatar" )
row.names(cluster_20000) <- cluster_20000$location
cluster_20000$location <- NULL
cluster_20000 <- scale(cluster_20000)

cor(cluster_20000)

km2 <- kmeans(cluster_20000, 2, iter.max = 25, nstart = 25)
km3 <- kmeans(cluster_20000, 3, iter.max = 25, nstart = 25)
km4 <- kmeans(cluster_20000, 4, iter.max = 25, nstart = 25)
km5 <- kmeans(cluster_20000, 5, iter.max = 25, nstart = 25)
km6 <- kmeans(cluster_20000, 6, iter.max = 25, nstart = 25)
km7 <- kmeans(cluster_20000, 7, iter.max = 25, nstart = 25)
km8 <- kmeans(cluster_20000, 8, iter.max = 25, nstart = 25)
km9 <- kmeans(cluster_20000, 9, iter.max = 25, nstart = 25)

wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss),
         + sum(km5$withinss), sum(km6$withinss), sum(km7$withinss), sum(km8$withinss), sum(km9$withinss))
names(wss) <- 2:9
barplot(wss)


#Visualization of clusters 
fviz_cluster(km3, data = cluster_20000)
fviz_cluster(km4, data = cluster_20000)


#statistical outliers  singapore, kuwait and Bahrain
#check the result without them

cluster_20000 <- select(current_coronadata_filter_20000, 'location', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections' )
cluster_20000 <- as.data.frame(cluster_20000)
cluster_20000 <- filter(cluster_20000, location != "Qatar" & location != "Kuwait" & location != "Singapore" & location != "Bahrain")


row.names(cluster_20000) <- cluster_20000$location
cluster_20000$location <- NULL

cluster_20000 <- scale(cluster_20000)

cor(cluster_20000)

km2 <- kmeans(cluster_20000, 2, iter.max = 25, nstart = 25)
km3 <- kmeans(cluster_20000, 3, iter.max = 25, nstart = 25)
km4 <- kmeans(cluster_20000, 4, iter.max = 25, nstart = 25)
km5 <- kmeans(cluster_20000, 5, iter.max = 25, nstart = 25)
km6 <- kmeans(cluster_20000, 6, iter.max = 25, nstart = 25)
km7 <- kmeans(cluster_20000, 7, iter.max = 25, nstart = 25)
km8 <- kmeans(cluster_20000, 8, iter.max = 25, nstart = 25)
km9 <- kmeans(cluster_20000, 9, iter.max = 25, nstart = 25)

wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss),
         + sum(km5$withinss), sum(km6$withinss), sum(km7$withinss), sum(km8$withinss), sum(km9$withinss))
names(wss) <- 2:9
barplot(wss)


#Visualization of clusters 
fviz_cluster(km3, data = cluster_20000)
fviz_cluster(km4, data = cluster_20000)
#----------------</Clustering kmeans - Clustervariables 1 & 2 together>-----------------#


#---------<Clustering kmeans - Clustervariables 1 & 2 together + GDP per Capita>--------#
cluster_20000_bip <- select(current_coronadata_filter_20000, 'location', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections', 'gdp_per_capita' )
cluster_20000_bip <- as.data.frame(cluster_20000_bip)
#cluster_20000_bip <- filter(cluster_20000_bip, location != "Qatar" & location != "Kuwait" & location != "Singapore" & location != "Bahrain")
cluster_20000_bip <- filter(cluster_20000_bip)

row.names(cluster_20000_bip) <- cluster_20000_bip$location
cluster_20000_bip$location <- NULL
cluster_20000_bip <- scale(cluster_20000_bip)

cor(cluster_20000_bip)

km2 <- kmeans(cluster_20000_bip, 2, iter.max = 25, nstart = 25)
km3 <- kmeans(cluster_20000_bip, 3, iter.max = 25, nstart = 25)
km4 <- kmeans(cluster_20000_bip, 4, iter.max = 25, nstart = 25)
km5 <- kmeans(cluster_20000_bip, 5, iter.max = 25, nstart = 25)
km6 <- kmeans(cluster_20000_bip, 6, iter.max = 25, nstart = 25)
km7 <- kmeans(cluster_20000_bip, 7, iter.max = 25, nstart = 25)
km8 <- kmeans(cluster_20000_bip, 8, iter.max = 25, nstart = 25)
km9 <- kmeans(cluster_20000_bip, 9, iter.max = 25, nstart = 25)

wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss),
         + sum(km5$withinss), sum(km6$withinss), sum(km7$withinss), sum(km8$withinss), sum(km9$withinss))
names(wss) <- 2:9
barplot(wss)

#Visualization of clusters 
fviz_cluster(km3, data = cluster_20000_bip)
fviz_cluster(km4, data = cluster_20000_bip)


#Bahrain, Kuwait, Singapore and Qatar are outliers
#Try without them 
cluster_20000_bip <- select(current_coronadata_filter_20000, 'location', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections', 'gdp_per_capita' )
cluster_20000_bip <- as.data.frame(cluster_20000_bip)
cluster_20000_bip <- filter(cluster_20000_bip, location != "Qatar" & location != "Kuwait" & location != "Singapore" & location != "Bahrain")

row.names(cluster_20000_bip) <- cluster_20000_bip$location
cluster_20000_bip$location <- NULL
cluster_20000_bip <- scale(cluster_20000_bip)

cor(cluster_20000_bip)

km2 <- kmeans(cluster_20000_bip, 2, iter.max = 25, nstart = 25)
km3 <- kmeans(cluster_20000_bip, 3, iter.max = 25, nstart = 25)
km4 <- kmeans(cluster_20000_bip, 4, iter.max = 25, nstart = 25)
km5 <- kmeans(cluster_20000_bip, 5, iter.max = 25, nstart = 25)
km6 <- kmeans(cluster_20000_bip, 6, iter.max = 25, nstart = 25)
km7 <- kmeans(cluster_20000_bip, 7, iter.max = 25, nstart = 25)
km8 <- kmeans(cluster_20000_bip, 8, iter.max = 25, nstart = 25)
km9 <- kmeans(cluster_20000_bip, 9, iter.max = 25, nstart = 25)

wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss),
         + sum(km5$withinss), sum(km6$withinss), sum(km7$withinss), sum(km8$withinss), sum(km9$withinss))
names(wss) <- 2:9
barplot(wss)

#Visualization of clusters 
fviz_cluster(km3, data = cluster_20000_bip)
fviz_cluster(km4, data = cluster_20000_bip)

#no relevant error reduce with 4 clusters, 3 clusters are the optimal number of k (in our opinion)
#regarding to elbow-knick of barplot
#Due to the hierarchical clustering results, we chose to select 4 clusters per analysis to be able to compare the results!


#Create Cluster data 
#get clusters from kmeans with 4 clusters
km3$cluster
cluster_assignment <- km4$cluster
cluster_assignment <- as.data.frame(cluster_assignment)

cluster_assignment <- rownames_to_column(cluster_assignment, var = "location")

#join current corona data with clusters to get seperate dataframes of each cluster
current_corona_with_clusters_km <- current_coronadata_filter_20000 %>%
    left_join(cluster_assignment, c("location" = "location"))


current_corona_with_clusters_km <- filter(current_corona_with_clusters_km, cluster_assignment != "NA")
view(current_corona_with_clusters_km)

#get dataframes of each km-cluster
corona_cluster_1_km <- current_corona_with_clusters_km
corona_cluster_1_km <- filter(current_corona_with_clusters_km, cluster_assignment == 1)
corona_cluster_2_km <- current_corona_with_clusters_km
corona_cluster_2_km <- filter(current_corona_with_clusters_km, cluster_assignment == 2)
corona_cluster_3_km <- current_corona_with_clusters_km
corona_cluster_3_km <- filter(current_corona_with_clusters_km, cluster_assignment == 3)
corona_cluster_4_km <- current_corona_with_clusters_km
corona_cluster_4_km <- filter(current_corona_with_clusters_km, cluster_assignment == 4 )

#--------</Clustering kmeans - Clustervariables 1 & 2 together + GDP per Capita>--------#




#----------------<Clustering dbscan>-------------------------------------------------------------#

#let's try a density based algorithm -> dbscan

#try 2 minpts
dbscan <- dbscan(cluster_20000_bip, eps = 1, minPts = 2, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.1, minPts = 2, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.2, minPts = 2, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.3, minPts = 2, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.4, minPts = 2, weights = NULL , borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

#try 3 minpts
dbscan <- dbscan(cluster_20000_bip, eps = 1, minPts = 3, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.1, minPts = 3, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.2, minPts = 3, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.3, minPts = 3, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

#try 4 minpts
dbscan <- dbscan(cluster_20000_bip, eps = 1, minPts = 4, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.1, minPts = 4, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.2, minPts = 4, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.3, minPts = 4, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)


#try 5minpts
dbscan <- dbscan(cluster_20000_bip, eps = 1.1, minPts = 5, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.2, minPts = 5, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.3, minPts = 5, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.4, minPts = 5, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)

dbscan <- dbscan(cluster_20000_bip, eps = 1.5, minPts = 5, weights = NULL, borderPoints = TRUE)
dbscan
fviz_cluster(dbscan, data = cluster_20000_bip)


#any combination of epsilon and minpoints does not show a good result for our corona dataset
#maybe its the wrong approach, because dbscan is good for filtering out noise
#and our clustering target is to get a cluster for every country, except the outliers like qatar etc
#thats why we don't continue working with the result of the dbscan clustering


#----------------</Clustering dbscan>------------------------------------------------------------#




#----------------<Hierarchical Clustering>-------------------------------------------------------------#

cluster_20000_h <- select(current_coronadata_filter_20000, 'location', 'gdp_per_capita', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections' )
cluster_20000_h <- as.data.frame(cluster_20000_h)

row.names(cluster_20000_h) <- cluster_20000_h$location
cluster_20000_h$location <- NULL


#Standardisieren

cluster_20000_h_scaled <- cbind(scale(cluster_20000_h[,1:6]))

#Distanzen bilden
dist_cluster_20000_h <- dist(cluster_20000_h_scaled[,1:6], method = "euclidean") # Alternative Methoden sind Maximum, Manhatten, canberra, pinary, minkowski

#Clustern
cluster_20000_hierarchical <- hclust(dist_cluster_20000_h, method = "ward.D2")

#Plotten
plot(cluster_20000_hierarchical, hang=-1, labels = cluster_20000_h$location, cex= 0.7)
rect.hclust(cluster_20000_hierarchical, k=4)
abline(h = 6.5, col = 'red')

suppressPackageStartupMessages(library(dendextend))
cluster_20000_hierarchical_dendro_obj <- as.dendrogram(cluster_20000_hierarchical)
cluster_20000_hierarchical_dendro <- color_branches(cluster_20000_hierarchical_dendro_obj, h = 5, k=4, border = 4)
plot(cluster_20000_hierarchical_dendro)



#Table with assignments to cluster 
cluster_20000_hierarchical_assignment <- cutree(cluster_20000_hierarchical, k = 4)
abline(col = 'red')


#View(cluster_20000_hierarchical_assignment)

#Qatar has an own Cluster -  Bahrain Kuweit and Singapore as well, therefore we remove them from dataset 

cluster_20000_h_filtered <- select(current_coronadata_filter_20000, 'location', 'gdp_per_capita', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections' )
cluster_20000_h_filtered <- as.data.frame(cluster_20000_h_filtered)
cluster_20000_h_filtered <- filter(cluster_20000_h_filtered, location != "Qatar" & location != "Kuwait" & location != "Singapore" & location != "Bahrain")

row.names(cluster_20000_h_filtered) <- cluster_20000_h_filtered$location
cluster_20000_h_filtered$location <- NULL

#Standardisieren
cluster_20000_h_filtered_scaled <- cbind(scale(cluster_20000_h_filtered[,1:6]))

#Distanzen bilden
dist_cluster_20000_filtered_h <- dist(cluster_20000_h_filtered_scaled[,1:6], method = "euclidean") # Alternative Methoden sind Maximum, Manhatten, canberra, pinary, minkowski

#Clustern
cluster_20000_hierarchical_filtered <- hclust(dist_cluster_20000_filtered_h, method = "ward.D2")


#Plotten
plot(cluster_20000_hierarchical_filtered, hang=-1, labels = cluster_20000_h_filtered$location, cex= 0.7)
rect.hclust(cluster_20000_hierarchical_filtered, k=4)
abline( col = 'red')

suppressPackageStartupMessages(library(dendextend))
cluster_20000_hierarchical_dendro_obj <- as.dendrogram(cluster_20000_hierarchical_filtered)
cluster_20000_hierarchical_dendro <- color_branches(cluster_20000_hierarchical_dendro_obj, h = 5, k=4, border = 4)
plot(cluster_20000_hierarchical_dendro)


#Table with assignments to cluster 
cluster_20000_hierarchical_assignment_filtered <- cutree(cluster_20000_hierarchical_filtered, k = 4)
abline(col = 'red')

View(cluster_20000_hierarchical_assignment_filtered)


#Write Cluster in DataSet
cluster_20000_hierarchical_assignment_filtered <- as.data.frame(cluster_20000_hierarchical_assignment_filtered)
View(cluster_20000_hierarchical_assignment_filtered)

cluster_20000_hierarchical_assignment_filtered_table <- rownames_to_column(cluster_20000_hierarchical_assignment_filtered, var = "location")
View(cluster_20000_hierarchical_assignment_filtered_table)

cluster_20000_h_joined <- current_coronadata_filter_20000 %>%
  left_join(cluster_20000_hierarchical_assignment_filtered_table, c("location" = "location"))

View(cluster_20000_h_joined)


#Create Table for each Cluster
corona_cluster_h1  <- cluster_20000_h_joined
corona_cluster_h1 <- filter(cluster_20000_h_joined, cluster_20000_hierarchical_assignment_filtered == 1)
corona_cluster_h2 <- cluster_20000_h_joined
corona_cluster_h2 <- filter(cluster_20000_h_joined, cluster_20000_hierarchical_assignment_filtered == 2)
corona_cluster_h3 <- cluster_20000_h_joined
corona_cluster_h3 <- filter(cluster_20000_h_joined, cluster_20000_hierarchical_assignment_filtered == 3)
corona_cluster_h4 <- cluster_20000_h_joined
corona_cluster_h4 <- filter(cluster_20000_h_joined, cluster_20000_hierarchical_assignment_filtered == 4)
corona_cluster_statistical_outliers <- filter(cluster_20000_h_joined, cluster_20000_h_joined$location == "Bahrain" |  cluster_20000_h_joined$location == "Kuwait" | cluster_20000_h_joined$location == "Singapore" | cluster_20000_h_joined$location =="Qatar")

  
View(corona_cluster_h1)
View(corona_cluster_h2)
View(corona_cluster_h3)
View(corona_cluster_h4)
View(corona_cluster_statistical_outliers)

#----------------</Hierarchical Clustering>-------------------------------------------------------------#





#----------------<analyze clustering hclust vs kmeans>-----------------------------------------#


cluster_comparison <- select(current_coronadata_filter_20000, 'location', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections', 'gdp_per_capita' )
cluster_comparison <- as.data.frame(cluster_comparison)
cluster_comparison<- filter(cluster_comparison, location != "Qatar" & location != "Kuwait" & location != "Singapore" & location != "Bahrain")


cluster_comparison <- cluster_comparison %>%
  left_join(cluster_assignment, c("location" = "location")) %>%
  left_join(cluster_20000_hierarchical_assignment_filtered_table, c("location" = "location"))



names(cluster_comparison)[names(cluster_comparison) == "cluster_assignment"] <- "cluster_assignment_kmeans"
names(cluster_comparison)[names(cluster_comparison) == "cluster_20000_hierarchical_assignment_filtered"] <- "cluster_assignment_hclust"

#analysis of clusters shows that the clusters of kmeans nearly equals the clusters of the hclust algorithm
#exceptions are germany, peru and panama
#adjust cluster assignment of hclust to simplify comparison

#cluster 2 of kmeans nearly equals cluster 1 of hclust
#cluster 4 of kmeans nearly equals cluster 2 of hclust
#cluster 3 of kmeans nearly equals cluster 4 of hclust
#cluster 1 of keamns nearly equals cluster 3 of hclust

cluster_comparison$cluster_assignment_hclust <- as.character(cluster_comparison$cluster_assignment_hclust)

cluster_comparison$cluster_assignment_hclust[cluster_comparison$cluster_assignment_hclust == "1"] <- "B"
cluster_comparison$cluster_assignment_hclust[cluster_comparison$cluster_assignment_hclust == "2"] <- "D"
cluster_comparison$cluster_assignment_hclust[cluster_comparison$cluster_assignment_hclust == "4"] <- "C"
cluster_comparison$cluster_assignment_hclust[cluster_comparison$cluster_assignment_hclust == "3"] <- "A"

cluster_comparison$cluster_assignment_hclust[cluster_comparison$cluster_assignment_hclust == "A"] <- "1"
cluster_comparison$cluster_assignment_hclust[cluster_comparison$cluster_assignment_hclust == "B"] <- "2"
cluster_comparison$cluster_assignment_hclust[cluster_comparison$cluster_assignment_hclust == "C"] <- "3"
cluster_comparison$cluster_assignment_hclust[cluster_comparison$cluster_assignment_hclust == "D"] <- "4"

view(cluster_comparison)

#export cluster_assignment to compare clusters in Tableau
write.csv(cluster_comparison, "/Users/niklaswagner/Documents/GitHub/TeamDB/DS1 Project/cluster_assignment.csv")



#result of comparison is to continue working with clusters of kmeans
#they show less variance in relation to the different characteristics


#lets continue working with kmeans clusters 1-4
#export to analyze clusters using tableau
summary(corona_cluster_1_km)
view(corona_cluster_1_km)
write.csv(corona_cluster_1_km, "/Users/niklaswagner/Documents/GitHub/TeamDB/DS1 Project/corona_cluster_1_km.csv")

summary(corona_cluster_2_km)
view(corona_cluster_2_km)
write.csv(corona_cluster_2_km, "/Users/niklaswagner/Documents/GitHub/TeamDB/DS1 Project/corona_cluster_2_km.csv")

summary(corona_cluster_3_km)
view(corona_cluster_3_km)
write.csv(corona_cluster_3_km, "/Users/niklaswagner/Documents/GitHub/TeamDB/DS1 Project/corona_cluster_3_km.csv")

summary(corona_cluster_4_km)
view(corona_cluster_4_km)
write.csv(corona_cluster_4_km, "/Users/niklaswagner/Documents/GitHub/TeamDB/DS1 Project/corona_cluster_4_km.csv")


#----------------</analyze clustering hclust vs kmeans>-----------------------------------------#




#----------------<regression analysis based on clusters>-----------------------------------------#

str(current_corona_with_clusters)

lin_reg_cluster_1_cases <- lm (total_cases_per_million ~ population_density + gdp_per_capita + life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand + median_age, data = corona_cluster_1_km)
#> Regression not possible because N > p

# Cluster 1 only 6 countries and can't be properly analyzed with a regression, we'll keep the regression to clusters 2-4
lin_reg_cluster_2_cases <- lm (total_cases_per_million ~ population_density + gdp_per_capita + life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand + median_age, data = corona_cluster_2_km)
lin_reg_cluster_2_deaths <- lm (total_deaths_per_million ~ population_density + gdp_per_capita + life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand + median_age, data = corona_cluster_2_km)
summary(lin_reg_cluster_2_deaths) #-> Nothing significant
summary(lin_reg_cluster_2_cases) #-> Strong negative influence of median_age + gdp; positive influence of nurses and life_expectancy

lin_reg_cluster_3_cases <- lm (total_cases_per_million ~ population_density + gdp_per_capita + life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand + median_age, data = corona_cluster_3_km)
lin_reg_cluster_3_deaths <- lm (total_deaths_per_million ~ population_density + gdp_per_capita + life_expectancy + `Happiness score` + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand + median_age, data = corona_cluster_3_km)
summary(lin_reg_cluster_3_deaths) #-> Positive influence of life expectancy, negative of diabetes and hospital_beds
summary(lin_reg_cluster_3_cases) #-> No clear influences

lin_reg_cluster_4_cases <- lm (total_cases_per_million ~ population_density + gdp_per_capita + life_expectancy +  `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand + median_age, data = corona_cluster_4_km)
lin_reg_cluster_4_deaths <- lm (total_deaths_per_million ~ population_density + gdp_per_capita + life_expectancy + `Nurses/1000 2019` + `Physicians/1000 2019` + diabetes_prevalence + hospital_beds_per_thousand + median_age, data = corona_cluster_4_km)
summary(lin_reg_cluster_4_deaths) #-> Positive influence of life expectancy, negative of diabetes and hospital_beds
summary(lin_reg_cluster_4_cases) #-> Not highly significant, but strong negative influence of median_age and Physicians

#----------------</regression analysis based on clusters>----------------------------------------#


