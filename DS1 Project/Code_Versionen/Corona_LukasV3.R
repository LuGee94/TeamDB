#----------------<prepare>----------------------------------------------------------------------#
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("readxl")
#install.packages("psych")
#install.packages("factoextra")
#install.packages("dbscan")


library(tidyverse) # -> standard library for Data Science in R: Plots, data wrangling and modeling
library(lubridate) # -> For working with dates and time
library(readxl) # -> reading xlsx files
library(psych) # -> cluster analyse
library(factoextra) # -> clustering algorithms & visualization
library(dbscan)
rm(list=ls())

options(scipen = 5)

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

#----------------</Merge Corona Data with additional Data>--------------------------------------#



#----------------<Transform and Calculate Corona Data>------------------------------------------#

#obsolete Feature
CoronaData$'test_units' <- NULL

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

#join the ranges between the different boundaries to out current_date_data
current_coronadata <- current_coronadata %>%
  left_join(CoronaData_1000, c("location" = "location"))  %>%
  left_join(CoronaData_10000, c("location" = "location"))  %>%
  left_join(CoronaData_20000, c("location" = "location")) %>%
  left_join(CoronaData_100000, c("location" = "location"))  


view(current_coronadata)

#filter cases >= 10000 to select only significant countries with an impact on following regression
#most countries with cases <= 10000 are small countries where we have NA-Values
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
current_coronadata_filter_20000$'cases100' <- NULL
current_coronadata_filter_20000$'cases1000' <- NULL
current_coronadata_filter_20000$'cases10000' <- NULL
current_coronadata_filter_20000$'cases100000' <- NULL
current_coronadata_filter_20000$cases20000 <- NULL


current_coronadata_filter_20000$population <- NULL

view(current_coronadata_filter_20000)

#----------------</Transform and Calculate Corona Data>-----------------------------------------#



#----------------<Regression analysis- death rate>----------------------------------------------#

corona_data_death_explanation <- current_coronadata_filter_10000

corona_data_death_explanation <- filter(corona_data_death_explanation, location != "Ukraine")

lin_reg_cov_deaths <- lm( cvd_death_rate ~ population_density + median_age + aged_65_older + diabetes_prevalence + hospital_beds_per_100k + Health_Expenditure_2018 + `Happiness score` + `Life-Expectancy 2019` + `Nurses/1000 2019` + `Physicians/1000 2019` , data = corona_data_death_explanation)
summary(lin_reg_cov_deaths)

cor(corona_data_death_explanation$median_age, corona_data_death_explanation$`Life-Expectancy 2019`)
cor(corona_data_death_explanation$median_age, corona_data_death_explanation$aged_65_older)
cor(corona_data_death_explanation$`Nurses/1000 2019`, corona_data_death_explanation$`Physicians/1000 2019`)
cor(corona_data_death_explanation$Health_Expenditure_2018, corona_data_death_explanation$gdp_per_capita)

lin_reg_cov_deaths_pm <- lm( cvd_death_rate ~ diabetes_prevalence + hospital_beds_per_100k + `Life-Expectancy 2019` + `Nurses/1000 2019` + `Physicians/1000 2019` , data = corona_data_death_explanation)
summary(lin_reg_cov_deaths_pm)

plot(`Life-Expectancy 2019`, cvd_death_rate)

plot(Health_Expenditure_2018, cvd_death_rate)

lin_reg_deaths_he <- lm ( cvd_death_rate ~ poly(Health_Expenditure_2018,4, raw = T) + poly(`Life-Expectancy 2019`,4, raw = T), data = corona_data_death_explanation)
summary(lin_reg_deaths_he)


#----------------</Regression analysis - death rate>---------------------------------------------#




#----------------<Regression analysis- infection rate>-------------------------------------------#




#----------------</Regression analysis - infection rate>-----------------------------------------#



#----------------<Clustering kmeans>-------------------------------------------------------------#
#ungroup(current_coronadata)

#cluster based on total cases per million and total deaths per million
cluster_coronadata <- select(current_coronadata_filter_20000,'location','total_cases_per_million','total_deaths_per_million')
cluster_coronadata <- as.data.frame(cluster_coronadata)

row.names(cluster_coronadata) <- cluster_coronadata$location
cluster_coronadata$location <- NULL

view(cluster_coronadata)

set.seed(123)

km2 <- kmeans(cluster_coronadata, 2, iter.max = 25, nstart = 25)
km3 <- kmeans(cluster_coronadata, 3, iter.max = 25, nstart = 25)
km4 <- kmeans(cluster_coronadata, 4, iter.max = 25, nstart = 25)
km5 <- kmeans(cluster_coronadata, 5, iter.max = 25, nstart = 25)
km6 <- kmeans(cluster_coronadata, 6, iter.max = 25, nstart = 25)
km7 <- kmeans(cluster_coronadata, 7, iter.max = 25, nstart = 25)
km8 <- kmeans(cluster_coronadata, 8, iter.max = 25, nstart = 25)
km9 <- kmeans(cluster_coronadata, 9, iter.max = 25, nstart = 25)

wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss),
         + sum(km5$withinss), sum(km6$withinss), sum(km7$withinss), sum(km8$withinss), sum(km9$withinss))
names(wss) <- 2:9
barplot(wss)

fviz_cluster(km2, data = cluster_coronadata)
fviz_cluster(km3, data = cluster_coronadata)

#outlier by qatar, filter that out

cluster_coronadata_filtered <- select(current_coronadata_filter_20000,'location','total_cases_per_million','total_deaths_per_million')
cluster_coronadata_filtered <- filter(cluster_coronadata_filtered, location != "Qatar")
cluster_coronadata_filtered <- as.data.frame(cluster_coronadata_filtered)


row.names(cluster_coronadata_filtered) <- cluster_coronadata_filtered$location
cluster_coronadata_filtered$location <- NULL

km2_filtered <- kmeans(cluster_coronadata_filtered, 2, iter.max = 25, nstart = 25)
km3_filtered <- kmeans(cluster_coronadata_filtered, 3, iter.max = 25, nstart = 25)
km4_filtered <- kmeans(cluster_coronadata_filtered, 4, iter.max = 25, nstart = 25)
km5_filtered <- kmeans(cluster_coronadata_filtered, 5, iter.max = 25, nstart = 25)
km6_filtered <- kmeans(cluster_coronadata_filtered, 6, iter.max = 25, nstart = 25)
km7_filtered <- kmeans(cluster_coronadata_filtered, 7, iter.max = 25, nstart = 25)
km8_filtered <- kmeans(cluster_coronadata_filtered, 8, iter.max = 25, nstart = 25)
km9_filtered <- kmeans(cluster_coronadata_filtered, 9, iter.max = 25, nstart = 25)


wss_filtered <- c(sum(km2_filtered$withinss), sum(km3_filtered$withinss), sum(km4_filtered$withinss),
         + sum(km5_filtered$withinss), sum(km6_filtered$withinss), sum(km7_filtered$withinss), sum(km8_filtered$withinss), sum(km9_filtered$withinss))
names(wss_filtered) <- 2:9
barplot(wss_filtered)

fviz_cluster(km2_filtered, data = cluster_coronadata_filtered)
fviz_cluster(km3_filtered, data = cluster_coronadata_filtered)


#lets try the development of infections

cluster_corona_development <- select(current_coronadata_filter_20000,'location','days_100_to_1000_infections','days_100_to_10000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections')
cluster_corona_development  <- as.data.frame(cluster_corona_development)

row.names(cluster_corona_development) <- cluster_corona_development$location
cluster_corona_development$location <- NULL
cluster_corona_development$total_cases <- NULL


cor(cluster_corona_development)

#because theres correlation between column 2 and 3, we remove column 2, because the correlation between column1&column3 < column1&column2

cluster_corona_development$days_100_to_10000_infections <- NULL

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



fviz_cluster(km2_dev, data = cluster_corona_development)
fviz_cluster(km3_dev, data = cluster_corona_development)



#lets try all of these variables

cluster_20000 <- select(current_coronadata_filter_20000, 'location', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections' )
cluster_20000 <- as.data.frame(cluster_20000)
row.names(cluster_20000) <- cluster_20000$location
cluster_20000$location <- NULL

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



fviz_cluster(km2, data = cluster_20000)
fviz_cluster(km3, data = cluster_20000)

#outlier qatar, filter that out
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



fviz_cluster(km2, data = cluster_20000)
fviz_cluster(km3, data = cluster_20000)

#result looks weird, maybe because the variables aren't standardized, letz fix that


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



fviz_cluster(km2, data = cluster_20000)
fviz_cluster(km3, data = cluster_20000)


#result still looks weird, maybe because we have two outliers singapore and kuwait, let's check the result without them

cluster_20000 <- select(current_coronadata_filter_20000, 'location', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections' )
cluster_20000 <- as.data.frame(cluster_20000)
cluster_20000 <- filter(cluster_20000, location != "Qatar" & location != "Kuwait" & location != "Singapore")
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



fviz_cluster(km2, data = cluster_20000)
fviz_cluster(km3, data = cluster_20000)
fviz_cluster(km4, data = cluster_20000)

#no relevant error reduce with 4 clusters, 3 clusters are the optimal number of k (in our opinion)
#regarding to elbow-knick of dendogram/barplot



#----------------</Clustering kmeans>------------------------------------------------------------#




#----------------<Clustering dbscan>-------------------------------------------------------------#


dbscan <- dbscan(cluster_20000, eps = 1, minPts = 2, weights = NULL)
dbscan
fviz_cluster(dbscan, data = cluster_20000)

dbscan <- dbscan(cluster_20000, eps = 1.2, minPts = 3, weights = NULL)
dbscan
fviz_cluster(dbscan, data = cluster_20000)


dbscan <- dbscan(cluster_20000, eps = 1.3, minPts = 2, weights = NULL)
dbscan
fviz_cluster(dbscan, data = cluster_20000)


dbscan <- dbscan(cluster_20000, eps = 1.5, minPts = 5, weights = NULL)
dbscan
fviz_cluster(dbscan, data = cluster_20000)









cluster_20000_h <- select(current_coronadata_filter_20000, 'location', 'total_cases_per_million', 'total_deaths_per_million', 'days_100_to_1000_infections', 'days_1000_to_10000_infections', 'days_10000_to_20000_infections' )
cluster_20000_h <- as.data.frame(cluster_20000_h)

row.names(cluster_20000) <- cluster_20000$location
cluster_20000_h$location <- NULL


#Schritt 3: Distanzmatrix

dist_matrix <- dist(cluster_20000_h, method = "euclidean")
dist_matrix

#Schritt 4: Clusteranalyse

cluster <- hclust(dist_matrix, method = "single")
cluster

#Schritt 5: Zuordnungs?bersicht 

cluster$merge

#Schritt 6: Dendrogramm

plot(cluster, labels = cluster$labels)


plot(points, main = 'Hierarchical clustering', col = as.factor(cluster$cluster)) 


#Schritt 7: Elbow-Diagram

plot(41:1, cluster$height, xlab = "Number of Clusters", ylab = "Within SSE", type = "o")










plot(population_density~ total_deaths_per_million, data = current_coronadata)
with(current_coronadata, text(population_density ~total_deaths_per_million, labels= location, pos=4, cex= .4))

model1<- lm(total_deaths_per_million ~ population_density + `Nurses/1000 2019` + `Life-Expectancy 2019` + `Happiness score` + hospital_beds_per_100k + gdp_per_capita)
summary(model1)

confint(model1, conf.level = 0.95)

plot(model1)
