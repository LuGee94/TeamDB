#----------------<prepare>----------------------------------------------------------------------#
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("readxl")
#install.packages("psych")


library(tidyverse) # -> standard library for Data Science in R: Plots, data wrangling and modeling
library(lubridate) # -> For working with dates and time
library(readxl) # -> reading xlsx files
library(psych) # -> cluster analyse

rm(list=ls())

options(scipen = 5)

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

#----------------</Merge Corona Data with additional Data>--------------------------------------#



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
                                                  cases100000 == ">=100000" ~ row_number())) %>%
  ungroup()

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

#----------------</Transform and Calculate Corona Data>-----------------------------------------#



#----------------<Regression analysis- death rate>-----------------------------------------#

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


#----------------</Regression analysis - death rate>-----------------------------------------#




#----------------<Regression analysis- infection rate>-----------------------------------------#




#----------------</Regression analysis - infection rate>-----------------------------------------#



#----------------<Clustering>-----------------------------------------#
#ungroup(current_coronadata)

#cluster based on total cases per million and total deaths per million
cluster_coronadata <- select(current_coronadata_filter_10000,'location','total_cases_per_million','total_deaths_per_million')
cluster_coronadata <- as.data.frame(cluster_coronadata)

row.names(cluster_coronadata) <- cluster_coronadata$location
cluster_coronadata$location <- NULL

view(cluster_coronadata)

set.seed(1312)

km2 <- kmeans(cluster_coronadata, 2)
km3 <- kmeans(cluster_coronadata, 3)
km4 <- kmeans(cluster_coronadata, 4)
km5 <- kmeans(cluster_coronadata, 5)
km6 <- kmeans(cluster_coronadata, 6)
km7 <- kmeans(cluster_coronadata, 7)
km8 <- kmeans(cluster_coronadata, 8)
km9 <- kmeans(cluster_coronadata, 9)

wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss),
         + sum(km5$withinss), sum(km6$withinss), sum(km7$withinss), sum(km8$withinss), sum(km9$withinss))
names(wss) <- 2:9
barplot(wss)

cluster.pca2 <- prcomp(cluster_coronadata, scale = TRUE)
plot(predict(cluster.pca2)[, 1:2], type = "n")
text(predict(cluster.pca2)[, 1:2], rownames(cluster_coronadata), col = km2$cluster, pos=4, cex= .4)
points(predict(cluster.pca2, km2$centers)[, 1:2], col = 1:2, pch = 2, cex = 5)

cluster.pca3 <- prcomp(cluster_coronadata, scale = TRUE)
plot(predict(cluster.pca3)[, 1:2], type = "n")
text(predict(cluster.pca3)[, 1:2], rownames(cluster_coronadata), col = km3$cluster, pos=4, cex= .4)
points(predict(cluster.pca3, km3$centers)[, 1:2], col = 1:3, pch = 2,cex = 5)

cluster.pca4 <- prcomp(cluster_coronadata, scale = TRUE)
plot(predict(cluster.pca4)[, 1:2], type = "n")
text(predict(cluster.pca4)[, 1:2], rownames(cluster_coronadata), col = km4$cluster, pos=4, cex= .4)
points(predict(cluster.pca4, km4$centers)[, 1:2], col = 1:4, pch = 2, cex = 5)

cluster.pca5 <- prcomp(cluster_coronadata, scale = TRUE)
plot(predict(cluster.pca5)[, 1:2], type = "n")
text(predict(cluster.pca5)[, 1:2], rownames(cluster_coronadata), col = km5$cluster, pos=4, cex= .4)
points(predict(cluster.pca5, km5$centers)[, 1:2], col = 1:5, pch = 2, cex = 5)


#cluster.pca6 <- prcomp(cluster_coronadata, scale = TRUE)
#plot(predict(cluster.pca6)[, 1:2], type = "n")
#text(predict(cluster.pca6)[, 1:2], rownames(cluster_coronadata), col = km6$cluster)
#points(predict(cluster.pca6, km6$centers)[, 1:2], col = 1:6, pch = 6, cex = 6)

#cluster.pca7 <- prcomp(cluster_coronadata, scale = TRUE)
#plot(predict(cluster.pca7)[, 1:2], type = "n")
#text(predict(cluster.pca7)[, 1:2], rownames(cluster_coronadata), col = km7$cluster)
#points(predict(cluster.pca7, km7$centers)[, 1:2], col = 1:7, pch = 7, cex = 7)

#cluster.pca8 <- prcomp(cluster_coronadata, scale = TRUE)
#plot(predict(cluster.pca8)[, 1:2], type = "n")
#text(predict(cluster.pca8)[, 1:2], rownames(cluster_coronadata), col = km8$cluster)
#points(predict(cluster.pca8, km8$centers)[, 1:2], col = 1:8, pch = 8, cex = 8)

#cluster.pca9 <- prcomp(cluster_coronadata, scale = TRUE)
#plot(predict(cluster.pca9)[, 1:2], type = "n")
#text(predict(cluster.pca9)[, 1:2], rownames(cluster_coronadata), col = km9$cluster)
#points(predict(cluster.pca9, km9$centers)[, 1:2], col = 1:3, pch = 9, cex = 9)



#outlier by qatar, filter that out

cluster_coronadata_filtered <- select(current_coronadata_filter_10000,'location','total_cases_per_million','total_deaths_per_million')
cluster_coronadata_filtered <- filter(cluster_coronadata_filtered, location != "Qatar")
cluster_coronadata_filtered <- as.data.frame(cluster_coronadata_filtered)


row.names(cluster_coronadata_filtered) <- cluster_coronadata_filtered$location
cluster_coronadata_filtered$location <- NULL

km2_filtered <- kmeans(cluster_coronadata_filtered, 2)
km3_filtered <- kmeans(cluster_coronadata_filtered, 3)
km4_filtered <- kmeans(cluster_coronadata_filtered, 4)
km5_filtered <- kmeans(cluster_coronadata_filtered, 5)
km6_filtered <- kmeans(cluster_coronadata_filtered, 6)
km7_filtered <- kmeans(cluster_coronadata_filtered, 7)
km8_filtered <- kmeans(cluster_coronadata_filtered, 8)
km9_filtered <- kmeans(cluster_coronadata_filtered, 9)


wss_filtered <- c(sum(km2_filtered$withinss), sum(km3_filtered$withinss), sum(km4_filtered$withinss),
         + sum(km5_filtered$withinss), sum(km6_filtered$withinss), sum(km7_filtered$withinss), sum(km8_filtered$withinss), sum(km9_filtered$withinss))
names(wss_filtered) <- 2:9
barplot(wss_filtered)

cluster.pca2_filtered <- prcomp(cluster_coronadata_filtered, scale = TRUE)
plot(predict(cluster.pca2_filtered)[, 1:2], type = "n")
text(predict(cluster.pca2_filtered)[, 1:2], rownames(cluster_coronadata_filtered), col = km2_filtered$cluster,pos=4, cex= .4)
points(predict(cluster.pca2_filtered, km2_filtered$centers)[, 1:2], col = 1:2, pch = 2, cex = 5)

cluster.pca3_filtered <- prcomp(cluster_coronadata_filtered, scale = TRUE)
plot(predict(cluster.pca3_filtered)[, 1:2], type = "p")
text(predict(cluster.pca3_filtered)[, 1:2], rownames(cluster_coronadata_filtered), col = km3_filtered$cluster,pos=4, cex= .4)
points(predict(cluster.pca3_filtered, km3_filtered$centers)[, 1:2], col = 1:3, pch = 4, cex = 5)

#cluster 4 irrelevant because same withiness as clusters = 3

cluster.pca5_filtered <- prcomp(cluster_coronadata_filtered, scale = TRUE)
plot(predict(cluster.pca5_filtered)[, 1:2], type = "n")
text(predict(cluster.pca5_filtered)[, 1:2], rownames(cluster_coronadata_filtered), col = km5_filtered$cluster,pos=4, cex= .4)
points(predict(cluster.pca5_filtered, km5_filtered$centers)[, 1:2], col = 1:5, pch = 4, cex = 5)


#still too many countries...

cluster_coronadata_filtered2 <- select(current_coronadata_filter_10000,'location','total_cases_per_million','total_deaths_per_million', 'total_cases')
cluster_coronadata_filtered2 <- filter(cluster_coronadata_filtered2, location != "Belgium" & location != "Qatar")
cluster_coronadata_filtered2 <- as.data.frame(cluster_coronadata_filtered2)

row.names(cluster_coronadata_filtered2) <- cluster_coronadata_filtered2$location
cluster_coronadata_filtered2$location <- NULL
cluster_coronadata_filtered2$total_cases <- NULL



km2_filtered2 <- kmeans(cluster_coronadata_filtered2, 2)
km3_filtered2 <- kmeans(cluster_coronadata_filtered2, 3)
km4_filtered2 <- kmeans(cluster_coronadata_filtered2, 4)
km5_filtered2 <- kmeans(cluster_coronadata_filtered2, 5)
km6_filtered2 <- kmeans(cluster_coronadata_filtered2, 6)
km7_filtered2 <- kmeans(cluster_coronadata_filtered2, 7)
km8_filtered2 <- kmeans(cluster_coronadata_filtered2, 8)
km9_filtered2 <- kmeans(cluster_coronadata_filtered2, 9)

wss_filtered2 <- c(sum(km2_filtered2$withinss), sum(km3_filtered2$withinss), sum(km4_filtered2$withinss),
                  + sum(km5_filtered2$withinss), sum(km6_filtered2$withinss), sum(km7_filtered2$withinss), sum(km8_filtered2$withinss), sum(km9_filtered2$withinss))
names(wss_filtered2) <- 2:9
barplot(wss_filtered2)

cluster.pca2_filtered2 <- prcomp(cluster_coronadata_filtered2, scale = TRUE)
plot(predict(cluster.pca2_filtered2)[, 1:2], type = "n")
text(predict(cluster.pca2_filtered2)[, 1:2], rownames(cluster_coronadata_filtered2), col = km2_filtered2$cluster, pos=4, cex= .4)
points(predict(cluster.pca2_filtered2, km2_filtered2$centers)[, 1:2], col = 1:2, pch = 2, cex = 5)


cluster.pca3_filtered2 <- prcomp(cluster_coronadata_filtered2, scale = TRUE)
plot(predict(cluster.pca3_filtered2)[, 1:2], type = "n")
text(predict(cluster.pca3_filtered2)[, 1:2], rownames(cluster_coronadata_filtered2), col = km3_filtered2$cluster, pos=4, cex= .4)
points(predict(cluster.pca3_filtered2, km3_filtered2$centers)[, 1:2], col = 1:3, pch = 4, cex = 3)

cluster.pca4_filtered2 <- prcomp(cluster_coronadata_filtered2, scale = TRUE)
plot(predict(cluster.pca4_filtered2)[, 1:2], type = "n")
text(predict(cluster.pca4_filtered2)[, 1:2], rownames(cluster_coronadata_filtered2), col = km4_filtered2$cluster, pos=4, cex= .6)
points(predict(cluster.pca4_filtered2, km4_filtered2$centers)[, 1:2], col = 1:4, pch = 4, cex = 5)

cluster.pca5_filtered2 <- prcomp(cluster_coronadata_filtered2, scale = TRUE)
plot(predict(cluster.pca5_filtered2)[, 1:2], type = "n")
text(predict(cluster.pca5_filtered2)[, 1:2], rownames(cluster_coronadata_filtered2), col = km5_filtered2$cluster, pos=4, cex= .4)
points(predict(cluster.pca5_filtered2, km5_filtered2$centers)[, 1:2], col = 1:5, pch = 5, cex = 5)

#5 clusters -> error to high



#lets try the development of infections

view(current_coronadata_filter_10000)
cluster_corona_development <- select(current_coronadata_filter_10000,'location','days_100_to_1000_infections','days_100_to_10000_infections', 'days_1000_to_10000_infections', 'total_cases')
cluster_corona_development  <- as.data.frame(cluster_corona_development)

row.names(cluster_corona_development) <- cluster_corona_development$location
cluster_corona_development$location <- NULL
cluster_corona_development$total_cases <- NULL


cor(cluster_corona_development)

#because theres correlation between column 2 and 3, we remove column 2, because the correlation between column1&column3 < column1&column2

cluster_corona_development$days_100_to_10000_infections <- NULL

km2_dev <- kmeans(cluster_corona_development, 2)
km3_dev <- kmeans(cluster_corona_development, 3)
km4_dev <- kmeans(cluster_corona_development, 4)
km5_dev <- kmeans(cluster_corona_development, 5)
km6_dev <- kmeans(cluster_corona_development, 6)
km7_dev <- kmeans(cluster_corona_development, 7)
km8_dev <- kmeans(cluster_corona_development, 8)
km9_dev <- kmeans(cluster_corona_development, 9)

wss_dev <- c(sum(km2_dev$withinss), sum(km3_dev$withinss), sum(km4_dev$withinss),
                   + sum(km5_dev$withinss), sum(km6_dev$withinss), sum(km7_dev$withinss), sum(km8_dev$withinss), sum(km9_dev$withinss))
names(wss_dev) <- 2:9
barplot(wss_dev)

cluster.pca2_dev <- prcomp(cluster_corona_development, scale = TRUE)
plot(predict(cluster.pca2_dev)[, 1:2], type = "n")
text(predict(cluster.pca2_dev)[, 1:2], rownames(cluster_corona_development), col = km2_dev$cluster, pos=4, cex= .4)
points(predict(cluster.pca2_dev, km2_dev$centers)[, 1:2], col = 1:2, pch = 2, cex = 5)

cluster.pca3_dev <- prcomp(cluster_corona_development, scale = TRUE)
plot(predict(cluster.pca3_dev)[, 1:2], type = "n")
text(predict(cluster.pca3_dev)[, 1:2], rownames(cluster_corona_development), col = km3_dev$cluster, pos=4, cex= .4)
points(predict(cluster.pca3_dev, km3_dev$centers)[, 1:2], col = 1:3, pch = 4, cex = 5)

cluster.pca4_dev <- prcomp(cluster_corona_development, scale = TRUE)
plot(predict(cluster.pca4_dev)[, 1:2], type = "n")
text(predict(cluster.pca4_dev)[, 1:2], rownames(cluster_corona_development), col = km4_dev$cluster, pos=4, cex= .7)
points(predict(cluster.pca2_dev, km4_dev$centers)[, 1:2], col = 1:4, pch = 4, cex = 5)

cluster.pca5_dev <- prcomp(cluster_corona_development)
plot(predict(cluster.pca5_dev)[, 1:2], type = "p")
text(predict(cluster.pca5_dev)[, 1:2], rownames(cluster_corona_development), col = km5_dev$cluster, pos=4, cex= .6)
points(predict(cluster.pca5_dev, km5_dev$centers)[, 1:2], col = 1:5, pch = 3, cex =10)

view(cluster_corona_development)

#----------------</Clustering>-----------------------------------------#
plot(population_density~ total_deaths_per_million, data = current_coronadata)
with(current_coronadata, text(population_density ~total_deaths_per_million, labels= location, pos=4, cex= .4))

model1<- lm(total_deaths_per_million ~ population_density + `Nurses/1000 2019` + `Life-Expectancy 2019` + `Happiness score` + hospital_beds_per_100k + gdp_per_capita)
summary(model1)

confint(model1, conf.level = 0.95)

plot(model1)
