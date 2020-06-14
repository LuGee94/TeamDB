CoronaData <- read.csv("~/Desktop/CoronaData.csv")
library(tidyverse) # -> standard library for Data Science in R: Plots, data wrangling and modeling
library(lubridate) # -> For working with dates and time
CoronaData <- as_tibble(CoronaData)
view(CoronaData)
# World contains sum, filter that out
CoronaData <- filter(CoronaData, location != "World")
today <- filter(CoronaData, date=="2020-05-22")
#Check max Date for each country, average (daily) number of cases
test <- as.Date(CoronaData$date, format = "%m-%d-%Y")
CoronaData$date <- as.Date(CoronaData$date, format = "%Y-%m-%d")
bycounty <- group_by(CoronaData, location)
max_date <- summarise(bycounty, maxdate = max(date)) 

max_date <- CoronaData %>%
  group_by(location) %>%
  summarise(maxdate = max(date),
            mindate = min(date),
            datediff = max(date) - min(date) +1,
            avg_case = mean(new_cases),
            count = n()) %>% #-> Might be problematic if we have dates without data for some countries, so to get some idea, let's include n()
  arrange(desc(avg_case))
view(max_date)  # -> New data for all countries, except Hong Kong and International
                # -> Number of records per country vary; but we get a decent idea of the top infected countries
                # -> For some countries, count is smaller than datediff, so there are days after the first infections without data
                
#Plots
# Scatterplot: Total_cases vs Total deaths (today)
ggplot(data=today) + geom_point(mapping= aes(x=total_cases, y=total_deaths))
#Scatterplot: Number of cases as timeline; including all countries
ggplot(data=CoronaData) + geom_point(mapping= aes(x=date, y=new_cases))
#-> We can see the outbreak in China in February, the outliers starting end of march are all from the USA

#Same thing only when total cases > 1000; different syntax + transparency
CoronaData %>%
  filter(total_cases>1000) %>%
  ggplot(mapping = aes(x=date, y=new_cases)) +
  geom_point(alpha = 5/10)

#Running number for each country
Running <- CoronaData %>%
  select(location:new_deaths) %>%
  group_by(location) %>%
  mutate(rownumber = row_number())

#Running number starting with 100 infections
Running_100 <- CoronaData %>%
  #select(location:new_deaths) %>% # -> Less columns  for exploration
  group_by(location) %>%
  mutate( cases100= case_when(total_cases < 100 ~ "<100",
                             total_cases >= 100  ~ ">=100")) %>%
  group_by(location, cases100) %>%
  mutate( day_from_100th_infection= case_when(cases100 == "<100" ~ as.integer(0),
                            cases100 == ">=100"  ~ row_number()))

#Try to get a negative number for days before 100 infections
Running_100 <- Running_100 %>%
  arrange(location, desc(date)) %>%
  group_by(location, cases100) %>%
  mutate( day_from_100th_infection= case_when(cases100 == "<100" ~ -row_number(),
                                              cases100 == ">=100"  ~ day_from_100th_infection)) %>%
  arrange(location, date)

#Plotting our countries, we should now have all countries starting from the 100th infection (starting with day100infection = 1)
#Compare 4 countries
Running_100 %>%
  filter(location %in% c("Germany", "Italy", "China", "United States")) %>%
  filter(day_from_100th_infection > 0) %>%
  ggplot(mapping = aes(x=day_from_100th_infection, y=total_cases, color=location)) +
  geom_line()

# Example for variable we'll try to predict: Number of cases 20 days after 100th infection
day20 <- filter(Running_100, day_from_100th_infection == 20)
day30 <- filter(Running_100, day_from_100th_infection == 30)
#-> Ideas: Classification: Total cases > 1000? Total deaths >50?
summarise(day20, mean(total_cases>0))
day20 %>% 
  ungroup()%>% 
  summarise(cases_prop = mean(total_cases > 1000),
            death_prop = mean(total_deaths > 50))
#-> 43.4% have >1000 reported cases after 20 days; 26,7% have >50 deaths

#Correlation of GDP per capita vs cases after 20 days?
day20 %>%
  ggplot(mapping= aes(x=gdp_per_capita, y=total_cases)) + 
  geom_point() +
  geom_smooth()

# -> Possibly there is a nonlinear trend: low detection for poor countries; more detection for medium-rich; richest countries might have good prevention
# -> Maybe it's better to look for cases per million; so that countries with different populations are more comparable
day20 %>%
  ggplot(mapping= aes(x=gdp_per_capita, y=total_cases_per_million)) + 
  geom_point() +
  geom_smooth()
#-> Trend looks a lot more positive

#Linear regression: Gdp on total cases
lm.fit <- lm(total_cases ~ gdp_per_capita, data=day20)
summary(lm.fit) #-> Very low accuracy (R2: 3 %), but there is a statistically significant linear relationship

#Linear regression: Gdp on total cases per million
lm.fit2 <- lm(total_cases_per_million ~ gdp_per_capita, data=day20)
summary(lm.fit2) #-> Highly significant positive influence, still moderate R2 (15%)
attach(day20)
#Plot the regression
plot(y=total_cases_per_million, x=gdp_per_capita)
abline(lm.fit2)


#Idee für Fragestellungen:
# - Finden wir Prädiktoren (Happiness Index, etc.), die die Anzahl d. Fälle besser vorhersagen, als das reine GDP?
# - Haben "advanced model" (z.B. Random Forests) dabei einen Vorteil zu linearen Modellen

#Join mit Happiness Index
happiness <- as_tibble(happiness)
happiness <- select(happiness, Country.or.region:Perceptions.of.corruption)
day20joined <- day20 %>%
  left_join(happiness, c("location" = "Country.or.region"))

#Linear Regression with Happiness factors
lm.fit2 <- lm(total_cases_per_million ~ GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data=day20joined)
summary(lm.fit2)
