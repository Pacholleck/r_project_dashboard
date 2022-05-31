####################
# READ DATASETS
###################


# Set Directory

setwd("")
getwd()

# Data set by country
poland <- read.csv("poland economic indicator from world data bank.csv")

# Data set by city
expenditure_education <- read.csv2("Expenditure per capita for education by city .csv", fileEncoding = "UTF-8")
expenditure_total <- read.csv2("Expenditure per capita grand total by city.csv", fileEncoding = "UTF-8")
average_salary <- read.csv2("Average monthly gross wages and salaries by city.csv",fileEncoding = "UTF-8")
academic_teacher <- read.csv2("Academic teachers by city.csv",fileEncoding = "UTF-8")
consumption_electricity <- read.csv2("Consumption of electricity by economic sectors by city.csv",fileEncoding = "UTF-8")
prices_index_consumer_good <- read.csv2("Price indices of consumer goods and services by city.csv",fileEncoding = "UTF-8")
unemployment <- read.csv2("Registered unemployment rate by city.csv",fileEncoding = "UTF-8")
retail_sale <- read.csv2("Retail sales of goods by city .csv",fileEncoding = "UTF-8")
revenue_per_capita <- read.csv2("Revenue per capita by city.csv",fileEncoding = "UTF-8")
road_accident <- read.csv2("Road traffic accidents; Years by city.csv",fileEncoding = "UTF-8")
urban_population <-read.csv2("Urban population in % of total population (half-yearly data) by city.csv",fileEncoding = "UTF-8")



##################
# Data Preparation
##################

#Expenditure education

library(tidyverse)
library(dplyr)

expenditure_education <- 
  expenditure_education %>% 
  select(-c(for.education.2021..PLN.,X))
 
for (i in 2:18){
  colnames(expenditure_education)[i] <- substr(colnames(expenditure_education)[i],15,18)
}

colnames(expenditure_education)[2] <- "City"

#Expenditure Total

expenditure_total <- 
  expenditure_total %>% 
  select(-c(grand.total.2021..PLN.,X))

for (i in 2:18){
  colnames(expenditure_total)[i] <- substr(colnames(expenditure_total)[i],13,16)
}

colnames(expenditure_total)[2] <- "City"


#consumption_electricity

consumption_electricity <- 
  consumption_electricity %>% 
  select(-c(total.2021..GWh.,X))

for (i in 2:18){
  colnames(consumption_electricity)[i] <- substr(colnames(consumption_electricity)[i],7,10)
}

colnames(consumption_electricity)[2] <- "City"

#average salary
average_salary <- 
  average_salary %>% 
  select(-c(grand.total.2021..PLN.,X))


for (i in 2:18){
  colnames(average_salary)[i] <- substr(colnames(average_salary)[i],13,16)
}

colnames(average_salary)[2] <- "City"

#academic_teacher

academic_teacher <- 
  academic_teacher %>% 
  select(-X)

for (i in 2:16){
  colnames(academic_teacher)[i] <- substr(colnames(academic_teacher)[i],24,27)
}

colnames(average_salary)[2] <- "City"

#poland


poland <- slice(poland, -c(32:37))  #for deleting rows

polcolnames <- c("Country.Name"	,
                 "Country.Code"	,
                 "Time"	,
                 "Time.Code"	,
                 "GDP.current.US"	,
                 "GDP.per.capita.current.US"	,
                 "GDP.growth.annual"	,
                 "GDP.per.capita.PPP.current.international"	,
                 "Consumer.price.index.2010.100"	,
                 "Birth.rate.crude.per.1.000.people"	,
                 "Fertility.rate.total.births.per.woman"	,
                 "Life.expectancy.at.birth.female.years"	,
                 "Life.expectancy.at.birth.male.years"	,
                 "CO2.emissions.metric.tons.per.capita"	,
                 "Energy.imports.net.of.energy.use"	,
                 "Energy.use.kg.of.oil.equivalent.per.capita"	,
                 "Fossil.fuel.energy.consumption"	,
                 "Renewable.energy.consumption."	,
                 "Adjusted.savings.education.expenditure.current.US."	,
                 "Compulsory.education.duration.years."	,
                 "Military.expenditure.of.GDP"	,
                 "Current.health.expenditure.of.GDP"	,
                 "Imports.of.goods.and.services.current.LCU"	,
                 "Exports.of.goods.and.services.current.LCU"	,
                 "Inflation.GDP.deflator.annual"	
)


for (i in 1:length(colnames(poland))){
  colnames(poland)[i] <- polcolnames[i]
}

poland$Time.Code <- NULL


basic_clean <- function(dataset){
  col_num <- as.character(c(seq(2005,2021)))
  if("X" %in% colnames(dataset) == TRUE) {dataset[,"X"] <- NULL}
  colnames(dataset) <- c('Code', 'Name',seq(2005,2021))
  dataset[col_num] <- sapply(dataset[col_num],as.numeric)
  return (dataset)
}

prices_index_consumer_good_total <- prices_index_consumer_good[,1:19]
prices_index_consumer_good_total <- basic_clean(prices_index_consumer_good_total)

prices_index_consumer_good_food <- prices_index_consumer_good[,c(1,2,seq(20,36))]
prices_index_consumer_good_food <- basic_clean(prices_index_consumer_good_food)

prices_index_consumer_good_alc <- prices_index_consumer_good[,c(1,2,seq(37,53))]
prices_index_consumer_good_alc <- basic_clean(prices_index_consumer_good_alc)

prices_index_consumer_good_clothing <- prices_index_consumer_good[,c(1,2,seq(54,70))]
prices_index_consumer_good_clothing <- basic_clean(prices_index_consumer_good_clothing)

prices_index_consumer_good_dwelling <- prices_index_consumer_good[,c(1,2,seq(71,87))]
prices_index_consumer_good_dwelling <- basic_clean(prices_index_consumer_good_dwelling)

prices_index_consumer_good_health <- prices_index_consumer_good[,c(1,2,seq(88,104))]
prices_index_consumer_good_health <- basic_clean(prices_index_consumer_good_health)

prices_index_consumer_good_transport <- prices_index_consumer_good[,c(1,2,seq(105,121))]
prices_index_consumer_good_total <- basic_clean(prices_index_consumer_good_total)

prices_index_consumer_good_recreation <- prices_index_consumer_good[,c(1,2,seq(122,138))]
prices_index_consumer_good_recreation <- basic_clean(prices_index_consumer_good_recreation)

prices_index_consumer_good_education <- prices_index_consumer_good[,c(1,2,seq(139,155))]
prices_index_consumer_good_education <- basic_clean(prices_index_consumer_good_education)

urban_population <- basic_clean(urban_population)
road_accident <- basic_clean(road_accident)
revenue_per_capita <- basic_clean(revenue_per_capita)
retail_sale <- basic_clean(retail_sale)
unemployment <- basic_clean(unemployment)
