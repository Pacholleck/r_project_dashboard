####################
# READ DATASETS
###################


# Set Directory

setwd("C:\\Users\\Adnan_Sevinc\\OneDrive - EPAM\\University\\2.Semester\\Advanced Programming in R 2400-DS1APR\\Project\\data")
getwd()

# Data set by country
poland <- read.csv("poland economic indicator from world data bank.csv")

# Data set by city
expenditure_education <- read.csv2("Expenditure per capita for education by city .csv")
expenditure_total <- read.csv2("Expenditure per capita grand total by city.csv")
average_salary <- read.csv2("Average monthly gross wages and salaries by city.csv")
academic_teacher <- read.csv2("Academic teachers by city.csv")
consumption_electricity <- read.csv2("Consumption of electricity by economic sectors by city.csv")
prices_index_consumer_good <- read.csv2("Price indices of consumer goods and services by city.csv")
unemployment <- read.csv2("Registered unemployment rate by city.csv")
retail_sale <- read.csv2("Retail sales of goods by city .csv")
revenue_per_capita <- read.csv2("Revenue per capita by city.csv")
road_accident <- read.csv2("Road traffic accidents; Years by city.csv")
urban_population <-read.csv2("Urban population in % of total population (half-yearly data) by city.csv")

datam <- list.files(expenditure_education,expenditure_total,average_salary,academic_teacher,consumption_electricity,
           prices_index_consumer_good,unemployment,retail_sale,revenue_per_capita,road_accident,urban_population)


##################
# Data Preparation
##################

datam <- c(expenditure_education,expenditure_total)

# x[,"Code"] <- NULL)




