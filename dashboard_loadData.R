####################
# READ DATASETS
###################

  #dir_data <- "C:\\Users\\Adnan_Sevinc\\OneDrive - EPAM\\University\\2.Semester\\Advanced Programming in R 2400-DS1APR\\Project\\apr_project\\data"
  setwd(dir)
  
  # Data set by country
  poland <- read.csv("data/poland economic indicator from world data bank.csv")
  
  # Data set by city
  expenditure_education <- read.csv2("data/Expenditure per capita for education by city .csv", fileEncoding = "UTF-8")
  expenditure_total <- read.csv2("data/Expenditure per capita grand total by city.csv", fileEncoding = "UTF-8")
  average_salary <- read.csv2("data/Average monthly gross wages and salaries by city.csv",fileEncoding = "UTF-8")
  #academic_teacher <- read.csv2("data/Academic teachers by city.csv",fileEncoding = "UTF-8")
  consumption_electricity <- read.csv2("data/Consumption of electricity by economic sectors by city.csv",fileEncoding = "UTF-8")
  prices_index_consumer_good <- read.csv2("data/Price indices of consumer goods and services by city.csv",fileEncoding = "UTF-8")
  unemployment <- read.csv2("data/Registered unemployment rate by city.csv",fileEncoding = "UTF-8")
  retail_sale <- read.csv2("data/Retail sales of goods by city .csv",fileEncoding = "UTF-8")
  revenue_per_capita <- read.csv2("data/Revenue per capita by city.csv",fileEncoding = "UTF-8")
  road_accident <- read.csv2("data/Road traffic accidents; Years by city.csv",fileEncoding = "UTF-8")
  urban_population <-read.csv2("data/Urban population in % of total population (half-yearly data) by city.csv",fileEncoding = "UTF-8")
  
  
  
  ##################
  # Data Preparation
  ##################
  
  #poland
  
  
  poland <- slice(poland, -c(32:37))  #for deleting rows
  
  polcolnames <- c("Country"	,
                   "Code"	,
                   "Time"	,
                   "Time.Code"	,
                   "GDP.current.US"	,
                   "GDP.per.capita.current.US"	,
                   "GDP.growth.annual"	,
                   "GDP.per.capita.PPP"	,
                   "CPI"	,
                   "Birth.rate"	,
                   "Fertility.rate"	,
                   "Life.expectancy.female"	,
                   "Life.expectancy.male"	,
                   "CO2"	,
                   "Energy.imports"	,
                   "Energy.use"	,
                   "Fossil.fuel.energy.consumption"	,
                   "Renewable.energy.consumption"	,
                   "Education.expenditure.current.US"	,
                   "Compulsory.education"	,
                   "Military.expenditure.of.GDP"	,
                   "Health.expenditure.of.GDP"	,
                   "Imports"	,
                   "Exports"	,
                   "Inflation.GDP.deflator.annual"	
  )
  
  
  for (i in 1:length(colnames(poland))){
    colnames(poland)[i] <- polcolnames[i]
  }
  
  poland$Time.Code <- NULL
  
  poland$Time <- as.Date(paste(poland$Time, 1, 1, sep = "-"))
            
  lastUpdate <- max(poland$Time)
  
  for (i in 4:length(colnames(poland))){
    poland[i] = as.numeric(poland[,i])
  }
  
  reg_data <-na.omit(poland)
  
  for (i in 4:length(colnames(reg_data))){
    reg_data[i] = log(reg_data[,i])
  }
  
  
  
  ###########################
  ########Voivodships########
  ###########################
  
  average_salary <- 
    average_salary %>% 
    select(-c(grand.total.2021..PLN.))
  expenditure_education <- 
    expenditure_education %>% 
    select(-c(for.education.2021..PLN.))
  expenditure_total <- 
    expenditure_total %>% 
    select(-c(grand.total.2021..PLN.))
  consumption_electricity <- 
    consumption_electricity %>% 
    select(-c(total.2021..GWh.))
  
  
  prices_index_consumer_good_total <- prices_index_consumer_good[,1:19]
  prices_index_consumer_good_food <- prices_index_consumer_good[,c(1,2,seq(20,36))]
  prices_index_consumer_good_alc <- prices_index_consumer_good[,c(1,2,seq(37,53))]
  prices_index_consumer_good_clothing <- prices_index_consumer_good[,c(1,2,seq(54,70))]
  prices_index_consumer_good_dwelling <- prices_index_consumer_good[,c(1,2,seq(71,87))]
  prices_index_consumer_good_health <- prices_index_consumer_good[,c(1,2,seq(88,104))]
  prices_index_consumer_good_transport <- prices_index_consumer_good[,c(1,2,seq(105,121))]
  prices_index_consumer_good_recreation <- prices_index_consumer_good[,c(1,2,seq(122,138))]
  prices_index_consumer_good_education <- prices_index_consumer_good[,c(1,2,seq(139,155))]
  
  
  
  dataframes <- list(#academic_teacher=academic_teacher,
                     average_salary=average_salary,
                     consumption_electricity=consumption_electricity,
                     expenditure_total=expenditure_total,
                     expenditure_education=expenditure_education,
                     unemployment=unemployment[,1:19],
                     retail_sale=retail_sale,
                     road_accident = road_accident,
                     revenue_per_capita = revenue_per_capita,
                     prices_index_consumer_good_education = prices_index_consumer_good_education,
                     prices_index_consumer_good_recreation = prices_index_consumer_good_recreation,
                     prices_index_consumer_good_transport = prices_index_consumer_good_transport,
                     prices_index_consumer_good_dwelling=prices_index_consumer_good_dwelling,
                     prices_index_consumer_good_clothing=prices_index_consumer_good_clothing,
                     prices_index_consumer_good_alc=prices_index_consumer_good_alc,
                     prices_index_consumer_good_food =prices_index_consumer_good_food,
                     prices_index_consumer_good_total=prices_index_consumer_good_total
                     )
  
  basic_clean <- function(dataset){
    col_num <- as.character(c(seq(2005,2021)))
    if("X" %in% colnames(dataset) == TRUE) {dataset %>% select(-X)}
    colnames(dataset) <- c('Code', 'Voivoidship',seq(2005,2021))
    dataset<- dataset[!is.na(names(dataset))]
    dataset[col_num] <- sapply(dataset[col_num],as.numeric)
    return (dataset)
  }
  
  dataframes <- lapply(dataframes, basic_clean) 
  


  
  dataframes <- lapply(dataframes,function(x) melt(x, id = c('Code','Voivoidship'),variable.name = "Year"))
  
  df_names <- names(dataframes)
  for (i in 1:length(df_names)){
    colnames(dataframes[[i]]) <- c('Code', 'Voivoidship','Year',df_names[i])
  }

  df_voivodships <- Reduce(function(x, y) merge(x, y, all=TRUE), dataframes)
  df_voivodships$Year <- as.numeric(as.character(df_voivodships$Year))
  
  