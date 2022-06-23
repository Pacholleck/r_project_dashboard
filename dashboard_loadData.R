####################
# READ DATASETS
###################

  setwd(dirname(rstudioapi::getSourceEditorContext()$path))  

  # Data set by country
  poland <- read.csv("data/poland economic indicator from world data bank.csv")
  
  # Data set by voivodship
  expenditure_education <- read.csv2("data/Expenditure per capita for education by city .csv", fileEncoding = "UTF-8")
  expenditure_total <- read.csv2("data/Expenditure per capita grand total by city.csv", fileEncoding = "UTF-8")
  average_salary <- read.csv2("data/Average monthly gross wages and salaries by city.csv",fileEncoding = "UTF-8")
  #academic_teacher <- read.csv2("data/Academic teachers by city.csv",fileEncoding = "UTF-8")
  consumption_electricity <- read.csv2("data/Consumption of electricity by economic sectors by city.csv",fileEncoding = "UTF-8")
  CPI <- read.csv2("data/Price indices of consumer goods and services by city.csv",fileEncoding = "UTF-8")
  unemployment <- read.csv2("data/Registered unemployment rate by city.csv",fileEncoding = "UTF-8")
  retail_sale <- read.csv2("data/Retail sales of goods by city .csv",fileEncoding = "UTF-8")
  revenue_per_capita <- read.csv2("data/Revenue per capita by city.csv",fileEncoding = "UTF-8")
  road_accident <- read.csv2("data/Road traffic accidents; Years by city.csv",fileEncoding = "UTF-8")
  urban_population <-read.csv2("data/Urban population in % of total population (half-yearly data) by city.csv",fileEncoding = "UTF-8")
  
  
  
  ##################
  #Data Preparation#
  ##################
  
  ###########################
  ##########Poland###########
  ###########################
  
  
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
  
  
  for (i in 1:length(colnames(poland))){    #Rename columns
    colnames(poland)[i] <- polcolnames[i]
  }
  
  poland$Time.Code <- NULL   #Drop Code Column
  
  poland$Time <- as.Date(paste(poland$Time, 1, 1, sep = "-"))  #set data type
            
  
  for (i in 4:length(colnames(poland))){      #converting to numeric
    poland[i] = as.numeric(poland[,i])
  }
  
  reg_data <-na.omit(poland)  #Drop NA
  
  for (i in 4:length(colnames(reg_data))){  #Taking Logarithmic
    reg_data[i] = log(reg_data[,i])
  }
  
  
  
  ###########################
  ########Voivodships########
  ###########################
  
  # Remove specific columns
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
  
  #Subset CPI into different dataframes
  CPI_total <- CPI[,1:19]
  CPI_food <- CPI[,c(1,2,seq(20,36))]
  CPI_alc <- CPI[,c(1,2,seq(37,53))]
  CPI_clothing <- CPI[,c(1,2,seq(54,70))]
  CPI_dwelling <- CPI[,c(1,2,seq(71,87))]
  CPI_health <- CPI[,c(1,2,seq(88,104))]
  CPI_transport <- CPI[,c(1,2,seq(105,121))]
  CPI_recreation <- CPI[,c(1,2,seq(122,138))]
  CPI_education <- CPI[,c(1,2,seq(139,155))]
  
  
  # Dataframes to preprocess
  dataframes <- list(#academic_teacher=academic_teacher,
                     average_salary=average_salary,
                     consumption_electricity=consumption_electricity,
                     expenditure_total=expenditure_total,
                     expenditure_education=expenditure_education,
                     unemployment=unemployment[,1:19],
                     retail_sale=retail_sale,
                     road_accident = road_accident,
                     revenue_per_capita = revenue_per_capita,
                     CPI_total=CPI_total,
                     CPI_food =CPI_food,
                     CPI_alc=CPI_alc,
                     CPI_education = CPI_education,
                     CPI_recreation = CPI_recreation,
                     CPI_transport = CPI_transport,
                     CPI_dwelling=CPI_dwelling,
                     CPI_clothing=CPI_clothing
                     
                     
                     
                     )
  
  # Basic data cleaning
  basic_clean <- function(dataset){
    col_num <- as.character(c(seq(2005,2021)))
    if("X" %in% colnames(dataset) == TRUE) {dataset %>% select(-X)}
    colnames(dataset) <- c('Code', 'Voivodeship',seq(2005,2021))
    dataset<- dataset[!is.na(names(dataset))]
    dataset[col_num] <- sapply(dataset[col_num],as.numeric)
    return (dataset)
  }
  
  dataframes <- lapply(dataframes, basic_clean) 
  
  # Melt precleaned data
  dataframes <- lapply(dataframes,function(x) melt(x, id = c('Code','Voivodeship'),variable.name = "Year"))
  
  # Change columnnames
  df_names <- names(dataframes)
  for (i in 1:length(df_names)){
    colnames(dataframes[[i]]) <- c('Code', 'Voivodeship','Year',df_names[i])
  }

  # Reduce nested list into one dataframe
  df_Voivodeship <- Reduce(function(x, y) merge(x, y, all=TRUE), dataframes)
  df_Voivodeship$Year <- as.numeric(as.character(df_Voivodeship$Year))
  
  
