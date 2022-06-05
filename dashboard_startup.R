
###############################################################################
                     ### libraries  ###


requiredPackages = c("data.table", 
                     "dplyr", 
                     "tidyr",
                     "tidyverse",
                     "ggplot2", 
                     "maps",
                     "shiny",
                     "maps",
                     "ggiraph",
                     "scales",
                     "tseries",
                     "lmtest")

for(i in requiredPackages){
  for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
  for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) } 
}


###############################################################################
                         ### set working directory ###
dir  <- "C:\\Users\\Adnan_Sevinc\\OneDrive - EPAM\\University\\2.Semester\\Advanced Programming in R 2400-DS1APR\\Project\\apr_project"
setwd(dir)

list.files(path=".", pattern=NULL, all.files=FALSE,
           full.names=FALSE)

###############################################################################
                           ### load data  ###

source("dashboard_loadData.R")


###############################################################################
                          ### shiny_ui  ###
setwd(dir)
source("dashboard_shiny_ui.R")


###############################################################################
                    ### shiny_server  ###

source("dashboard_shiny_server.R")


###############################################################################
                      ### start ShinyApp  ###

shinyApp(ui = ui, server = server)





