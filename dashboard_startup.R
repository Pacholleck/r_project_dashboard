
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
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

list.files(path=".", pattern=NULL, all.files=FALSE,
           full.names=FALSE)

###############################################################################
                           ### load data  ###

source("dashboard_loadData.R")


###############################################################################
                          ### shiny_ui  ###
source("dashboard_shiny_ui.R")


###############################################################################
                    ### shiny_server  ###

source("dashboard_shiny_server.R")


###############################################################################
                      ### start ShinyApp  ###

shinyApp(ui = ui, server = server)





