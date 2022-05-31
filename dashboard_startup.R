# libraries
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(shiny)
library(maps)
library(ggiraph)
library(scales)

# set working directory
dir_default = "C:/Users/duzzi/OneDrive/University of Warsaw/2nd Semester/Advanced R Programming/Advanced_R_Project/r_project_dashboard"
setwd(dir_default)

# load helper functions
source("dashboard_helperFunctions.R")
# load data
source("dashboard_loadData.R")
# load chart generators
source("dashboard_charts.R")
# load shiny app components
source("dashboard_shiny_ui.R")
source("dashboard_shiny_server.R")

# start ShinyApp
shinyApp(ui = ui, server = server)


