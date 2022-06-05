ui <- fluidPage(
  navbarPage("Poland Economic Indicators",
             tabPanel("WDO data",
                      titlePanel("Indicators Over Poland"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("indicator", h5("Select indicator"),
                                      choices = colnames(poland[,c(4:length(colnames(poland)))]), 
                                      selected = "GDP.growth.annual", 
                                      multiple = TRUE,
                                      selectize = TRUE
                          ),
                          dateRangeInput("date", h5("Date range"),
                                         start = "1990-01-01",
                                         end = lastUpdate,
                                         min = "1990-01-01",
                                         max = lastUpdate),
                          textInput("title", "Title of chart:"),
                          actionButton("accept_title", "change title")
                        ),
                        mainPanel(
                          plotOutput("plt1")
                        ))
                      
             ),
             tabPanel("Econometric Analysis",
                      titlePanel("Regressin Analysis"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "xcol", 
                            label = h3("Independent Variable(s)"),
                            choices = colnames(poland[,c(4:length(colnames(poland)))])), # List of values to select from
                          selectInput(inputId = "ycol", 
                                      label = h3("Dependent Variable"), 
                                      choices = colnames(poland[,c(4:length(colnames(poland)))]), 
                          )
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Summary", h3("Summary of Regression Model"), verbatimTextOutput("summary")),
                                      tabPanel("Residuals",
                                               
                                               fluidRow(
                                                 column(6,  h3("Histogram of Residuals"),plotOutput("plt2")),
                                                 column(10, h3("Jarque-Bera test "),verbatimTextOutput("jb")),
                                               )),
                                      
                                      tabPanel("Data", DT::dataTableOutput('datam'))
                                      
                          )))
                      
                      
             ))
)

