ui <- fluidPage(
  navbarPage("Poland",
             tabPanel("All Poland",
                      titlePanel("Indicators Over Poland"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("indicator", h5("Select indicator"),
                                      choices = colnames(poland[,c(4:length(colnames(poland)))]), 
                                      selected = "GDP.growth.annual", 
                                      multiple = TRUE,
                                      selectize = TRUE
                          ),
                          sliderInput("date", h5("Date range"),
                                         min = 1990,
                                         max = 2020,
                                         value = c(1990,2020),
                                         sep = ""),
                          textInput("title", "Title of chart:"),
                          actionButton("accept_title", "change title")
                        ),
                        mainPanel(
                          plotOutput("plt1")
                        ))
                      
             ),
             tabPanel("Econometric Analysis",
                      titlePanel("Regression Analysis"),
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
                      
                      
             )
             ,
             tabPanel("Voivodeships",
                      titlePanel("Indicators Within Voivodeships"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("indicator1", h5("Select indicator"),
                                      choices = colnames(df_Voivodeship[,c(4:11)]),
                                      selected = "average_salary",
                                      multiple = FALSE,
                                      selectize = TRUE
                          ),
                          selectInput("Voivodeship", h5("Select Voivodeship"),
                                      choices = unique(df_Voivodeship$Voivodeship),
                                      selected = "POLAND",
                                      multiple = TRUE,
                                      selectize = TRUE
                          ),
                          sliderInput("date1", h5("Date range"),
                                      min = 2005,
                                      max = 2021,
                                      value = c(2005,2021),
                                      sep = ""),
                          textInput("title", "Title of chart:"),
                          actionButton("accept_title", "change title")
                        ),
                        mainPanel(
                          plotOutput("plt3")
                        ))

             )
             ,
             tabPanel("Consumer Price Index",
                      titlePanel("Prices Within Voivodeships"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("indicator2", h5("Select indicator"),
                                      choices = colnames(df_Voivodeship[,c(12:length(colnames(df_Voivodeship)))]),
                                      selected = "prices_index_consumer_good_total",
                                      multiple = TRUE,
                                      selectize = TRUE
                          ),
                          selectInput("Voivodeship2", h5("Select Voivodeship"),
                                      choices = unique(df_Voivodeship$Voivodeship),
                                      selected = "POLAND",
                                      multiple = TRUE,
                                      selectize = TRUE
                          ),
                          sliderInput("date2", h5("Date range"),
                                      min = 2005,
                                      max = 2021,
                                      value = c(2005,2021),
                                      sep = "")
                        ),
                        mainPanel(
                          plotOutput("plt4")
                        ))
                      
             )
             
             
             )
)

