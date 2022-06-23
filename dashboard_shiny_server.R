# Shiny Server which processes the inputs
server <- function(input, output, session) {
   
#### Economic indicators Poland ####
  
  # get manual plot title
  plot_title <- reactive({
    Sys.sleep(1)
    input$accept_title
    isolate(input$title) 
  })
  
  
  # Validate Input
  output$plt1 <- renderPlot({
    validate(
      need(input$indicator != "", "Please provide at least one indicator!"))
  
  # Process the indicator data
    Sys.sleep(1)
    indicators <-
      poland %>% 
      filter(between(year(Time),input$date[1],input$date[2])) %>% 
      select(Time,input$indicator) %>%
      melt(id=c("Time")) %>%
      mutate(variable = as.character(variable), value = as.numeric(value))
    
    # Plot data
    ggplot(indicators, aes(x = Time, y = value)) + 
      geom_line(aes(color = variable, linetype = variable)) + 
      xlab("Year") +
      ylab("Values") +
      scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
      theme(plot.title = element_text(size = 30, hjust = 0.5))
    
  })
  
  
#### Voivodeships ####
  
  # Validate Input
  output$plt3 <- renderPlot({
    validate(
      need(input$Voivodeship != "", "Please provide at least one Voivodeship!"))

    # Process the indicator data
    Sys.sleep(1)
    indicators1 <-
      df_Voivodeship %>%
      filter(between(Year,input$date1[1],input$date1[2]), Voivodeship %in% input$Voivodeship) %>%
      select(Year,Voivodeship, input$indicator1)


    # Plot data
    ggplot(indicators1, aes(x = Year, y = indicators1[,3])) +
      geom_line(aes(color = Voivodeship, linetype = Voivodeship)) +
      xlab("Year") +
      ylab("Values") +
      ggtitle(plot_title())+
      theme(plot.title = element_text(size = 30, hjust = 0.5))

  })

#### Consumer Price Index ####
  
  # Validate Input
  output$plt4 <- renderPlot({
    validate(
      need(input$Voivodeship != "", "Please provide at least one Voivodeship!"),
      need(input$indicator2 != "", "Please provide at least one Indicator!")
      )
    
    # Process the indicator data
    indicators2 <-
      df_Voivodeship %>%
      filter(between(Year,input$date2[1],input$date2[2]), Voivodeship %in% input$Voivodeship2) %>%
      select(Year,Voivodeship, input$indicator2)
    
    # Plot data
    plot_list <- list()
    
    for (x in 3:(length(input$indicator2)+2)){
      
      plot_list[[x-2]] <- 
        p <- eval(substitute(
          ggplot(indicators2, aes(x = Year, y = indicators2[,x])) + 
            geom_line(aes(color = Voivodeship, linetype = Voivodeship)) + 
            xlab("Year") +
            ylab(input$indicator2[x-2]) +
            ggtitle(plot_title())+
            theme(plot.title = element_text(size = 30, hjust = 0.5))
          , list(x=x)))
      
    }
    
    multiplot(plotlist = plot_list, cols = 2)

  })
  
#### Economometric Model ####
  
  #Obtaining Residuals
  
  regression_1<-reactive({
    validate(
      need(input$xcol != "" | input$ycol != "", "Please provide a Regression dimensions!")
    )
    
    x <- as.numeric(reg_data[[as.name(input$xcol)]])
    y <- as.numeric(reg_data[[as.name(input$ycol)]])
    model = lm(y ~ x, data = reg_data)
    data.frame(residuals_1 =lm(y ~ x, data = reg_data)$residuals)
    
  })
  
  
  # Histogram of residuals
  
  output$plt2<-renderPlot({
    ggplot(data = regression_1(), aes(x=regression_1()$residuals_1)) + 
      geom_histogram()+
      theme_classic()
    
    
  })
  
  #JB test
  output$jb <- renderPrint({
    jarque.bera.test(regression_1()$model$residuals)
    
  })
  
  
  
  #Regression Summary
  
  output$summary <- renderPrint({
    
    x <- as.numeric(reg_data[[as.name(input$xcol)]])
    y <- as.numeric(reg_data[[as.name(input$ycol)]])
    
    fit <- lm(y ~ x, data = reg_data)
    names(fit$coefficients) <- c("Intercept", input$xcol)
    summary(fit)
  })
  
  # Data output
  
  output$datam <- DT::renderDataTable({
    DT::datatable(reg_data[,c(input$xcol,input$ycol)], 
                  options = list(searching = FALSE,
                                 lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                                 
                                 pageLength = 15,
                                 autoWidth = TRUE), 
                  rownames = FALSE)
  }) 
}





############ Helper for Multiplot ###############
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

