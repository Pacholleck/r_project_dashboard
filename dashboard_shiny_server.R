server <- function(input, output, session) {
  
  
  #First Dashboard
  
  plot_title <- reactive({
    Sys.sleep(2)
    input$accept_title
    isolate(input$title) 
  })
  
  poland
  
  output$plt1 <- renderPlot({
    Sys.sleep(2)
    indicators <-
      poland %>% 
      filter(Time > input$date[1] & Time < input$date[2]) %>% 
      select(Time,input$indicator) %>%
      melt(id=c("Time")) %>%
      mutate(variable = as.character(variable), value = as.numeric(value))
    
    
    
    ggplot(indicators, aes(x = Time, y = value)) + 
      geom_line(aes(color = variable, linetype = variable)) + 
      xlab("Year") +
      ylab("Values") +
      scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
      ggtitle(plot_title())+
      theme(plot.title = element_text(size = 30, hjust = 0.5))
    
  })
  
  #Obtaining Residuals
  
  regression_1<-reactive({
    
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

