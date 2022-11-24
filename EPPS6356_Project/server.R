function(input, output){
  # Input parameters ----
  
  # Input for DESC page Frequency
  
    # Select variable for chart
    DescPieVar <- reactive({
      
      # Replace space with . in variable name
      gsub(" ", ".", input$DescRadio)
    })
    

  # Input for INF page COEF
    
    #Select which Models to Display
    InfCoefModel <- reactive({
      as.numeric(gsub("Stage ", "", input$InfCoefCheck))
    })
    
    # Input for INF page COEF Plot
    
    #Select which Models to Display
    InfCoefPlot <- reactive({
      as.numeric(gsub("Stage ", "", input$PlotCoefCheck))
    })
    
    
  # Input for INF page PROB
  
    # Select which Models to Display
    InfProbModel <- reactive({
      as.numeric(gsub("Stage ", "", input$InfProbCheck))
    })
    
    # Input Parameter Transformations
    InfProbInter <- reactive({
      switch(input$InfInterInput,
             "True" = 1,
             "False" = 0)
    })
    
    InfProbTime <- reactive({
      switch(input$InfTimeInput,
             "Morning" = c(0,0,0),
             "Day" = c(1,0,0),
             "Evening" = c(0,1,0),
             "Night" = c(0,0,1))
    })
    
    InfProbWeath <- reactive({
      switch(input$InfWeathInput,
             "Normal Weather" = 0,
             "Dangerous Weather" = 1)
    }) 
    
    InfProbColl <- reactive({
      switch(input$InfCollInput,
             "One Car" = c(0,0,0,0),
             "Angular" = c(1,0,0,0),
             "Opposite Direction" = c(0,1,0,0),
             "Other" = c(0,0,1,0),
             "Same Direction" = c(0,0,0,1))
    })
  
  # Output functions ----
  # Desc Page, Frequency Plot
  output$FreqPlot <- renderTable({
    
    # Create a frequency table the of Selected Variable
    tab <- table(crash[,DescPieVar()])
  })
  
    
  # Desc Page, Pie Plot
  output$PiePlot <- renderPlotly({
    # Create a pie chart of the Selected Variable
    
    counts <-  crash %>% count(across(input$DescRadio2))
    
    plot_ly(data = counts, values = ~n, labels = ~get(input$DescRadio2), 
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent', 
            width = 800, height = 800) %>% 
      layout(title = "Makeup of Accident Data by Variable")
    })
  
  
  # Desc Page, Bar Plot
  output$BarPlot <- renderPlotly({
    # Create a Bar Plot of the Selected Variable
  
    factor_data <- data.frame(lapply(crash, factor))
    
    bar_var <- data.frame(factor_data %>% count(across(input$DescRadio3)))
    
    bar_var <- bar_var %>% 
      mutate(perc = paste0(sprintf("%4.1f", n / sum(n) * 100), "%"))
    
    plot_ly(data = bar_var, x = ~get(input$DescRadio3), y = ~n,
            type = "bar", text = ~paste0(sprintf("%4.1f", n / sum(n) * 100), "%"),
            textposition = "outside") %>% 
      layout(xaxis = list(categoryorder = "total ascending"),
             yaxis = list(title = "Number of Accidents"),
             title = "Makeup of Accident Data by Variable")
  })
  
  # Inf Page Coefficient Dot-Whisker Plot
  output$CoefPlot <- renderPlot({
    
    # for loop to remove non-selected models
    seq <- c()
    for (i in InfCoefPlot()){
      seq <- c(seq,seq(i,32,4))
    }
    
    # reduce data to just specified models
    coeff.red <- coeff.matrix[c(sort(seq),33:42),]
    
    dwplot(coeff.red)
  })
  

  # Inf Page Coefficient Table
  output$InfCoefPlot <- renderTable({
    
    # for loop to remove non-selected models
    seq <- c()
    for (i in InfCoefModel()){
      seq <- c(seq,seq(i,32,4))
    }
    
    # reduce data to just specified models
    coeff.red <- coeff.matrix[c(sort(seq),33:42),]
    
    return(coeff.red)
    
  })

  
  
  # Inf Page Probability Plot
  output$InfProbPlot <- renderTable({
    
    # Create a vector of parameters for probability
    probparameter <- c(input$InfSpeedInput,InfProbWeath(),InfProbTime(),InfProbColl(),InfProbInter())
    
    # Create the coefficient matrix to get the log odds
    estmat <- matrix(0, nrow = length(InfProbModel()), ncol = 18)
    for (i in InfProbModel()){
      # This is to allow the for function to properly put values into the estmat matrix
      i2 <- match(i, InfProbModel())
      
      # Just pulling coefficient values from coeff.matrix
      estmat[i2,] <- coeff.matrix$estimate[c(seq(i,32,4),33:42)]
    }
    
    # Create the parameter matrix to get the log odds
    parmat <- matrix(0, nrow = 18, ncol = 8)
    for (i in 1:8){
      
      # Creates a vector for each Road Class with the shiny parameters afterwards
      parmat[,i] <-  c(1,rep(0,max(0,i-2)),ifelse(i == 1,0,1),rep(0,min(6,8-i)),probparameter)
    }
    
    # Create matrix that expresses confidence intervals of predictions
    probci <- matrix(0, nrow = length(InfProbModel()), ncol = 8)
    for (i in InfProbModel()) {
      
      # Describe for parameters
      i2 <- match(i, InfProbModel())
      a <- qt(input$InfAlphInput/200, (96854 + 42)/4, lower.tail = F)
      
      # Creating a varying variance-covariance matrix that depends on which stage we are looking at
      vvc <- vcov[c(seq(i,32,4),33:42),c(seq(i,32,4),33:42)]
      
      # extracting the diagonal values of standard error calculation
      # non-diagonal values are garbage values with no meaning
      probci[i2,] <- sqrt(diag(t(parmat) %*% vvc %*% parmat)) * a
    }
    
    # Create a matrix that is the predicted log odds
    # Rows are models, columns are Road classes
    probest <- as.data.frame(estmat %*% parmat)
    
    # Transforms log odds into probability using e^y / (e^y + 1) where y is the predicted log odds
    probprob <- exp(probest) / (exp(probest) + 1)
    
    # Constructing upper and lower intervals for prob estimates
    probupper <-  exp(probest + probci) / (exp(probest + probci) + 1)
    problower <- exp(probest - probci) / (exp(probest - probci) + 1)
    
    # After conversion into data frame, label columns
    colnames(probprob) <- c("County Road", "City Street", "Farm to Market", "Interstate", "Non-Trafficway", "Other Road", "Tollways", "US & State Highways")
    
    #Objects to worry about:
    #probest - Estimated log odds of model
    #probprob - Estimated probabilities of model
    #probupper - Upper bound of calculated probabilities
    #problower - Lower bound of calculated probabilities

    
    return(problower)
  }, digits = 3)
  
}