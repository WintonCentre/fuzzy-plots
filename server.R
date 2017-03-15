library(shiny)
library(fanplot)
library(shinyjs)





shinyServer(function(input, output, session) {
  
  
  expand_df <- function(df, percentiles) {
    k <- nrow(df)
    bands <- length(percentiles)
    
    sd_unit = 1;
    if (!is.na(input$sd_unit) && !is.null(input$sd_unit) && input$sd_unit > 0)
      sd_unit = input$sd_unit
    
    val <- matrix(NA, nrow = bands, ncol = k)
    for (i in 1:k) {
      val[, i] <- qsplitnorm(p = percentiles,
                             mode = df$mode[i],
                             sd = df$sd[i] * sd_unit,
                             skew = 0)
    }
    return(val)
  }
  
  expand_med <- function(df, percentiles) {
    k <- nrow(df)
    med <- rep(NA, k)
    
    sd_unit = 1;
    if (!is.na(input$sd_unit) &&!is.null(input$sd_unit) && input$sd_unit > 0)
      sd_unit = input$sd_unit
    
    for (i in 1:k) {
      med[i] <- qsplitnorm(p = 0.5,
                           mode = df$mode[i],
                           sd = df$sd[i] * sd_unit,
                           skew = 0)
    }
    return(med)
  }
  
  smooth_df <- function(df, frequency) {
    k <- nrow(df)
    
    sd_unit = 1;
    if (!is.na(input$sd_unit) && !is.null(input$sd_unit) && input$sd_unit > 0)
      sd_unit = input$sd_unit
    
    smoothed_k <- frequency*(k - 1) + 1
    return(data.frame(#x = spline(df$x, n=smoothed_k)$y,
      mode = spline(df$mode, n=smoothed_k)$y,
      sd = spline(df$sd * sd_unit, n=smoothed_k)$y))
  }
  
  # expand <- function(df, percentiles) {
  #   k <- nrow(df)
  #   bands <- length(percentiles)
  #   
  #   val <- matrix(NA, nrow = bands, ncol = k)
  #   med <- rep(NA, k)
  #   for (i in 1:k) {
  #     val[, i] <- qsplitnorm(p = percentiles,
  #                            mode = df$mode[i],
  #                            sd = 4*df$sd[i],
  #                            skew = 0)
  #     med[i] <- qsplitnorm(p = 0.5,
  #                          mode = df$mode[i],
  #                          sd = 4*df$sd[i],
  #                          skew = 0)
  #   }
  #   return(list(expanded_val = val, expanded_med = med))
  # }
  
  smooth_and_expand <- function(df, smoothing, percentiles) {
    sdf <- smooth_df(df, smoothing)
    esdf <- expand_df(sdf, percentiles)
    med <- expand_med(sdf, percentiles)
    original_val <- expand_df(df, percentiles)
    return(list(original_val = original_val, smoothed_val = esdf, med = med))
  }
  
  
  load_data <- reactive({
    if (input$use_sample_data) {
      shinyjs::hide(id = "file1", anim = TRUE, animType = "slide", time = 0.33)
      #shinyjs::hideElement(id = "header", anim = TRUE, animType = "slide", time = 0.33)
      return(readRDS("data/sample.rds"))
    }
    else {
      shinyjs::show(id = "file1", anim = TRUE, animType = "slide", time = 0.33)
      #shinyjs::show(id = "header", anim = TRUE, animType = "slide", time = 0.33)
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      
      return(read.csv(inFile$datapath, header = TRUE))
    }
  })
  
  output$df <- renderDataTable({
    df1 <- load_data()
    if (is.null(df1))
      return(NULL)
    
    
    # Update dropdowns in UI with column names read from csv file
    internal_names <- c("mode","sd")
    for (i in 1:length(internal_names)) {
      previous_choice = input[[internal_names[i]]]
      if (previous_choice == "" || !(previous_choice %in% names(df1))) {
        updateSelectInput(session, internal_names[i],
                          choices = names(df1),
                          selected = internal_names[i])
      }
    }
    
    if (input$modeLabel == "" && input$mode != "") {
      updateTextInput(session, "modeLabel", value = input$mode)
    }
    
    
    # first parameter to renderDataTable is df1                                                                                                                                                                                  
    df1                                                                                                                                                                                                                          
  }, options = list(filter = FALSE, searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE))                                                                                                                          
  
  
  output$plot <- renderPlot({
    
  
    df1 <- load_data()
    if (is.null(df1))
      return(NULL)
    
    # Update dropdowns in UI with column names read from csv file
    internal_names <- c("mode","sd")
    for (i in 1:length(internal_names)) {
      previous_choice = input[[internal_names[i]]]
      if (previous_choice == "" || !(previous_choice %in% names(df1))) {
        updateSelectInput(session, internal_names[i],
                          choices = names(df1),
                          selected = internal_names[i])
      }
    }
    
    if (input$modeLabel == "" && input$mode != "") {
      updateTextInput(session, "modeLabel", value = input$mode)
    }
    
    if (input$mode == "" || input$sd == "")
      return(NULL)
    
    
    # redefine df1 with internal names, "x", "mode", and "sd".
    if (input$mode != "" && input$sd != "") {
      #      x <- df1[[input$x]]
      mode <- df1[[input$mode]]
      sd <- df1[[input$sd]]
      df1 <- data.frame(mode = mode, sd = sd)
    }
    
    # smooth data frame values and parameters
    # expand uncertainties
    smoothing <- 20
    smoothed_expanded <- smooth_and_expand(df = df1, smoothing = smoothing,
                                            percentiles = c(0.025, 0.15, 0.30, 0.70, 0.85, 0.975))
    
    sv <- smoothed_expanded$smoothed_val
    min_smoothed <- floor(min(sv[1, ]))
    max_smoothed <- ceiling(max(sv[nrow(sv),1]))
    
    original_val <- smoothed_expanded$original_val
    smoothed_val <- smoothed_expanded$smoothed_val
    med <- smoothed_expanded$med
    par(mar = c(4,4,4,4))    
    grid()                                                                                                                                                                                                                       
    plot((original_val[3,] + original_val[4,])/2,                                                                                 
         type = "p", pch = 18,                                                                                                                  xlim = c(1,ncol(original_val) + 1),
         ylim = c(min_smoothed, max_smoothed),
         xlab = input$xLabel,
         ylab = input$modeLabel,
         main = input$mainTitle)                                                                                                                                                                                                                        
    axis(1, at=time(1:ncol(original_val)), labels = TRUE)                                                                                                                                                                        
    
    if (input$expand) {                                                                                                                                                                                                          
      fan(smoothed_val, data.type = "values", start = start(smoothed_val),                                                                                                                                                   
          type = "interval",
          probs = c(0.70, 0.85, 0.975),
          fan.col = colorRampPalette(c("tomato", "gray90")), alpha = 0.5,
          frequency = smoothing)
    }
    
    if(!input$expand) {
      lines(ts(med, start = start(med), frequency = smoothing), col = "black")
    }
  })
  
})
