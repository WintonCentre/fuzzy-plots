library(shiny)
library(fanplot)

percentiles <- c(0.025, 0.15, 0.30, 0.70, 0.85, 0.975)
bands <- length(percentiles)

smoothing <- 10
smooth <- function(k) { smoothing*(k - 1) + 1 }



shinyServer(function(input, output) {
  
  netstats <- reactive({
  
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    df1 <- read.csv(inFile$datapath, header = input$header)
    
    k <- nrow(df1)
    
    val <- matrix(NA, nrow = bands, ncol = k)
    med <- rep(NA, k)
    for (i in 1:k) {
      val[, i] <- qsplitnorm(p = percentiles, 
                             mode = df1$mode[i],
                             sd = df1$sd[i],
                             skew = df1$skew[i])
      med[i] <- qsplitnorm(p = 0.5,
                           mode = df1$mode[i],
                           sd = df1$sd[i],
                           skew = df1$skew[i])
    }
    
    ik <- smooth(k)
    smoothed_val <- matrix(NA, nrow = bands, ncol = ik)
    xyval <- matrix(NA, nrow = bands, ncol = k)
    med3 <- spline(xy.coords(med), n = ik)$y
    
    for (j in 1:bands) {
      xyval[j,] <- xy.coords(val[j,])$y
      smoothed_val[j,] <- spline(xy.coords(val[j,]), n = ik)$y
    }
    
    return(list(val = xyval, smoothed_val = smoothed_val, med = med3))
    
  })
  
  output$df <- renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header)
  }, options = list(filter = FALSE, searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE))
  
  output$plot <- renderPlot({
    netstats <- netstats()
    if (is.null(netstats)) 
      return(NULL)
  
    val <- netstats$val
    smoothed_val <- netstats$smoothed_val
    med <- netstats$med

    par(mar = c(2,2,2,2))

    grid()
    plot((val[3,] + val[4,])/2, type="p", pch=18, ylim = c(-10,2))
    
    fan(smoothed_val, data.type = "values", start = start(smoothed_val), 
        type = "interval",
        probs = c(0.70, 0.85, 0.975),
        fan.col = colorRampPalette(c("tomato", "gray90")), alpha = 0.5,
        frequency = smoothing)

    lines(ts(med, start = start(med), frequency = smoothing), col = "black")

  })

})