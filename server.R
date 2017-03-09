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
    val2 <- matrix(NA, nrow = bands, ncol = ik)
    val3 <- matrix(NA, nrow = bands, ncol = ik)
    med3 <- spline(xy.coords(med), n = ik)$y
    
    for (j in 1:bands) {
      val3[j,] <- approx(xy.coords(val[j,]), n = ik)$y
      val2[j,] <- spline(xy.coords(val[j,]), n = ik)$y
    }
    
    return(list(val = val3, ival = val2, med = med3))
    
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
    ival <- netstats$ival
    med <- netstats$med
    print(netstats)
    
    par(mar = c(2,2,2,2))

    grid()
    plot(ts((val[3,] + val[4,])/2, frequency = smoothing), ylim = c(-10,2))
    
    fan(ival, data.type = "values", start = start(ival), type = "interval",
        probs = c(0.70, 0.85, 0.975),
        fan.col = colorRampPalette(c("tomato", "gray90")), alpha = 0.5,
        frequency = smoothing)

    lines(ts(med, start = start(med), frequency = smoothing), col = "black")

  })

})