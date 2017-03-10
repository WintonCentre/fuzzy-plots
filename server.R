library(shiny)
library(fanplot)



expand <- function(df, percentiles) {
  k <- nrow(df)
  bands <- length(percentiles)
  
  val <- matrix(NA, nrow = bands, ncol = k)
  med <- rep(NA, k)
  for (i in 1:k) {
    val[, i] <- qsplitnorm(p = percentiles, 
                           mode = df$mode[i],
                           sd = df$sd[i],
                           skew = df$skew[i])
    med[i] <- qsplitnorm(p = 0.5,
                         mode = df$mode[i],
                         sd = df$sd[i],
                         skew = df$skew[i])
  }
  
}

smooth_and_expand <- function(df, smoothing, percentiles) {
  
  k <- nrow(df)
  smoothed_k <- smoothing*(k - 1) + 1
    
  bands <- length(percentiles)
  
  val <- matrix(NA, nrow = bands, ncol = k)
  med <- rep(NA, k)
  for (i in 1:k) {
    val[, i] <- qsplitnorm(p = percentiles, 
                           mode = df$mode[i],
                           sd = df$sd[i],
                           skew = df$skew[i])
    med[i] <- qsplitnorm(p = 0.5,
                         mode = df$mode[i],
                         sd = df$sd[i],
                         skew = df$skew[i])
  }
  
  smoothed_val <- matrix(NA, nrow = bands, ncol = smoothed_k)
  xyval <- matrix(NA, nrow = bands, ncol = k)
  med3 <- spline(xy.coords(med), n = smoothed_k)$y
  
  for (j in 1:bands) {
    xyval[j,] <- xy.coords(val[j,])$y
    smoothed_val[j,] <- spline(xy.coords(val[j,]), n = smoothed_k)$y
  }
  
  return(list(original_val = xyval, smoothed_val = smoothed_val, med = med3))
}

shinyServer(function(input, output) {
  

  output$df <- renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header)
  }, options = list(filter = FALSE, searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE))
  
  output$plot <- renderPlot({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    df1 <- read.csv(inFile$datapath, header = input$header)
    
    # smooth data frame values and parameters
    # expand uncertainties
    smoothing <- 2
    smoothed_expanded <- smooth_and_expand(df = df1, smoothing = smoothing,
                                  percentiles = c(0.025, 0.15, 0.30, 0.70, 0.85, 0.975))
    
    original_val <- smoothed_expanded$original_val
    smoothed_val <- smoothed_expanded$smoothed_val
    med <- smoothed_expanded$med

    par(mar = c(2,2,2,2))

    grid()
    plot((original_val[3,] + original_val[4,])/2, type = "p", pch = 18, ylim = c(-10,2))
    
    fan(smoothed_val, data.type = "values", start = start(smoothed_val), 
        type = "interval",
        probs = c(0.70, 0.85, 0.975),
        fan.col = colorRampPalette(c("tomato", "gray90")), alpha = 0.5,
        frequency = smoothing)

    lines(ts(med, start = start(med), frequency = smoothing), col = "black")

  })

})