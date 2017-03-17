library(shiny)
#library(fanplot)
library(shinyjs)

fan <- function (data = NULL, data.type = "simulations", style = "fan", 
          type = "percentile", probs = if (type == "percentile") seq(0.01, 
                                                                     0.99, 0.01) else c(0.5, 0.8, 0.95), start = 1, frequency = 1, 
          anchor = NULL, anchor.time = NULL, fan.col = heat.colors, 
          alpha = if (style == "spaghetti") 0.5 else 1, n.fan = NULL, 
          ln = if (length(probs) < 10) probs else probs[round(probs, 
                                                              2) %in% round(seq(0.1, 0.9, 0.1), 2)], ln.col = if (style == 
                                                                                                                  "spaghetti") "gray" else NULL, med.ln = if (type == "interval") TRUE else FALSE, 
          med.col = "orange", rlab = ln, rpos = 4, roffset = 0.1, rcex = 0.8, 
          rcol = NULL, llab = FALSE, lpos = 2, loffset = roffset, lcex = rcex, 
          lcol = rcol, upplab = "U", lowlab = "L", medlab = if (type == 
                                                                "interval") "M" else NULL, n.spag = 30, space = if (style == 
                                                                                                                    "boxplot") 1/frequency else 0.9/frequency, add = FALSE, 
          ylim = range(data) * 0.8, ...) 
{
  
  print("Hello there")
  
  if (add == TRUE) 
    plot(data[, 1], type = "n", ylim = ylim, ...)
  if (!(data.type %in% c("values", "simulations"))) 
    stop("data.type must be set to one of: values, simulations")
  if (!(style %in% c("fan", "boxfan", "spaghetti", "boxplot"))) 
    stop("style must be set to one of: fan, boxfan, spaghetti or boxplot")
  if (class(data)[1] == "mts") {
    start <- start(data)[1]
    frequency <- frequency(data)
  }
  if (style == "fan" | style == "boxfan" | style == "spaghetti") {
    if (!(type %in% c("percentile", "interval"))) 
      stop("type must be set to one of: percentile or interval")
    p <- probs
    if (min(p) < 0 | max(p) > 100) 
      stop("all probs must be between 0 and 1 (or 0 and 100)")
    if (max(p) > 1) 
      p <- p/100
    if (type == "percentile") 
      p <- c(p, 1 - p)
    if (type == "interval" & data.type == "simulations") 
      p <- c(p + (1 - p)/2, 1 - p - (1 - p)/2)
    p <- round(p, 5)
    p <- sort(unique(p))
    if (data.type == "simulations") {
      pp <- as.matrix(data)
      pp <- apply(pp, 2, quantile, probs = p)
    }
    if (data.type == "values") {
      pp <- as.matrix(data)
      if (type == "percentile" & length(p) != nrow(pp)) {
        print(paste("length(p)= ",length(p)))
        print(paste("nrow(pp)= ", nrow(pp)))
        
        print(p)
        print(pp)
        stop("probs must correspond to the nrows of data if data.type==values and type is percentile")
      }
      if (type == "interval" & length(probs) != 2 * nrow(pp)) {
        p <- probs
        p <- c(p + (1 - p)/2, 1 - p - (1 - p)/2)
        p <- sort(p)
        p <- round(p, 5)
      }
    }
    n <- nrow(pp)
    if (type == "interval") {
      rownames(pp) <- paste0(rep(c(lowlab, upplab), each = n/2), 
                             200 * abs(0.5 - p), "%")
    }
    if (!is.null(anchor)) {
      pp <- cbind(rep(anchor, n), pp)
    }
    pp <- t(pp)
    if (class(data) != "zoo") {
      ppts <- ts(pp, start = start, frequency = frequency)
      tt <- time(ppts)
      tt <- as.numeric(tt)
      if (!is.null(anchor)) {
        ppts <- ts(pp, start = time(lag(ppts))[1], frequency = frequency)
        tt <- time(ppts)
        tt <- as.numeric(tt)
      }
    }
    if (class(data) == "zoo") {
      tt <- time(data)
      if (!is.null(anchor)) 
        tt <- c(anchor.time, tt)
    }
  }
  if (style == "fan" | style == "boxfan") {
    if (is.null(n.fan)) 
      fan.col <- fan.col(floor(n/2))
    if (!is.null(n.fan)) 
      fan.col <- fan.col(n.fan)
    fan.col <- adjustcolor(fan.col, alpha.f = alpha)
    if (is.null(ln.col)) 
      ln.col <- fan.col[1]
  }
  if (style == "fan") {
    fan.fill <- function(ts1, ts2, tt, fan.col = "grey") {
      xx <- cbind(tt, rev(tt))
      yy <- cbind(as.vector(ts1), rev(as.vector(ts2)))
      polygon(xx, yy, col = fan.col, border = fan.col)
    }
    for (i in 1:floor(n/2)) {
      fan.fill(ts1 = pp[, i], ts2 = pp[, n - i + 1], tt = tt, 
               fan.col = fan.col[floor(n/2) + 1 - i])
    }
  }
  if (style == "boxfan") {
    x <- pp[, 1]
    for (i in 1:nrow(pp)) {
      for (j in 1:floor(n/2)) {
        rect(xleft = tt[i] - 0.5 * space, ybottom = pp[i, 
                                                       j], xright = tt[i] + 0.5 * space, ytop = pp[i, 
                                                                                                   n - j + 1], col = fan.col[floor(n/2) + 1 - 
                                                                                                                               j], border = fan.col[floor(n/2) + 1 - j])
      }
    }
  }
  if (style == "fan" | style == "boxfan") {
    ln0 <- ln
    if (!is.null(ln0)) {
      if (min(ln0) < 0 | max(ln0) > 100) 
        stop("all ln must be between 0 and 1 (or 0 and 100)")
      if (max(ln0) > 1) 
        ln0 <- ln0/100
      if (type == "interval") {
        ln0 <- c(ln0 + (1 - ln0)/2, 1 - ln0 - (1 - ln0)/2)
        ln0 <- sort(ln0)
      }
      ln0 <- round(ln0, 5)
      if (style == "fan") {
        for (i in match(ln0, p)) lines(x = tt, y = pp[, 
                                                      i], col = ln.col)
      }
      if (style == "boxfan") {
        for (i in 1:nrow(pp)) {
          for (j in match(ln0, p)) {
            lines(x = tt[i] + c(-0.5, 0.5) * space, y = rep(pp[i, 
                                                               j], 2), col = ln.col)
          }
        }
      }
      if (is.na(sum(match(ln0, p)))) 
        print("some lines not plotted as conflict with precentiles given in probs")
    }
  }
  if (style == "fan" | style == "boxfan") {
    if (data.type == "values" & type == "percentile") 
      colnames(pp) <- paste0(p * 100, "%")
    if (!is.null(rlab)) {
      if (min(rlab) < 0 | max(rlab) > 100) 
        stop("all rlab must be between 0 and 1 (or 0 and 100)")
      if (max(rlab) > 1) 
        rlab <- rlab/100
      if (type == "interval") 
        rlab <- c(rlab + (1 - rlab)/2, 1 - rlab - (1 - 
                                                     rlab)/2)
      rlab <- sort(rlab)
      rlab <- round(rlab, 5)
      for (i in match(rlab, p)) {
        if (style == "fan") 
          text(tt[length(tt)], pp[nrow(pp), i], colnames(pp)[i], 
               pos = rpos, offset = roffset, cex = rcex, 
               col = rcol)
        if (style == "boxfan") 
          text(tt[length(tt)] + 0.5 * space, pp[nrow(pp), 
                                                i], colnames(pp)[i], pos = rpos, offset = roffset, 
               cex = rcex, col = rcol)
      }
      if (is.na(sum(match(rlab, p)))) 
        print("some right labels not plotted as conflict with precentiles given in probs")
    }
    if (is.numeric(llab[1]) | llab[1] == TRUE) {
      if (is.numeric(llab[1])) {
        if (min(llab) < 0 | max(llab) > 100) 
          stop("all llab must be between 0 and 1 (or 0 and 100)")
        if (max(llab) > 1) 
          llab <- llab/100
        if (type == "interval") 
          llab <- c(llab + (1 - llab)/2, 1 - llab - (1 - 
                                                       llab)/2)
        llab <- sort(llab)
        llab <- round(llab, 5)
      }
      if (llab[1] == TRUE) 
        llab <- rlab
      for (i in match(llab, p)) {
        if (style == "fan") 
          text(tt[1], pp[1, i], colnames(pp)[i], pos = lpos, 
               offset = loffset, cex = lcex, col = lcol)
        if (style == "boxfan") 
          text(tt[1] - 0.5 * space, pp[1, i], colnames(pp)[i], 
               pos = lpos, offset = loffset, cex = lcex, 
               col = lcol)
      }
      if (is.na(sum(match(llab, p)))) 
        print("some left labels not plotted as conflict with precentiles given in probs")
    }
  }
  if (style == "fan" | style == "boxfan") {
    if (med.ln == TRUE & data.type == "simulations") {
      pp <- data
      pm <- apply(pp, 2, median)
      if (!is.null(anchor)) 
        pm <- c(anchor, pm)
      if (is.null(med.col)) 
        med.col <- ln.col
      if (style == "fan") {
        lines(x = tt, y = pm, col = med.col)
      }
      if (style == "boxfan") {
        for (i in 1:nrow(pp)) {
          lines(x = tt[i] + c(-0.5, 0.5) * space, y = rep(pm[i], 
                                                          2), col = med.col)
        }
      }
      if (!is.null(rlab) & style %in% c("fan", "spaghetti")) 
        text(tt[length(tt)], pm[length(pm)], medlab, 
             pos = rpos, offset = roffset, cex = rcex, col = rcol)
      if (!is.null(rlab) & style == "boxfan") 
        text(tt[length(tt)] + 0.5 * space, pm[length(pm)], 
             medlab, pos = rpos, offset = roffset, cex = rcex, 
             col = rcol)
      if (any(llab == TRUE, is.numeric(llab)) & style %in% 
          c("fan", "spaghetti")) 
        text(tt[1], pm[1], medlab, pos = lpos, offset = loffset, 
             cex = lcex, col = lcol)
      if (any(llab == TRUE, is.numeric(llab)) & style == 
          "boxfan") 
        text(tt[1] - 0.5 * space, pm[1], medlab, pos = lpos, 
             offset = loffset, cex = lcex, col = lcol)
    }
  }
  if (style == "spaghetti") {
    ps <- as.matrix(data)
    n <- nrow(ps)
    ps <- ps[sample(1:n, n.spag), ]
    if (!is.null(anchor)) 
      ps <- cbind(rep(anchor, nrow(ps)), ps)
    spag.col <- adjustcolor(ln.col, alpha.f = alpha)
    for (i in 1:nrow(ps)) lines(x = tt, y = ps[i, ], col = spag.col)
  }
  if (style == "boxplot") {
    if (data.type == "values") 
      stop(print("data must be simulations"))
    pp <- data
    n <- ncol(pp)
    if (!is.null(anchor)) 
      print("anchor ignored for boxplots plots")
    p <- ts(pp[1, ], start = start, frequency = frequency)
    for (i in 1:n) boxplot(pp[, i], add = TRUE, at = time(p)[i], 
                           boxwex = space, xaxt = "n", yaxt = "n", ...)
  }
  box()
}

fan0 <- function (data = NULL, data.type = "simulations", style = "fan", 
          type = "percentile", probs = if (type == "percentile") seq(0.01, 
                                                                     0.99, 0.01) else c(0.5, 0.8, 0.95), start = 1, frequency = 1, 
          anchor = NULL, anchor.time = NULL, fan.col = heat.colors, 
          alpha = if (style == "spaghetti") 0.5 else 1, n.fan = NULL, 
          ln = NULL, ln.col = if (style == "spaghetti") "gray" else NULL, 
          med.ln = if (type == "interval") TRUE else FALSE, med.col = "orange", 
          rlab = ln, rpos = 4, roffset = 0.1, rcex = 0.8, rcol = NULL, 
          llab = FALSE, lpos = 2, loffset = roffset, lcex = rcex, lcol = rcol, 
          upplab = "U", lowlab = "L", medlab = if (type == "interval") "M" else NULL, 
          n.spag = 30, space = if (style == "boxplot") 1/frequency else 0.9/frequency, 
          add = TRUE, ylim = range(data) * 0.8, ...) 
{
  if (add == TRUE) 
    plot(data[, 1], type = "n", ylim = ylim, ...)
  fan(data = data, data.type = data.type, style = style, type = type, 
      probs = probs, start = start, frequency = frequency, 
      anchor = anchor, anchor.time = anchor.time, fan.col = fan.col, 
      alpha = alpha, n.fan = n.fan, ln = ln, ln.col = ln.col, 
      med.ln = med.ln, med.col = med.col, rlab = rlab, rpos = rpos, 
      roffset = roffset, rcex = rcex, rcol = rcol, llab = llab, 
      lpos = lpos, loffset = loffset, lcex = lcex, lcol = lcol, 
      upplab = upplab, lowlab = lowlab, medlab = medlab, n.spag = n.spag, 
      space = space, add = FALSE)
}

# I forgot that it would be great if 
# the default plot was just the dots
# then clicking 'smooth' joined them up
# then clicking 'uncertainty bands' put confidence bands
# then clicking 'uncertainty blur' put the sausage on
# Cool!

# In a02samar2017.xls, 
#   check column E 2013 to present \pm 73,000
#   column I \pm 0.1

# 
# norm.density.palette <- function(sds = 2, colmax = "tomato", colmin = "white", gamma = 1, scale = 1) {
#   rgbmax <- col2rgb(colmax, alpha=TRUE)
#   rgbmin <- c(col2rgb(colmax, alpha = FALSE), 0)
#   if (gamma <= 0) 
#     stop("gamma must be greater than 0")
#   return(
#     function(n) {
#       if( n %% 2 == 0) 
#          n <- n + 1
#       print(paste("n=",n))
#       pal <- seq(n)
#       x <- seq(-2, 2, length=n)
#       dens <- dnorm(x)
#       dens <- dens/max(dens) * scale
#       print(dens)
#       
#       p <- dens[1:n]^gamma
#       cols <- rgb(p * rgbmax[1] + (1 - p) * rgbmin[1], 
#                   p * rgbmax[2] + (1 - p) * rgbmin[2], 
#                   p * rgbmax[3] + (1 - p) * rgbmin[3], 
#                   alpha = p * rgbmax[4] + (1 - p) * rgbmin[4], 
#                   maxColorValue = 255)
#       return(cols)
#     }
#   )
# }

norm.density.palette <- function(sds = 4, colmax = "tomato", colmin = "transparent", gamma = 1, scale = 1) {
  rgbmax <- col2rgb(colmax, alpha=TRUE)
  rgbmin <- c(col2rgb(colmin, alpha = FALSE), 0)
  if (gamma <= 0) 
    stop("gamma must be greater than 0")
  return(
    function(n) {

      pal <- seq(n)
      x <- seq(-sds, sds, length=n)
      dens <- dnorm(x)
      dens <- dens/max(dens) * scale
      
      # p <- dens[1:(n-1)]^gamma
      # cols <- rgb(p * rgbmax[1] + (1 - p) * rgbmin[1], 
      #             p * rgbmax[2] + (1 - p) * rgbmin[2], 
      #             p * rgbmax[3] + (1 - p) * rgbmin[3], 
      #             alpha = p * rgbmax[4] + (1 - p) * rgbmin[4], 
      #             maxColorValue = 255)
      # 
      # rgbmax <- col2rgb(colmax, alpha = TRUE)
      # rgbmin <- if (colmin == "transparent") 
      #   c(col2rgb(colmax, alpha = FALSE), 0)
      # else col2rgb(colmin, alpha = TRUE)
      
      if (gamma <= 0) 
        stop("gamma must be greater than 0")
      p <- dens[1:(n - 1)]^gamma
      if (colmin == "transparent") 
        cols <- rgb(p * rgbmax[1] + (1 - p) * rgbmin[1], 
                    p * rgbmax[2] + (1 - p) * rgbmin[2], 
                    p * rgbmax[3] + (1 - p) * rgbmin[3], 
                    alpha = p * rgbmax[4] + (1 - p) * rgbmin[4], 
                    maxColorValue = 255)
      else cols <- rgb(p * rgbmax[1] + (1 - p) * rgbmin[1], 
                       p * rgbmax[2] + (1 - p) * rgbmin[2], 
                       p * rgbmax[3] + (1 - p) * rgbmin[3], 
                       alpha = rgbmax[4], maxColorValue = 255)
      
      return(cols)
    }
  )
}


get_percentiles <- function(n = 10, sds = 4, scale = 1) {
  if (n %% 2 != 0) 
    n <- n + 1
  print(paste("percentile count = ", n))
  x <- seq(-sds, sds, length = n) #[1:(n/2)]
  dens <- dnorm(x)
  #dens <- c(dnorm(x), rev(1 - dnorm(x)))
  return(dens)
}



shinyServer(function(input, output, session) {
  
  
  expand_df <- function(df, percentiles) {
    k <- nrow(df)
    bands <- length(percentiles)
    
    sd_unit = 1;
    if (!is.na(input$sd_unit) & !is.null(input$sd_unit) & input$sd_unit > 0)
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
    if (!is.na(input$sd_unit) &!is.null(input$sd_unit) & input$sd_unit > 0)
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
    if (!is.na(input$sd_unit) & !is.null(input$sd_unit) & input$sd_unit > 0)
      sd_unit = input$sd_unit
    
    smoothed_k <- frequency*(k - 1) + 1
    return(data.frame(#x = spline(df$x, n=smoothed_k)$y,
      mode = spline(df$mode, n=smoothed_k)$y,
      sd = spline(df$sd * sd_unit, n=smoothed_k)$y))
  }
  
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
      print(previous_choice)
      if (previous_choice == "" || !(previous_choice %in% names(df1))) {
        updateSelectInput(session, internal_names[i],
                          choices = names(df1),
                          selected = internal_names[i])
      }
    }
    
    if (input$modeLabel == "" & input$mode != "") {
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
    
    if (input$modeLabel == "" & input$mode != "") {
      updateTextInput(session, "modeLabel", value = input$mode)
    }
    
    if (input$mode == "" || input$sd == "")
      return(NULL)
    
    
    # redefine df1 with internal names, "x", "mode", and "sd".
    if (input$mode != "" & input$sd != "") {
      #      x <- df1[[input$x]]
      mode <- df1[[input$mode]]
      sd <- df1[[input$sd]]
      df1 <- data.frame(mode = mode, sd = sd)
    }
    
    # smooth data frame values and parameters
    # expand uncertainties
    smoothing <- 10
    pps = get_percentiles(n=200)
    #seq(0.025,0.975,0.025)
    npps <- length(pps)
    print(paste("length pps =", length(pps)))
    mid <- floor(npps / 2)

    # pps =  psplitnorm(c(seq(-3,-2, 0.5), 
    #                     seq(-1.98,-1, 0.02), 
    #                     seq(-0.99,0.99,0.01), 
    #                     seq(1, 1.98, 0.02), 
    #                     seq(2, 3, 0.5)))

    smoothed_expanded <- smooth_and_expand(df = df1, smoothing = smoothing,
                                            percentiles = pps
                                             #c(0.025, 0.15, 0.30, 0.70, 0.85, 0.975)
                                             )
    
    sv <- smoothed_expanded$smoothed_val
    min_smoothed <- floor(min(sv))
    max_smoothed <- ceiling(max(sv))
    print(min_smoothed)
    print(max_smoothed)
    
    original_val <- smoothed_expanded$original_val
    smoothed_val <- smoothed_expanded$smoothed_val
    
    med <- smoothed_expanded$med
    par(mar = c(4,4,4,4))    
    grid()                                                                                                                                             
    plot((original_val[mid,] + original_val[mid+1,])/2,
         type = "p", pch = 18,
         xlim = c(1,ncol(original_val)),
         ylim = c(min_smoothed, max_smoothed),
         xlab = input$xLabel,
         ylab = input$modeLabel,
         main = input$mainTitle)                                                                                                                                                                                                                        
    axis(1, at=time(1:ncol(original_val)), labels = TRUE)                                                                                                                                                                        
    if (input$expand) {
      print(paste("nrows =", nrow(smoothed_val)))
      fan0(smoothed_val,
           data.type = "values",
           start = 1,
           frequency = smoothing,
           xlim = c(1,ncol(original_val)),
           ylim = c(min_smoothed, max_smoothed),
           type = "percentile",
           med.ln = TRUE,
           medlab = NULL,
           style = "fan",
           probs = pps,
           fan.col = norm.density.palette(), # colorRampPalette(c("tomato", "white")),
           xlab = input$xLabel,
           ylab = input$modeLabel,
           main = input$mainTitle
           )
    }
    
    if(!input$expand) {
      lines(ts(med, start = start(med), frequency = smoothing), col = "black")
    }
  })
  
})
