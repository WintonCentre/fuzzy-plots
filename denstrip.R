#
# Desnstrip source
#
function (x, dens, at, width, horiz = TRUE, colmax, colmin = "white", 
          scale = 1, gamma = 1, ticks = NULL, tlen = 1.5, twd, tcol, 
          mticks = NULL, mlen = 1.5, mwd, mcol, lattice = FALSE, ...) 
{
  if (!is.numeric(x)) 
    stop("'x' must be numeric")
  if (missing(dens)) {
    de <- density(x, ...)
    x <- de$x
    dens <- de$y
  }
  else {
    if (!is.numeric(dens)) 
      stop("'dens' must be numeric")
    if (length(dens) != length(x)) 
      stop("Lengths of 'dens' and 'x' must be the same")
    dens <- dens[order(x)]
    x <- sort(x)
  }
  if (lattice) {
    rect.fn <- panel.rect
    seg.fn <- panel.segments
    default.width <- diff(current.panel.limits()[[if (horiz) 
      "ylim"
      else "xlim"]])/30
    default.colmax <- trellis.par.get("add.line")$col
    default.twd <- trellis.par.get("add.line")$lwd
    default.mwd <- trellis.par.get("add.line")$lwd * 2
  }
  else {
    rect.fn <- rect
    seg.fn <- segments
    default.width <- diff(par("usr")[if (horiz) 
      3:4
      else 1:2])/30
    default.colmax <- par("fg")
    default.twd <- par("lwd")
    default.mwd <- par("lwd") * 2
  }
  if (missing(width)) 
    width <- default.width
  if (missing(colmax)) 
    colmax <- default.colmax
  if (missing(twd)) 
    twd <- default.twd
  if (missing(mwd)) 
    mwd <- default.mwd
  if (missing(tcol)) 
    tcol <- colmax
  if (missing(mcol)) 
    mcol <- colmax
  dens <- dens/max(dens) * scale
  n <- length(x)
  rgbmax <- col2rgb(colmax, alpha = TRUE)
  rgbmin <- if (colmin == "transparent") 
    c(col2rgb(colmax, alpha = FALSE), 0)
  else col2rgb(colmin, alpha = TRUE)
  if (gamma <= 0) 
    stop("gamma must be greater than 0")
  p <- dens[1:(n - 1)]^gamma
  if (colmin == "transparent") 
    cols <- rgb(p * rgbmax[1] + (1 - p) * rgbmin[1], 
                p * rgbmax[2] + (1 - p) * rgbmin[2], 
                p * rgbmax[3] + (1 - p) * rgbmin[3], 
                alpha = p * rgbmax[4] + (1 - p) * rgbmin[4], 
                maxColorValue = 255)
  else cols <- rgb(p * rgbmax[1] + (1 - p) * rgbmin[1], p * 
                     rgbmax[2] + (1 - p) * rgbmin[2], p * rgbmax[3] + (1 - 
                                                                         p) * rgbmin[3], alpha = rgbmax[4], maxColorValue = 255)
  first.col <- c(TRUE, cols[2:(n - 1)] != cols[1:(n - 2)])
  next.col <- c(first.col, TRUE)
  next.col[1] <- FALSE
  if (horiz) {
    xleft <- x[-n][first.col]
    xright = x[next.col]
    ybottom <- at - width/2
    ytop <- at + width/2
  }
  else {
    xleft <- at - width/2
    xright <- at + width/2
    ybottom <- x[-n][first.col]
    ytop <- x[next.col]
  }
  rect.fn(xleft = xleft, ybottom = ybottom, xright = xright, 
          ytop = ytop, border = NA, col = cols[first.col])
  if (!is.null(ticks)) {
    if (horiz) {
      tx0 <- tx1 <- ticks
      ty0 <- at - width * tlen/2
      ty1 <- at + width * tlen/2
    }
    else {
      tx0 <- at - width * tlen/2
      tx1 <- at + width * tlen/2
      ty0 <- ty1 <- ticks
    }
    seg.fn(tx0, ty0, tx1, ty1, lwd = twd, col = tcol)
  }
  if (!is.null(mticks)) {
    if (horiz) {
      tmx0 <- tmx1 <- mticks
      tmy0 <- at - width * mlen/2
      tmy1 <- at + width * mlen/2
    }
    else {
      tmx0 <- at - width * mlen/2
      tmx1 <- at + width * mlen/2
      tmy0 <- tmy1 <- mticks
    }
    seg.fn(tmx0, tmy0, tmx1, tmy1, lwd = mwd, col = mcol)
  }
  invisible()
}