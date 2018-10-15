
# Fuzzy plots

A shiny app which allows you to upload and plot a csv file containing a single data series with uncertainties.

See series.csv for a sample file.

## Revision notes, 15-10-2018

These are the low level changes but they should be tied into UI variabes rather than edited directly.

### Smoothing
server.R#488
smoothing <- 10 means 10 splines between each pair of data points, so:
smoothing <- 1

### Lines, but no points
server.R#523
Change type parameter of 'plot' from 'p' to 'l'
type = 'l'
