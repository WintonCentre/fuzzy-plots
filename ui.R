library(shiny)

# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Data series with uncertainty"),
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    fileInput("file1", "Choose CSV file to upload",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    
    checkboxInput("header", "File contains headers in first row", TRUE)

  ),
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", 
               plotOutput("plot")),
      # tabPanel("output", verbatimTextOutput("value")),
      tabPanel("Data", 
               dataTableOutput("df"))
    )
  )
))

