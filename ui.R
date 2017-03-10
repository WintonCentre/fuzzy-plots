library(shiny)

# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Data series with uncertainty"),
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    inputPanel(
      fileInput("file1", "Choose CSV file to upload",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      
      checkboxInput("header", "File contains headers in first row", TRUE)
    ),
    inputPanel(
      selectInput("x", "Column containing time:", c("x","mode","sd"), "x"),
      selectInput("mode", "Column containing values:", c("x","mode","sd"), "mode"),
      selectInput("sd", "Column containing standard deviation:", c("x","mode","sd"), "sd")
    )

  ),
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(id = "tabs",
      tabPanel("Data", 
               dataTableOutput("df")),
      tabPanel("Plot", 
               plotOutput("plot"))
      # tabPanel("output", verbatimTextOutput("value")),
    )
  )
))

