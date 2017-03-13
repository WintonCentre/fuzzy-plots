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
      
      checkboxInput("header", "File contains headers in first row", TRUE),
      textInput("mainTitle", "Plot title:", "Main Title"),
      checkboxInput("expand", "Show uncertainty", FALSE)
      
    ),
    
    inputPanel(
    #   selectInput("x", "Independent variable:", c(), "x"),
       textInput("xLabel", "X Axis label")
    ),
    inputPanel(
      textInput("modeLabel", "Y Axis label"),
      selectInput("mode", "Dependent variable:", c(), "mode")
    ),
    inputPanel(
      selectInput("sd", "Standard deviation:", c(), "sd")
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

