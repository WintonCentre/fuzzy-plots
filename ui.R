library(shiny)
library(shinyjs)

# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Data series with uncertainty"),
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    inputPanel(
      shinyjs::useShinyjs(),
      
      checkboxInput("use_sample_data", "Use sample data", TRUE),
      
      fileInput("file1", "Choose CSV file to upload. First row must contain headers",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      #checkboxInput("header", "File contains headers in first row", TRUE),
      textInput("mainTitle", "Plot title:", "Main Title"),
      checkboxInput("expand", "Show uncertainty", FALSE)
      
    ),
    
    inputPanel(
    #   selectInput("x", "Independent variable:", c(), "x"),
       textInput("xLabel", "X Axis label:")
    ),
    inputPanel(
      textInput("Label", "Y Axis label:"),
      selectInput("", "Dependent variable:", c(), "")
    ),
    inputPanel(
      selectInput("sd", "Uncertainty:", c(), "sd"),
      numericInput("sd_unit", "Uncertainty unit (in sd)", 1, min = 0)
      
    )

  ),
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(id = "tabs",
      tabPanel("Data", 
               dataTableOutput("df")),
      tabPanel("Plot", 
               plotOutput("plot", height = "600px"))
      # tabPanel("output", verbatimTextOutput("value")),
    )
  )
))

