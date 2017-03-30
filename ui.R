library(shiny)
library(shinyjs)

# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Data series with uncertainty"),
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    inputPanel(
      textInput("mainTitle", "Plot title:", "Time Series")
    ),
    
    inputPanel(
      shinyjs::useShinyjs(),
      
            #checkboxInput("use_sample_data", "Use sample data", TRUE),
      radioButtons("use_sample_data", "Use sample data:", c("Sample1" = "sample1",
                                                                  "Sample2" = "sample2",
                                                                  "Upload CSV" = "upload")),
      
      fileInput("file1", "Choose CSV file to upload. First row must contain headers",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
      #checkboxInput("header", "File contains headers in first row", TRUE),
      #checkboxInput("expand", "Show uncertainty", FALSE)
      
    ),
    
    inputPanel(
      selectInput("t", "Optional time column:", c("t"), "x"),
      textInput("tLabel", "Time axis label:", "time")
    ),
    
    inputPanel(
      selectInput("mode", "Value column:", c("values"), "values"),
      textInput("modeLabel", "Value axis label:", "values"),
      numericInput("ymin", "Value axis minimum:", 0),
      numericInput("ymax", "Value axis maximum:", 1),
      checkboxInput("autorange", "Set value axis range automatically", TRUE)
    ),
    
    inputPanel(
      selectInput("sd", "Uncertainty column:", c(), "sd"),
      numericInput("sd_unit", "Uncertainty unit (in sd)", 1, min = 0)
      
    )

  ),
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(id = "tabs",
      tabPanel("Data", 
               dataTableOutput("df")),
      tabPanel("Plot", 
               plotOutput("plot", height = "600px")),
      tabPanel("Smoothed", 
               plotOutput("plot_smooth", height = "600px")),
      tabPanel("Uncertainty (Coarse)", 
               plotOutput("plot_uncertain_coarse", height = "600px")),
      tabPanel("Uncertainty (Detail)", 
               plotOutput("plot_uncertain_detail", height = "600px"))
      
      # tabPanel("output", verbatimTextOutput("value")),
    )
  )
))

