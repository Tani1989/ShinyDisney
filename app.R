#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application for viewing the datasets.
ui <- fluidPage(
    
    # Application title
    headerPanel("View Dataset"),
    
    # Sidebar with controls to select a dataset and specify the number
    # of observations to view
    sidebarPanel(
      textInput("caption", "Caption:", "Data Summary"),
      
      selectInput("dataset", "Select a dataset:", 
                  choices = c("Gross Income", "Characters", "Directors")),
      
      numericInput("obs", "Number of observations to view:", 10)
    ),
    
    # Show a summary of the dataset and a table with the requested
    # number of observations
    mainPanel(
     
      h3(textOutput("caption")), 
    
      verbatimTextOutput("summary"),
    
      tableOutput("view")
    
    
    
    
  ))

# Set the working directory and load the required files.
setwd("W:/disneyShinyapp")
Gross_Income <- read.csv("disney_movies_total_gross.csv",header = TRUE)
Characters <- read.csv("disney-characters.csv",header = TRUE)
Directors <- read.csv("disney-director.csv",header = TRUE)

# Define server logic required to view various datasets.
server <- function(input, output) {
   
 
    # Return the requested dataset
    datasetInput <- reactive({
      switch(input$dataset,
             "Gross Income" = Gross_Income,
             "Characters" = Characters,
             "Directors" = Directors)
    })
    output$caption <- renderText({
      input$caption
    })
    
    
    # Generate a summary of the dataset
    output$summary <- renderPrint({
      dataset <- datasetInput()
      summary(dataset)
    })
    
    # Show the first "n" observations
    output$view <- renderTable({
      head(datasetInput(), n = input$obs)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

