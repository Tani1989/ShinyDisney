#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that display summary and dataset in two different tabs.
ui <- shinyUI(fluidPage(
  
  # Application title
  headerPanel("View Dataset"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    
    selectInput("dataset", "Select a dataset:", 
                choices = c("Gross Income", "Characters", "Directors")),
    
    numericInput("obs", "Number of observations to view:", 10)
    
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    tabsetPanel(
      tabPanel("Data Information",verbatimTextOutput("summary")) ,
      tabPanel("view Dataset",tableOutput("view"))
    )
  )
 
  
  
  
  
))

#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# .
#    http://shiny.rstudio.com/
#

library(shiny)
setwd("W:/disneyShinyapp")
Gross_Income <- read.csv("disney_movies_total_gross.csv",header = TRUE,fileEncoding="UTF-8-BOM")
Characters <- read.csv("disney-characters.csv",header = TRUE,fileEncoding="UTF-8-BOM")
Directors <- read.csv("disney-director.csv",header = TRUE,fileEncoding="UTF-8-BOM")



# Define server logic required to display dataset and summary in two different tabs.
server <- shinyServer(function(input, output,session) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "Gross Income" = Gross_Income,
           "Characters" = Characters,
           "Directors" = Directors)
  })
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  
  
  # Generate a summary of the dataset
  
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
