#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that display the data table according to the movie selection.
ui <- fluidPage(
  # Application title
  headerPanel("View Dataset"),
  
  # Sidebar with controls to select a dataset and display data according to selection of movie.
  sidebarPanel(
    
    selectInput("dataset", "Select a dataset:", 
                choices = c("Gross Income", "Characters", "Directors")),
    
   
    selectizeInput('columns','Select The Movie',"")
    
  ),
# Two different tabs for Viewing dataset and Visualization.
  mainPanel(
    tabsetPanel(
      tabPanel("View Dataset",tableOutput("view")),
      tabPanel("Visualization",plotOutput("plot"))
      
      
    )
  )

  
)

library(shiny)
library(ggplot2)
setwd("W:/disneyShinyapp")
Gross_Income <- read.csv("disney_movies_total_gross.csv",header = TRUE,fileEncoding="UTF-8-BOM")
Characters <- read.csv("disney-characters.csv",header = TRUE,fileEncoding="UTF-8-BOM")
Directors <- read.csv("disney-director.csv",header = TRUE,fileEncoding="UTF-8-BOM")
  
# Define server logic required to view th data according to the movie selection and the visualization.
server <- function(input, output,session) {
   
  outVar <- reactive({
    
    switch(input$dataset,
           "Gross Income" = Gross_Income,
           "Characters" = Characters,
           "Directors" = Directors)
  
  })
  
  
  observe({
    updateSelectInput(session,"columns",choices = outVar()$movie_title)
  })
  
  
  output$view <- renderTable({
    test <- subset(outVar(),outVar()$movie_title == input$columns)
    return(test)
  })
  
  
  
  output$plot <- renderPlot({
   
    dd <- as.numeric(gsub('[$,]','',outVar()$total_gross))
    ss <- as.numeric(gsub('[$,]','',outVar()$inflation_adjusted_gross))
    options(scipen=1000000)
    ggplot(data = outVar(),aes_string(x = dd,y = ss)) +
      geom_point() + xlim(50000000,530000000) + ylim(50000000,530000000) +
      xlab(NULL) + ylab(NULL)
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

