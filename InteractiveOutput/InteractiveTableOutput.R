#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  headerPanel("View Records"),
  
  # Sidebar with controls to select a dataset and select the movie.
  sidebarPanel(
    
    selectInput("dataset", "Select a dataset:", 
                choices = c("Gross Income", "Characters", "Directors")),
    

    selectInput('columns', 'Select The Movie', "")
    
  ),

  #Display the records according to the user selection.
  mainPanel(
    tabsetPanel(
      #tabPanel("Data Information",verbatimTextOutput("summary")) ,
      tabPanel("view Dataset",tableOutput("view"))
      
      
    )
  )

)

setwd("W:/disneyShinyapp")
Gross_Income <- read.csv("disney_movies_total_gross.csv",header = TRUE,fileEncoding="UTF-8-BOM")
Characters <- read.csv("disney-characters.csv",header = TRUE,fileEncoding="UTF-8-BOM")
Directors <- read.csv("disney-director.csv",header = TRUE,fileEncoding="UTF-8-BOM")


# Define server logic required to display the output table according to the user selection.
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
    x <- subset(outVar(),outVar()$movie_title == input$columns)
    return(x)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

