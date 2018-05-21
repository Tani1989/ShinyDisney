#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
install.packages("shinyjs")
library(shinyjs)
library(shiny)

# Define UI for application that display the data table according to the movie selection.
ui <- fluidPage(
  
  # Application title
  headerPanel("Analysis on Disney Movies"),
  useShinyjs(),

    # Sidebar with controls to select a dataset and display data according to selection of movie.
  sidebarPanel(
    
    selectInput("dataset", "Select a dataset:",
                choices = c("Gross Income", "Characters", "Directors")),
    
   
    selectizeInput('columns','Select The Movie',""),
    
    selectInput("income","Select Income for genre",choices = c("Total Gross Income","Inflated Gross Income")),
    
    selectInput("income1","Select Income for wordcloud",choices = c("Total Gross Income","Inflated Gross Income"))
    
    
  ),
# Two different tabs for Viewing dataset and Visualization.
  mainPanel(
    tabsetPanel(
        tabPanel("View Dataset",tableOutput("view")),
      tabPanel("Visualization",plotOutput("plot")),
      tabPanel("WordCloud",plotOutput("cloud")),
      tabPanel("Top Genre",plotOutput("genre"))
      
      
    )
  )

  
)

library(shiny)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(NLP)
library(RColorBrewer)
library(dplyr)
setwd("W:/disneyShinyapp")
Gross_Income <- read.csv("disney_movies_total_gross.csv",header = TRUE,fileEncoding="UTF-8-BOM")
Characters <- read.csv("disney-characters.csv",header = TRUE,fileEncoding="UTF-8-BOM")
Directors <- read.csv("disney-director.csv",header = TRUE,fileEncoding="UTF-8-BOM")


genre <- Gross_Income[,c("release_date","total_gross")]
#topData <- Gross_Income[,c(1,5)]




  
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
  
  # Top 10 movies based on Gross Salary.
  

  
  output$cloud <- renderPlot({
    
    if(input$income1 == "Total Gross Income"){
    
    

    topDataGross <- as.numeric(gsub('[$,]','',Gross_Income$total_gross))
    
    
    Gross_Income$total <- topDataGross
    
    
    datacloud <-head(arrange(Gross_Income,desc(total)), n = 20)
    
    datacloud$Rank <- rank(datacloud$total) 
   

 wordcloud(words = datacloud$movie_title,freq = datacloud$Rank,min.freq=1,scale = c(1.5,0.2),
              max.words=200,random.order=FALSE,rot.per=0.5,colors=brewer.pal(8,"Dark2") ) 
    }
    else if (input$income1 == "Inflated Gross Income"){
      topDataGross <- as.numeric(gsub('[$,]','',Gross_Income$inflation_adjusted_gross))
      
      
      Gross_Income$total <- topDataGross
      
      
      datacloud <-head(arrange(Gross_Income,desc(total)), n = 20)
      
      datacloud$Rank <- rank(datacloud$total) 
      
      
      wordcloud(words = datacloud$movie_title,freq = datacloud$Rank,min.freq=1,scale = c(1.5,0.2),
                max.words=200,random.order=FALSE,rot.per=0.5,colors=brewer.pal(8,"Dark2") ) 
      
    }
    
  })
  
  
  
# Top movies genre according to total gross.


  output$genre <- renderPlot({
    
 if(input$income == "Total Gross Income"){
   topDataGross <- as.numeric(gsub('[$,]','',Gross_Income$total_gross))
   
  
Gross_Income$total_gross <- topDataGross
  
  subsetData <- subset(Gross_Income,Gross_Income$genre != "")

  
  
  group_genre <- aggregate(total_gross ~ genre,data = subsetData,FUN = sum)
  group_genre
  
  p <- ggplot(group_genre,aes(genre,total_gross)) 
  p +geom_bar(stat = "identity", aes(fill = genre)) + coord_flip() + theme_minimal()
 }
    else if(input$income == "Inflated Gross Income"){
      topDataGross1 <- as.numeric(gsub('[$,]','',Gross_Income$inflation_adjusted_gross))
      Gross_Income$inflation_adjusted_gross <- topDataGross1
      
      subsetData <- subset(Gross_Income,Gross_Income$genre != "")
      
      group_genre <- aggregate(inflation_adjusted_gross ~ genre,data = subsetData,FUN = sum)
      group_genre
      
      p <- ggplot(group_genre,aes(genre,inflation_adjusted_gross)) 
      p +geom_bar(stat = "identity", aes(fill = genre)) + coord_flip() + theme_minimal()
      
      
    }
    
      
      
      
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

