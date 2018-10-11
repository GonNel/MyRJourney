library(shiny)
# Define server logic 
shinyServer(function(input, output) {
   
  output$irisplot <- renderPlot({
    shh<-suppressMessages
    shh(library(tidyverse))
    shh(library(lubridate))
    #Some shiny data
    var1<-as.numeric(input$Variable)
    ggplot(iris,aes(iris[,var1]))+geom_histogram(aes(stat="count"),
                                                 fill=input$radio,alpha=input$alpha)+
      scale_fill_manual(values=input$radio)
  
    
    
  })
  
})
