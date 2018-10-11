library(shiny)
# Define server logic 
shinyServer(function(input, output) {
  
  output$selected_var<-renderText({
    paste("You have chosen to plot a histogram of ", input$Variable)})
  
   
  output$irisplot <- renderPlot({
    
    shh<-suppressMessages
    shh(library(tidyverse))
    shh(library(lubridate))
   
    #Some shiny data
    var1<-as.numeric(input$Variable)
    
    ggplot(iris,aes(iris[,var1]))+geom_histogram(fill=input$radio,alpha=input$alpha,
                                                 binwidth = input$binwidth,col="black")+
      scale_fill_manual(values=input$radio)
  
})
})
