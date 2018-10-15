library(shiny)
# Define server logic 
shinyServer(function(input, output) {
 
  output$selected_var<-renderText({
    paste("You have chosen to plot a histogram of Variable ", input$Variable)})
  
   output$Thanks<-renderText({
     "The data is based on the famous Iris dataset.
   Thank you for Viewing and please provide some feedback. 
It's highly appreciated!"
   })
  
  output$irisplot <- renderPlot({
    
    shh<-suppressMessages
    shh(library(tidyverse))
    shh(library(lubridate))
    shh(library(caret))
   
    #Some shiny data
    var1<-as.numeric(input$Variable)
   
    
    ggplot(iris,aes(iris[,var1]))+geom_histogram(fill=input$radio,alpha=input$alpha,
                                                 binwidth = input$binwidth,col="black")+
      scale_fill_manual(values=input$radio)+
      ggtitle(paste("Distribution of Variable",input$Variable))+
      theme(panel.background = element_rect(fill=NULL),
            plot.title =element_text(colour = "blue",hjust=0.5,size=25))+
      labs(x=input$Variable)
  })
  output$irscatter<-renderPlot({
    shh<-suppressMessages
    shh(library(tidyverse))
    shh(library(lubridate))
    shh(library(caret))
    
    #Some shiny data
    var1<-as.numeric(input$Variable)
    
    ggplot(iris,aes(Petal.Length,Sepal.Length))+
      geom_point(aes(col=Species))+
      ggtitle("Scatter Plot of Petal and Sepal Length")+
      theme(plot.title = element_text(hjust=0.5,size = 25,
            color="navy"))+
      scale_color_manual(values=c("steelblue4","springgreen4","tomato4"))
            
  })
})
