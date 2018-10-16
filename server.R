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
  output$predictme<-renderPlot({
    library(tidyverse)
    library(modelr)
    
    
    mymodel<-lm(Petal.Length~Sepal.Length,data=iris)
    myirisgrid<-expand.grid(Sepal.Length=c(0,7.5))
    mypredictions<-predict(mymodel,myirisgrid)
    
    grid<-iris %>% 
      data_grid(Sepal.Length) %>% 
      mutate(pred=predict(mymodel,newdata=.))
    iris %>% 
      ggplot(aes(Sepal.Length,Petal.Length,col=Species))+geom_point()+
      geom_smooth(method="lm",se=F)+
      ggtitle("Predicted Petal Length based on Sepal Length")+
      labs(caption="Predictions based on a linear model")+
      theme(plot.title = element_text(hjust = 0.5,size=19))+
      scale_color_manual(values=c("firebrick4","orange","steelblue4"))
  })
  output$corrplot<-renderPlot({
    library(tidyverse)
    library(modelr)
    library(GGally)
    
    
    mymodel<-lm(Petal.Length~Sepal.Length,data=iris)
    myirisgrid<-expand.grid(Sepal.Length=c(0,7.5))
    mypredictions<-predict(mymodel,myirisgrid)
    
    grid<-iris %>% 
      data_grid(Sepal.Length) %>% 
      mutate(pred=predict(mymodel,newdata=.))
    iris %>% 
      ggpairs(columns = c("Petal.Length","Sepal.Length"),aes(color=Species))
  })
})
