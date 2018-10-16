#

# This is the user-interface definition of a Shiny web application. You can

# run the application by clicking 'Run App' above.

#

# Find out more about building applications with Shiny here:

# 

#    http://shiny.rstudio.com/

#



library(shiny)



# Define UI for application that draws a histogram

shinyUI(fluidPage(
  
  
  
  # Application title
  
  titlePanel("Iris: A shiny exploration"),
  
  
  
  # Sidebar with a slider input for number of bins 
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("Species","Select species",
                  
                  choices = list("virginica"=3,"setosa"=1,"versicolor"=2),
                  
                  selected = 1),
      
      selectInput("Variable","Select variable",choices = list("Petal.Length"=1,
                                                              
                                                              "Sepal.Length"=2,
                                                              
                                                              "Petal.Width"=3,
                                                              
                                                              "Sepal.Width"=4),
                  
                  selected = 3)
      
      
      
      ,
      
      radioButtons("radio","Select Colour",choices = c("papayawhip","navy","snow",
                                                       
                                                       "red","green"),
                   
                   selected="red")
      
      ,
      
      sliderInput("alpha",label="Select opacity",min = 0,max=1,value = 0.65),
      
      sliderInput("binwidth",label="Select binwidth",min=0,max=3.5,value=0.25)),
    
    
    
    # Show a plot of the generated distribution
    
    mainPanel(
      textOutput("Thanks"),
     tabsetPanel(type="tabs",
                 tabPanel("Histogram",plotOutput("irisplot")),
      tabPanel("Scatter",plotOutput("irscatter")),
      tabPanel("Predictions",plotOutput("predictme")),
      tabPanel("Correlation Plot",plotOutput("corrplot"))
      )
     
    )
    
  )
  
))