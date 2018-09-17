#The data in this script was part of a series of lectures on introductory statistics with R
#I have used the data to practice my ggvis skills.
library(tidyverse)
library(ggvis)
Nobel<-read.csv("ChocolateAndNobel.csv")
Nobel<-as.tibble(Nobel)
Nobel
#Looking at variation of chocolate consumption and Nobel Prize winners
sum(is.na(Nobel))
#Visualise the data
Nobel %>% 
  ggvis(~chocolateConsumption,~nobelPrizes.per.100.million.) %>% 
  layer_points(fill=~country,size:=input_slider(0,100,5,label="Point size"),
               opacity:=input_slider(0,1,label="opacity"))%>% 
  add_axis("x",title="Nobel Prize variation with Chocolate consumption",
           orient="top",ticks = 0,
           properties = axis_props(ticks=NULL,
          axis=list(stroke="white"),labels = list(fontSize=30))) %>% 
  add_axis("x",title="Chocolate Consumption",orient="bottom",
           properties = axis_props(axis=list(stroke="gray"))) %>% 
     add_tooltip(function(Nobel) Nobel$country)
 
 #From the graph it appears that the more Chocolate consumers also have more Nobel Prize winners.
#Exception to this rule is Sweden,Finalnd,France,the U.S,the Netherlands and Germany. 
#The graph is not conclusive. We attempt to use statistics.
summary(Nobel)
t.test(Nobel$chocolateConsumption,Nobel$nobelPrizes.per.100.million.)



 
  
      
         