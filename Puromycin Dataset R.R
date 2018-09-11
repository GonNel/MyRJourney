shh<-suppressMessages
shh(library(tidyverse))
shh(library(ggvis))
shh(library(reshape2))
shh(library(ggrepel))
shh(library(lubridate))
shh(library(modelr))

Puro<-as.tibble(Puromycin)
summary(Puro) 
Puro %>% 
  ggvis(~conc,~rate) %>% 
  layer_points(fill=~state,size:=input_slider(0,100,value=2,label ="Size")) %>% 
  add_axis("x",title="Enzyme Concentration",properties = axis_props(labels=list(fontSize=15))) %>% 
  add_axis("x",title="Puromycin treatment increases the rate of enzyme reaction",orient="top",ticks=0,
properties=axis_props(ticks=NULL,axis=list(stroke="white"),labels=list(fontSize=25))) %>% 
  add_axis("y",title="Rate of reaction",properties = axis_props(labels = list(fontSize=12))) 
#Puromycin in most papers is shown to inhibit protein synthesis. It however appears that in this particular model puromycin enhances enzyme activity
#Applying a linear model is highly exploratory and I would ideally think such would not happen in nature as enzyme catalysed reactions don't typically follow linear
#A better model would be to model Michaelis Menten Kinetics
#We can model predictions with ggplot2
mmen1<-lm(rate~conc+state,data=Puro)
Purogrid<-Puro %>% 
          data_grid(conc,state) %>% 
          add_predictions(mmen1,"rate")
#The values for Vm and K have been largely got by trial and error and may not be a true representation of what happens in nature. The identity of this enzyme is unknown
     ggplot(Puro,aes(conc,rate,col=state))+geom_point()+
       geom_point(data=Purogrid,aes(size=5))+ggtitle("Predicted Enzyme Kinetics")+theme(plot.title = element_text(hjust=.5)) 
#The above is a linear model with ggplot2 that does not really reflect enzyme kinetics.
# A non-linear regression approach would go as follows
     mmen<-nls(rate~Vm*conc/(K+conc),start = list(Vm=3,K=1),data=Purogrid1)
  Purogrid1<-Puro %>% 
            data_grid(conc) %>% 
            add_predictions(mmen,"rate")
  ggplot(Purogrid1,aes(conc,rate))+geom_point()+geom_smooth()+ggtitle("Predicted Enzyme Kinetics")
#The plot again doesn't seem reasonable in terms of known enzyme kinetics and there is really no distinction between treated and non treated cells
  summary(mmen1)
#Taking a look at model 1 in terms of residuals
   Puro %>% 
     add_residuals(mmen1,"resid") %>% 
     ggplot(aes(conc,resid,col=state))+geom_point()+ggtitle("Residual distribution not centered around zero")
#Clearly our model fails when we look at our residual distribution and cannot be used as an accurate predictor of enzyme kinetics.   