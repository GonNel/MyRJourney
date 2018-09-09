c<-head(airquality)
na.omit(c)
c
chicago<-readRDS("chicago.RDS")
head(chicago)
library(tidyverse)
library(lubridate)
library(reshape2)
library(GGally)
library(modelr)
library(hexbin)
chicago<-as.tibble(chicago)
chicago1<-chicago %>% 
  mutate(city=as.factor(city)) %>% 
  rename(dewpoint=dptp,PM25=pm25tmean2,PM10=pm10tmean2) %>%
  filter(PM25>30) %>%
  mutate(PM25trend=PM25-mean(PM25)) %>% 
  separate("date",c("year","month","day")) %>% 
  na.omit() %>% 
  group_by(year)
#....................................................
#Who's unique

carbon_cold %>% 
  select(Plant,Treatment) %>% 
  unique()

#ok looks cool
  carbon_cold <- as.tibble(carbon_cold)
    carbon_cold %>%
  group_by(Plant) %>%
  filter(Plant == "Qn1" | Plant == "Mc3", conc < 400) %>%
  ggplot(aes(conc, uptake,col=Plant)) + geom_point() +
  labs(title = "Variation of Uptake with concentration") +
  theme(plot.title = element_text(hjust = 0.5, colour = "blue4"),
        panel.grid = element_blank(),panel.background = element_blank(),
        panel.border = element_rect(size=0.2,fill = NA)
        )+scale_color_manual(values = c("red","blue"))
#Not the best graph but we tried
#Let's do some Maths
sample1<-carbon_cold %>%
  group_by(Plant) %>%
  filter(Plant == "Qn1" | Plant == "Mc3", conc < 400)
#Let's try to create a model and see what happens
   modl_sample1<-lm(uptake~conc+Treatment,data=sample1)
summary(modl_sample1)
#Create a grid
   grid4<-sample1 %>% 
          data_grid(conc=seq_range(conc,380),Treatment=Treatment) %>% 
          add_predictions(modl_sample1,"uptake")
#Let's Plot
   ggplot(sample1,aes(conc,uptake))+geom_point()+geom_line(data=grid4,colour="red")
predict(modl_sample1,data.frame(conc=280,Treatment="chilled"))
predict(modl_sample1,data.frame(conc=280,Treatment="nonchilled"))
##Residuals
  ggplot(sample1,aes(modl_sample1$residuals))+
    geom_histogram(binwidth=0.9,colour="black",fill="red")
  #ANOVa
  anova(modl_sample1)
#Not the best model.................Until next time
   modl_sample1_1<-lm(uptake~conc+Treatment+Type,data=sample1)
  summary(modl_sample1_1)   
  grid5<-sample1 %>% 
    data_grid(Type,.model=modl_sample1_1) %>% 
    add_predictions(modl_sample1_1)
grid5
#plot
 ggplot(grid5,aes(Type,pred))+geom_point()
 