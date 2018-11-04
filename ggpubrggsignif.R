library(tidyverse)
library(ggsignif)
library(ggpubr)
library(ggthemes) 
iris %>% 
  ggplot(aes(Species,Sepal.Length,fill=Species))+
  geom_violin(position = position_identity())+
  geom_boxplot(width=.2,position = position_identity(),fill="snow")+
  stat_compare_means(comparisons = list(c("virginica","versicolor","setosa")),  
                     label="p.signif")+
  stat_compare_means(label.x = 1.5)+
  scale_fill_manual(values = c("orange3","dodgerblue","indianred4"))+
  theme_pubr()+
  ggtitle("Iris strikes back")+labs(caption="Nelson")+
  theme(plot.title = element_text(hjust=0.5),
        plot.caption = element_text(size=14,colour="red"))
 

