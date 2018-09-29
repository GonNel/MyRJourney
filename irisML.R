#This scripts applies basic machine learning on the iris dataset
#The visualizations are mostly experimental
shh<-suppressMessages
shh(library(tidyverse))
shh(library(caret))
shh(library(GGally))
shh(library(extrafont))
loadfonts(dev="win")
iris1<-as.tibble(iris)
iris1 %>% 
  map(~anyNA(.x))
#Visualise 
iris1 %>% 
  ggplot(aes(Sepal.Length,Petal.Width,col=Species))+geom_point()+
  scale_color_manual(values=c("green2","red","navy"))+
 labs(x="Sepal length",y="Petal Width")+
  theme(axis.title.x = element_text(colour="gray5",angle=35,
    size=13,family="Arial"),
    plot.title = element_text(hjust=0.5,colour="gray5",
    size=15,family="Arial"),axis.title.y = element_text(colour="gray5",angle=60,
                size=13,family="Arial"),
    panel.background = element_rect(fill="snow"))+
  scale_y_continuous(position = "right")+
  ggtitle("Variation of Petal Width  with Sepal Length")
#There appears to be a corelation between sepal length and Petal Width 
iris1 %>% 
  ggpairs(columns=c(1,4),aes(col=Species))
#The correlation is highest for the versicolor data although it is still a relatively weak correlation
#Apply some machine learning to predict lengths and widths based on Species
trainme<-createDataPartition(iris1$Species,p=0.8,list=F)
validator<-iris1[-trainme,]
trainme<-iris1[trainme,]
#Set control and metric
control<-trainControl(method="cv",number=10)
metric<-"Accuracy"
#Set models
set.seed(233456)
fit.cart<-train(Species~.,data=trainme,method="rpart",trControl=control,metric=metric)
fit.svm<-train(Species~.,data=trainme,method="svmRadial",trControl=control,metric=metric)
fit.nb<-train(Species~.,data=trainme,method="nb",trControl=control,metric=metric)
fit.rf<-train(Species~.,data=trainme,method="rf",trControl=control,metric=metric)
fit.knn<-train(Species~.,data=trainme,method="knn",trControl=control,metric=metric)
#Results
result<-resamples(list(knn=fit.knn,cart=fit.cart,nb=fit.nb,svm=fit.svm,rf=fit.rf))
dotplot(results)
#Apply on validation dataset
predicted<-predict(fit.knn,validator)
confusionMatrix(predicted,validator$Species)
#Not the best model and it can certainly be improved.
#Plant Scientists everywhere can partially rejoice as we can apply our model.
