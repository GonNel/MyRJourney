#In this script, I'll look at risk factors for cervical cancer.
#The data is available at the https://archive.ics.uci.edu/ml/machine-learning-databases/00383/?C=D;O=A
library(tidyverse)
library(reshape2)
library(caret)
library(party)

#The above site was down. The dataset was locally accessed
cervical<-read.csv("risk_factors_cervical_cancer.csv")
str(cervical)

head(cervical)
cervical<-as.tibble(cervical)
cervical1<-cervical %>% 
 select(Age:Smokes,contains("smoke"),contains("diagnosis"),contains("Dx")) %>% 
  mutate(Agegroup=as.factor(findInterval(Age,c(0,18,35,100)))) 
 #change levels of age group to a more user friendly version.
cervical1 %>% 
  mutate(Agegroup=as.factor(Agegroup))
levels(cervical1$Agegroup)<-c("Young","Mid Aged","Eldery")
levels(cervical1$Smokes)<-c("Undecided","No","Yes")
#Look at levels of change ? in pregnancy to NA. Omit these figures
my_replace<-function(x){
  str_replace_all(x,"\\?","NA")
}
#rename long name variables
#SinceLastDiagn and SinceFirst Diagn mean time since first STD diagnosis.
cervical1<-cervical1 %>% 
  rename(SexPartners=Number.of.sexual.partners,InitialSexAge=First.sexual.intercourse,
         Pregnancies=Num.of.pregnancies,Packsperyear=Smokes..packs.year.,
         YearsSmoker=Smokes..years.,SinceFirstDiagn=STDs..Time.since.first.diagnosis,
         SinceLastDiagn=STDs..Time.since.last.diagnosis,NumberSTDS=STDs..Number.of.diagnosis,
         CancerDiagn=Dx.Cancer,HPVDiagn=Dx.HPV,Diagn=Dx) %>% 
       mutate(Pregnancies=my_replace(Pregnancies),InitialSexAge=my_replace(InitialSexAge),
              Diagn=as.factor(Diagn),HPVDiagn=as.factor(HPVDiagn),
              YearsSmoker=as.integer(YearsSmoker),Packsperyear=as.integer(Packsperyear),
              CancerDiagn=as.factor(CancerDiagn),SinceLastDiagn=as.integer(SinceLastDiagn),
              SinceFirstDiagn=as.integer(SinceFirstDiagn),Dx.CIN=as.factor(Dx.CIN))
#Give new levels to Diagnoses
levels(cervical1$Diagn)<-c("No","Yes")
levels(cervical1$HPVDiagn)<-c("No","Yes")
levels(cervical1$CancerDiagn)<-c("No","Yes")
#Taking a look at risk of Cervical cancer in patients with 1 or more STDS.
#First remove other risk factors like stds,cin,other cancers
cervical1 %>% 
  filter(Diagn=="Yes") %>% 
  select(-SinceLastDiagn,-SinceFirstDiagn,-Dx.CIN) %>% 
  ggplot(aes(Smokes,fill=HPVDiagn))+geom_histogram(stat="count")+facet_grid(Agegroup~HPVDiagn)+
  ggtitle("Smoking and HPV risk")+
  theme(plot.title = element_text(hjust=0.5))
#Look at risk of STDs,Other cancers,number of preganancies, number of sexual partners
#Machine Learning
training<-createDataPartition(cervical1$Smokes,p=0.8,list=F)
validator<-cervical1[-training,]
training<-cervical1[training,]
#Use cross validation
control<-trainControl(method="cv",number=10)
metric<-"Accuracy"
#Set up training models
set.seed(5)
fit.cart<-train(Smokes~.,data=training,method="rpart",trControl=control,metric=metric)
set.seed(5)
fit.kNN<-train(Smokes~.,data=training,method="knn",trControl=control,metric=metric)
set.seed(5)
fit.svm<-train(Smokes~.,data=training,method="svmRadial",trControl=control,metric=metric)
set.seed(5)
fit.rf<-train(Smokes~.,data=training,method="rf",trControl=control,metric=metric)
#Check for the best model
results_fit<-resamples(list(cart=fit.cart,svm=fit.svm,rf=fit.rf,knn=fit.kNN))
dotplot(results_fit)
#The results show that the random forest is the best model for prediction.
#Apply our model
prediction<-predict(fit.rf,validator)
confusionMatrix(prediction,validator$Smokes)


