#Basic Machine Learning on kaggle titanic dataset
nomsg<-suppressMessages
nomsg(library(tidyverse))
nomsg(library(caret))
library(RANN)
train<-read.csv("train.csv")
train<-as.tibble(train)
#Remove cabin,name,Ticket for now as these may not predict survival 
train<-train %>% 
  mutate(PassengerId=as.factor(PassengerId),Pclass=as.factor(Pclass),
         Survived=as.factor(Survived),AgeGroup=as.factor(findInterval(Age,c(0,18,35,100)))) %>% 
 select(-PassengerId,-Name,-Ticket,-Cabin,-Embarked)
#View and deal with nas
anyNA(train$AgeGroup)
#Change levels
levels(train$AgeGroup)<-c("Young","Mid Age","Aged")
levels(train$Sex)<-c("F","M")
#Select only complete cases
train<-train[complete.cases(train),]
#Let's visualise survival by Age Group
train2 %>% 
   ggplot(aes(Survived,fill=Sex))+geom_histogram(stat="count")+facet_wrap(AgeGroup~Pclass)+
  ggtitle("Survival by class,Agegroup and Gender")+theme(plot.title=element_text(hjust=0.5)) 
#The graph does suggest that being of mid Age and embarking in the third class made you more likely to die
#Overall more women than men survived.
#Partition our data into a training and test dataset
#Missing values
anyNA(train)
#Create partition
train1<-createDataPartition(train$Survived,p=0.8,list=F)
validate<-train[-train1,]
train1<-train[train1,]
#Set metric and control
control<-trainControl(method="cv",number = 10)
metric<-"Accuracy"
#Set up models
set.seed(233)
fit.knn<-train(Survived~.,data=train1,method="knn",trControl=control,metric=metric)
set.seed(233)
fit.svm<-train(Survived~.,data=train1,method="svmRadial",trControl=control,metric=metric)
set.seed(233)
fit.cart<-train(Survived~.,data=train1,method="rpart",trControl=control,metric=metric)
set.seed(233)
fit.rf<-train(Survived~.,data=train1,method="rf",trControl=control,metric=metric)
set.seed(233)
fit.nb<-train(Survived~.,data=train1,method="nb",trControl=control,metric=metric)
result<-resamples(list(knn=fit.knn,svm=fit.svm,cart=fit.cart,rf=fit.rf,nb=fit.nb))
dotplot(result)
#Validate 
predicted<-predict(fit.svm,validate)
confusionMatrix(predicted,validate$Survived)
#Try on test data
test<-read.csv("test.csv")
#............
test<-test %>% 
  mutate(PassengerId=as.factor(PassengerId),Pclass=as.factor(Pclass),
        AgeGroup=as.factor(findInterval(Age,c(0,18,35,100)))) %>% 
  select(-Ticket,-Cabin,-Name,-Embarked)
levels(test$AgeGroup)<-c("Young","Mid Age","Aged")
levels(test$Sex)<-c("F","M")
#Make as train data
#Preprocess and remove NAs from age and Fare
   test2<-preProcess(test,method="medianImpute")
   test2_imp<-predict(test2,test)
   #map nas
   test2_imp<-test2_imp %>% 
     mutate(AgeGroup=as.factor(findInterval(Age,c(0,18,35,100))))
   #.....
   levels(test2_imp$AgeGroup)=c("Young","Mid Age","Aged")
predictedtest<-predict(fit.svm,test2_imp,na.action=na.pass)
 #Check variable  importance
varImp(fit.cart)
#Set column
Survival<-test2_imp%>% 
  mutate(Survived=predictedtest) %>% 
  select(PassengerId,Survived)


#find the confusion matrix
confusionMatrix(predictedtest,Survival$Survived)
#Write file
write.csv(Survival,"Submission13.csv",row.names = F)

