#Basic Machine Learning on kaggle titanic dataset
nomsg<-suppressMessages
nomsg(library(tidyverse))
nomsg(library(caret))
library(RANN)
train<-read.csv("train.csv")
train<-as.tibble(train)
#impute

#Remove cabin,name,Ticket for now as these may not predict survival 
train<-train %>% 
  mutate(PassengerId=as.factor(PassengerId),Pclass=as.factor(Pclass),
         Survived=as.factor(Survived),AgeGroup=as.factor(findInterval(Age,c(0,18,35,85)))) %>% 
 select(-PassengerId,-Name,-Ticket,-Cabin) %>% 
  na.omit()
#View and deal with nas
anyNA(train$AgeGroup)
#Change levels
summary(train$Age)
levels(train$AgeGroup)<-c("Young","Mid Age","Aged","NA")
levels(train$Sex)<-c("F","M")

str(train)
#Let's visualise survival by Age Group
train %>% 
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
#Examination of levels of Embarked shows a difference. Aim to make levels unifrom
levels(train1$Embarked)

#Set metric and control
control<-trainControl(method="cv",number=10)
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
predicted<-predict(fit.rf,validate)
confusionMatrix(predicted,validate$Survived)
#The model is 84% accurate
#Impute
preprocm<-preProcess(train1,method=c("knnImpute"))
fit.knn_imp<-predict(preprocm,train1)
#Try on test data
test<-read.csv("test.csv")
#change levels
levels(test$Embarked)<-c("","C","Q","S")
head(test)
str(test)
test<-test %>% 
  mutate(PassengerId=as.factor(PassengerId),Pclass=as.factor(Pclass),
        AgeGroup=as.factor(findInterval(Age,c(0,18,35,85)))) %>% 
  select(-Ticket,-Cabin,-Name)
levels(test$AgeGroup)<-c("Young","Mid Age","Aged","NA")
levels(test$Sex)<-c("F","M")
#predict on test data
predictedtest<-predict(fit.cart,test,na.action=na.pass)
 #Check variable  importance
varImp(fit.rf)
#Preprocess
test_imp<-preProcess(test,method="knnImpute")
test1<-predict(test_imp,test)
predict(fit.rf,test1)
#Save results in a given dataframe 
Survival<-test%>% 
  mutate(Survived=predictedtest) %>% 
  select(PassengerId,Survived)
#Write 
write.csv(Survival,file="Submission.csv",row.names = F)


