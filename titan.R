#Basic Machine Learning on kaggle titanic dataset
nomsg<-suppressMessages
nomsg(library(tidyverse))
nomsg(library(caret))
nomsg(library(mice))
nomsg(library(xgboost))
library(RANN)
library(caretEnsemble)
train<-read.csv("train.csv",stringsAsFactors = F)
train<-as.tibble(train)
#Remove cabin,name,Ticket for now as these may not predict survival 
#Extract Title from Name in Train. Give score to social class
newtrain<-train %>% 
  separate(Name,into=c("First","Title","Last"),"\\.|,") %>% 
  mutate(First=as.factor(First),Title=as.factor(Title),Last=as.factor(Last))
  #View and deal with nas
newtrain<-newtrain %>% 
  mutate(PassengerId=as.factor(PassengerId),Pclass=as.factor(Pclass),
         Survived=as.factor(Survived),
         AgeGroup=as.factor(findInterval(Age,c(0,18,35,100)))) %>% 
      select(-PassengerId,-First,-Last,-Ticket,-Cabin,-Embarked,-Title)
#Change levels
levels(newtrain$AgeGroup)<-c("Young","Mid Age","Aged")
levels(newtrain$Sex)<-c("F","M")
#Impute median
newtrain_1<-mice(data=newtrain,m=3,method="cart")
newtrain_imp<-complete(newtrain_1,3)
#checkNAs
anyNA(newtrain_imp)

#Let's visualise survival by Age Group
newtrain_imp %>% 
   ggplot(aes(Survived,fill=Sex))+geom_histogram(stat="count")+facet_wrap(AgeGroup~Pclass)+
  ggtitle("Survival by class,Agegroup and Gender")+
  theme(plot.title=element_text(hjust=0.5)) 
#The graph does suggest that being of mid Age and embarking in the third class made you more likely to die
#Overall more women than men survived.
#Partition our data into a training and test dataset

#Create partition
train1<-createDataPartition(newtrain_imp$Survived,p=0.8,list=F)
validate<-newtrain_imp[-train1,]
train1<-newtrain_imp[train1,]
#Set metric and control
control<-trainControl(method="cv",number =5)
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
#Try Gradiet Boosting
fit.gbm<-train(Survived~.,data=train1,method="gbm",trControl=control,metric=metric,
               verbose=F)
fit.xgboo<-train(Survived~.,data=train1,method="xgbTree",trControl=control,metric=metric,
                 verbose=F)
result<-resamples(list(knn=fit.knn,svm=fit.svm,cart=fit.cart,rf=fit.rf,nb=fit.nb,
                       gbm=fit.gbm,xgb=fit.xgboo))
dotplot(result)
#Validate 
predicted<-predict(fit.gbm,validate)
confusionMatrix(predicted,validate$Survived)

#Try on test data
test<-read.csv("test.csv",stringsAsFactors = F)
test<-as.tibble(test)
#............
#Make as train data
newtest<-test%>% 
  separate(Name,into=c("First","Title","Last"),"\\.|,") %>% 
  mutate(First=as.factor(First),Title=as.factor(Title),Last=as.factor(Last))
#.....
newtest<-newtest %>% 
  mutate(PassengerId=as.factor(PassengerId),Pclass=as.factor(Pclass),
         AgeGroup=as.factor(findInterval(Age,c(0,18,35,100)))) %>% 
  select(-Ticket,-First,-Last,-Cabin,-Embarked,-Title)
levels(newtest$AgeGroup)<-c("Young","Mid Age","Aged")
levels(newtest$Sex)<-c("F","M")
#Find NAs
newtest %>% 
  map_lgl(~anyNA(.x))
#Preprocess and remove NAs from age and Fare
newtest_1<-mice(newtest,m=3,method="cart")
newtest_imp<-complete(newtest_1,3)
   #map nas
anyNA(newtest_imp)
#Check Dona

predictedtest<-predict(fit.rf,newtest_imp,na.action=na.pass)
#Check variable  importance
plot(varImp(fit.rf))
#Set column
Survival<-newtest_imp%>% 
  mutate(Survived=predictedtest) %>% 
  select(PassengerId,Survived)
#find the confusion matrix
confusionMatrix(predictedtest,Survival$Survived)
#Using xgboost
write.csv(Survival,"submitme13.csv",row.names = F)
options(scipen = 0)
#Generalised linear model
#Encode Dummy variables
str(train)
#Fit a logistic regression model. Predicts binary outcome from numeric data
fit.logit<-glm(Survived~.,data=newtrain_imp,family=binomial())
summary(fit.logit)
prob<-predict(fit.logit,validate,type="response")
logit.pred<-factor(prob>.5,levels=c(F,T),labels=c("Nope","Survived"))
#Confusion matrix
logit.perf<-table(validate$Survived,logit.pred,dnn=c("Actual","Prediction"))
logit.perf
#Accuracy is around 82% 
(96+50)/(13+96+18+50)
#remove non useful data
reduced<-step(fit.logit)
#Apply reduced model
prob1<-predict(reduced,validate,type="response")
logit.pred1<-factor(prob1>.05,levels=c(F,T),labels=c("Nope","Survived"))
logit.perf1<-table(validate$Survived,logit.pred1,dnn=c("Actual","Prediction"))
logit.perf1
#New Accuracy is actually worse 
(13+68)/(13+96+68)
#Apply dummy encoding 
dmy<-dummyVars("~.",newtrain_imp,fullRank = T)
train_transformed<-data.frame(predict(dmy,newtrain_imp))
train_transformed$Survived.1<-as.factor(train_transformed$Survived.1)
#Train new data
train_trans1<-createDataPartition(train_transformed$Survived.1,p=0.75,list=F)
train_trans_validate<-train_transformed[-train_trans1,]
train_trans1<-train_transformed[train_trans1,]
#Set metric and control
control2<-trainControl(method="cv",number=10)
metric2<-"Accuracy"
#Train
model_gbm<-train(Survived.1~.,data=train_trans1,method="gbm",trControl=control2,
                 metric=metric2)
model_rf<-train(Survived.1~.,data=train_trans1,method="rf",trControl=control2,
                metric=metric2)
model_cart<-train(Survived.1~.,data=train_trans1,method="rpart",trControl=control2,
                 metric=metric2)
model_svm<-train(Survived.1~.,data=train_trans1,method="svmRadial",trControl=control2,
                 metric=metric2)
model_xgb<-train(Survived.1~.,data=train_trans1,method="xgbTree",trControl=control2,
                 metric=metric2)
result2<-resamples(list(cart=model_cart,gbm=model_gbm,glm=model_glm,
                        rf=model_rf,svm=model_svm,xgb=model_xgb))
dotplot(result2)
plot(varImp(model_rf),main="Who's Important in the Forest?!")
#Apply on test data
dmytest<-dummyVars("~.",newtest_imp,fullRank = T)
test_transformed<-data.frame(predict(dmytest,newtest_imp))
predt<-predict(model_gbm,test_transformed)
subm<-newtest_imp %>% 
  mutate(Survived=predt) %>% 
  select(PassengerId,Survived)
write.csv(subm,"subm123.csv",row.names = F)
