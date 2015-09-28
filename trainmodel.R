setwd('~/Documents/machinelearningassignment')
train<-read.csv('pml-training.csv')
test<-read.csv('pml-testing.csv')
library(caret)
library(doMC)
library(dplyr)
registerDoMC(cores=6)

test.noNa<-test[,colSums(is.na(test))==0]
test.noNa<-select(test.noNa,-c(X,user_name,problem_id,cvtd_timestamp,new_window,num_window,raw_timestamp_part_1,raw_timestamp_part_2,roll_belt))
train.noNa<-subset(train,select=c(names(test.noNa),'classe'))
inTrain<-createDataPartition(y=train.noNa$classe,p=0.7,list=FALSE)
training<-train.noNa[inTrain,]
testing<-train.noNa[-inTrain,]
#fitControl    <- trainControl(method = "none")
#tgrid           <- expand.grid(mtry=c(6)) 
#system.time(model  <- train(classe ~ ., data = train.noNa, method = "rf"))
#table(train.noNa$classe,predict(model,train.noNa))

system.time(modelr<-randomForest(classe~.,data=train.noNa,mtry=sqrt(length(test.noNa)),ntree=500))
answersnorollbelt<-predict(modelr,test.noNa)

system.time(modelr<-randomForest(classe~.,data=training,mtry=sqrt(length(training)),ntree=500))
confusionMatrix(predict(modelr,testing),testing$classe)

system.time(modelrpart<-train(classe~.,data=training,method='glm'))
confusionMatrix(predict(modelrpart,testing),testing$classe)


