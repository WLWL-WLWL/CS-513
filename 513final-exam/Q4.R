#Name: WEILI LIU
#CWID: 10471020

rm(list=ls())

filename <- file.choose()
inputdata<- read.csv(filename)

#Delete rows with missing values
data<-na.omit(inputdata)
data$Age<- as.numeric(as.character(data$Age))
data$MonthlyIncome<- as.numeric(as.character(data$MonthlyIncome))
data$YearsAtCompany<- as.numeric(as.character(data$YearsAtCompany))
data$Attrition <- as.factor(data$Attrition)

#Select 30% of the records as the test dataset and the remaining records as the training dataset
index <- sort(sample(nrow(data), round(.3*nrow(data))))
training <- data[-index,]
testing <- data[index,]

#Perform Random Forest classification for the “attrition” column
install.packages("randomForest")
library('randomForest')

randomForest_class<-randomForest(Attrition~.,data = training,importance=TRUE, ntree=1000)
randomForest_predict<-predict( randomForest_class ,testing , type="class" )

importance(randomForest_class)
varImpPlot(randomForest_class)

#Error rate 
error2<- (testing[,6]!=randomForest_predict)
errorrate2<-sum(error2)/length(testing[,6])
errorrate2
