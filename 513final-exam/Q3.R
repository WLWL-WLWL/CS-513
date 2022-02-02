#Name: WEILI LIU
#CWID: 10471020

rm(list=ls())

filename <- file.choose()
inputdata<- read.csv(filename)

#Delete rows with missing values
data<-na.omit(inputdata)
sapply(data,class)

data$Age<- as.numeric(as.character(data$Age))
data$JobSatisfaction<- as.numeric(as.character(data$JobSatisfaction))
data$MonthlyIncome<- as.numeric(as.character(data$MonthlyIncome))
data$YearsAtCompany<- as.numeric(as.character(data$YearsAtCompany))
data$Attrition <- as.numeric(as.factor(data$Attrition))
data$MaritalStatus<- as.numeric(as.factor(data$MaritalStatus))


#Select 30% of the records as the test dataset and the remaining records as the training dataset
index<-sort(sample(nrow(data),as.integer(0.3*nrow(data))))
training<-data[-index,]
testing<-data[index,]

#Perform ANN, with 6 hidden nodes, for classification for the “attrition” column 
install.packages('neuralnet')
library(neuralnet)
ann_class<-neuralnet(Attrition~.,data = training,hidden = 6,threshold = 0.01,act.fct = "logistic")

#Plot Artificial Neural Network model
plot(ann_class)

# Predict
ann_predict<-predict(ann_class,testing)
ann_predict
ann_predict_cat<-ifelse(ann_predict<1.5,'1','2')

#Evaluating the accuracy
wrong<- (testing$Attrition!=ann_predict_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate
