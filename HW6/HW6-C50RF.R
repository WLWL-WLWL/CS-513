# First name: WEILI 
# Last name: LIU
# ID : 10471020

rm(list=ls())

filename <- file.choose()
imputdata<- read.csv(filename,header = TRUE,na.strings = "?",colClasses=c("Sample"="character",
                                           "F1"="factor","F2"="factor","F3"="factor",
                                           "F4"="factor","F5"="factor","F6"="factor",
                                           "F7"="factor","F8"="factor","F9"="factor",
                                           "Class"="factor"))
imputdata$Class<- factor(imputdata$Class , levels = c("2","4") , labels = c("Benign","Malignant"))
View(imputdata)
imputdata<-na.omit(imputdata[,-1])

index <- sort(sample(nrow(imputdata), round(.25*nrow(imputdata))))
training <- imputdata[-index,]
testing <- imputdata[index,]

#install.packages("C50")
library('C50')

C50_list <- C5.0( Class~.,data=training )
summary(C50_list)
plot(C50_list)
Predict<-predict( C50_list ,testing , type="class" )
table(actual=testing[,10],C50=Predict)

#Error rate
error <- (testing[,10]!=Predict)
errorrate<-sum(error)/length(testing[,10])
errorrate


#Random Forest
install.packages("randomForest")
library('randomForest')

randomForest_class<-randomForest(Class~.,data = training)
summary(randomForest_class)
plot(randomForest_class)
randomForest_predict<-predict( randomForest_class ,testing , type="class" )

#Error rate of RF
error2<- (testing[,10]!=randomForest_predict)
errorrate2<-sum(error2)/length(testing[,10])
errorrate2
