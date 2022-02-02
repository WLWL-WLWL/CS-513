#  Course          : CS513 Knowledge Dis & Data Mining
#  Project         : final_project_C5.0
#  Name            : WEILI LIU
#  SWID            : 10471020
rm(list=ls())

filename <- file.choose()
imputdata<- read.csv(filename,header = TRUE,na.strings = "?",colClasses=c("neighbourhood_group"="factor",
                                                                         
                                                                          "room_type"="factor" ))
imputdata <- imputdata[-which(imputdata$price==0),]
imputdata$price <- ifelse(
  imputdata$price<100, "[0, 100)",
  ifelse(
    imputdata$price<200, "[100, 200)","[200, +)"
  )
)
View(imputdata)
#Delete useless features
imputdata$price <- as.factor(imputdata$price)
dataset = imputdata[,-c(1:4,6:8,13:14)]
summary(dataset)

install.packages("C50")
library('C50')

#Training and testing
index <- sort(sample(nrow(dataset), round(.25*nrow(dataset))))
training <- dataset[-index,]
testing <- dataset[index,]


C50_list <- C5.0( price~.,data=training[,])
summary(C50_list)
plot(C50_list)
Predict<-predict( C50_list ,testing , type="class" )
table(actual=testing[,3],Predict)

#Error rate
error <- (testing[,3]!=Predict)
errorrate<-sum(error)/length(testing[,3])
accuracy<- 1-errorrate
accuracy

