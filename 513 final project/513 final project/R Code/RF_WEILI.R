#  Course          : CS513 Knowledge Dis & Data Mining
#  Project         : final_project_Random Forest
#  Name            : WEILI LIU
#  SWID            : 10471020

rm(list=ls())

# import the dataset
filename<-file.choose()
data<-read.csv(filename)
View(data)

data <- data[-which(data$price==0),c(5,7:12,15:16)]

# Label some attributes.
price_level <- ifelse(
  data$price<100, "[0, 100)",
  ifelse(
    data$price<200, "[100, 200)","[200, +)"
  )
)

for(i in 1 : length(data[,2])){
  if(data$room_type[i] =="Entire home/apt"){
    data$room_type[i] = 1;
  }else if(data$room_type[i] =="Private room"){
    data$room_type[i] = 2;
  }else if(data$room_type[i]=="Shared room"){
    data$room_type[i] = 3; 
  }else{
    data$room_type[i] = NA;
  }
}

for(i in 1 : length(data[,1])){
  if(data$neighbourhood_group[i] =="Central Region"){
    data$neighbourhood_group[i] = 1;
  }else if(data$neighbourhood_group[i] =="West Region"){
    data$neighbourhood_group[i] = 2;
  }else if(data$neighbourhood_group[i]=="East Region"){
    data$neighbourhood_group[i] = 3; 
  }else{
    data$neighbourhood_group[i] = NA;
  }
}

for(i in 1 : length(data)){
  data[,i]<- as.numeric(data[,i])
}

data <- data.frame(data[,-5],price_level)
data$price_level <- as.factor(data$price_level)
dataset <- na.omit(data)
View(dataset)



install.packages("randomForest")
library('randomForest')

#Training and testing
index <- sort(sample(nrow(dataset), round(.25*nrow(dataset))))
training <- dataset[-index,]
testing <- dataset[index,]

rf_fit<-randomForest(price_level~ .,data = training,importance=TRUE, ntree=1000)
plot(rf_fit)
importance(rf_fit)
varImpPlot(rf_fit)

#Prediction
Prediction <- predict(rf_fit, testing)
table(actual=testing[,9],Prediction)

#Error rate of RF
wrong<- (testing[,9]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 
accuracy<- 1-error_rate
accuracy
