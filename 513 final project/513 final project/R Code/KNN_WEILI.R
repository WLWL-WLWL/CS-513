#  Course          : CS513 Knowledge Dis & Data Mining
#  Project         : final_project_KNN_kknn
#  Name            : WEILI LIU
#  SWID            : 10471020

# Remove all objects
rm(list=ls())
library(class)

# Import the dataset
filename<-file.choose()
data<-read.csv(filename)
View(data)

data <- data[-which(data$price==0),c(5,9:12,15:16)]

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

data <- data.frame(data[,-3],price_level)
data$price_level <- as.factor(data$price_level)
dataset <- na.omit(data)

# Normalization based on Min-Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
price_level <- dataset[,7]
data <- data.frame(lapply(dataset[,-7],normalize),price_level)
View(data)

#Training and testing
idx <- sort(sample(nrow(data),as.integer(.25*nrow(data))))
training <- data[-idx,] 
testing <- data[idx,]

# Accuracy
for(k in seq(1,100,4)){
  predict <- knn(training[,-7], testing[,-7], training$price_level,k)
  table(Actual=testing[,7],KNN=predict)
  accuracy <-sum(testing[,7] == predict) / length(testing[,1])
  print(paste("k = ",k," accuracy = ",accuracy))}



#kknn
install.packages("kknn")
library(kknn)

# k=85-unweighted
predict<- kknn(formula=price_level~., training, testing, k=85,kernel ="rectangular")
fit <- fitted(predict)
table(testing$price_level,fit)

wrong<- (testing$price_level !=fit)
wrong_rate<-sum(wrong)/length(wrong)
wrong_rate

# Accuracy_unweighted
accuracy_unweighted<- 1-wrong_rate
accuracy_unweighted

# k=85-weighted
predict2<- kknn(formula=price_level~., training, testing, k=85,kernel ="triangular")
fit2<- fitted(predict2)
table(testing$price_level,fit)

wrong2<- (testing$price_level !=fit)
wrong_rate2<-sum(wrong2)/length(wrong2)
wrong_rate2

# Accuracy_weight
accuracy_weight<- 1-wrong_rate2
accuracy_weight




