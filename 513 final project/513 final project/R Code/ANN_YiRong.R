#  Course          : CS513 Knowledge Dis & Data Mining
#  Project         : final_project_Artificial_Neural_Network
#  First Name      : Yi
#  Last Name       : Rong
#  SWID            : 10472696

# remove all objects
rm(list=ls())

# import the dataset
filename<-file.choose()
data<-read.csv(filename)
View(data)

summary(data)

# choose valuable data set(price not 0 and Numerical data) and Delete unrelated data
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
data <- na.omit(data)

# Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
price_level <- data[,7]
data <- data.frame(lapply(data[,-7],normalize),price_level)

# Get training data and test data
index<-sort(sample(nrow(data),round(.3*nrow(data))))
training<- data[-index,]
test<- data[index,]

#install.packages("neuralnet")
library("neuralnet")

# Train neural network
nn<- neuralnet(price_level~.,training, hidden=5, threshold=0.01,lifesign = "full")
plot(nn)

# Predict test data.
ann <- compute(nn,test[,-7])
ann$net.result 

# Label prediction by probability.
ann_cat<-ifelse(ann$net.result[,2] < 1/3,"[0, 100)",
                ifelse(ann$net.result[,2] < 2/3,"[100, 200)","[200, +)"))


# Error rate for test data
table(Actual=test$price_level,predition=ann_cat)
wrong<- (test$price_level!=ann_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate

# Accuracy for test data
accuracy <- 1 - error_rate
accuracy
