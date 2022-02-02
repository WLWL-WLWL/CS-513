#  Course          : CS513B Knowledge Dis & Data Mining
#  Project         : HW-7
#  Name            : WEILI LIU
#  SWID            : 10471020


rm(list=ls())

# Import data
filename<-file.choose()
data<-read.csv(filename)
View(data)


install.packages('neuralnet')
library(neuralnet)

#Clean data
data <- data[-c(1)]

for(i in 1 : length(data[,1])){
  if(data$diagnosis[i] =="M"){
    data$diagnosis[i] = 1;
  }else if(data$diagnosis[i] =="B"){
    data$diagnosis[i] = 2;
  }
}

for(i in 1 : length(data)){
  data[,i]<- as.numeric(data[,i])
}

data$diagnosis <- as.factor(data$diagnosis)
data <- na.omit(data)
summary(data)

#30% Testing and 70% Training
index<-sort(sample(nrow(data),round(.3*nrow(data))))
training<- data[-index,]
test<- data[index,]

#Five (5) nodes in the hidden layer
ann<- neuralnet(diagnosis~.,training, hidden=5, threshold=0.01,lifesign = "full")
plot(ann)
ANN <-compute(ann, test[,-1])
ANN$net.result 
ANN_diag<-ifelse(ANN$net.result[,2]<1/2,1,2)
table(Actual=test$diagnosis,predition=ANN_diag)

#Error rate
wrong<- (test$diagnosis!=ANN_diag)
error_rate<-sum(wrong)/length(wrong)
error_rate

