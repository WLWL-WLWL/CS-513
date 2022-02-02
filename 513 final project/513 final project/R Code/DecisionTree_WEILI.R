#  Course          : CS513 Knowledge Dis & Data Mining
#  Project         : final_project_Decision Tree
#  Name            : WEILI LIU
#  SWID            : 10471020

rm(list=ls())
install.packages("rpart")
install.packages("rpart.plot")     
install.packages("rattle")         
install.packages("RColorBrewer") 

# Load Packages
library(rpart)
library(rpart.plot)		
library(rattle)											
library(RColorBrewer)

# Import the dataset
filename<-file.choose()
data<-read.csv(filename)
View(data)
data <- data[-which(data$price==0),c(9:11,15,16)]

data$room_type <- as.factor(data$room_type)

# Label  attributes.
price_level <- ifelse(
  data$price<100, "[0, 100)",
  ifelse(
    data$price<200, "[100, 200)","[200, +)"
  )
)

data <- data.frame(data[,-2],price_level)
data$price_level <- as.factor(data$price_level)
dataset <- na.omit(data)
View(dataset)

#Training and testing
index <- sort(sample(nrow(dataset),as.integer(.3*nrow(dataset))))
training<- dataset[-index,]
testing<- dataset[index, ]
summary(testing)
summary(training)

#CART 
Dtree<-rpart(price_level~.,data=training)
fancyRpartPlot(Dtree)

#Prediction
prediction<-predict(Dtree	,testing,	type="class")
table(Actual=testing[,5],prediction)

#Error rate of DT
right<-(testing[,5]==prediction)
accuracy<-sum(right)/length(right)
accuracy







