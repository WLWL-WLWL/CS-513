#  Course          : CS513 Knowledge Dis & Data Mining
#  Project         : final_project_Naive_Bayes
#  First Name      : Yi
#  Last Name       : Rong
#  SWID            : 10472696

# remove all objects
rm(list=ls())

# import the dataset
filename<-file.choose()
data<-read.csv(filename)
View(data)

# Load Packages
library(e1071)
library(class)

# choose valuable data set(price not 0)
data <- data[-which(data$price==0),]

data$neighbourhood <- as.factor(data$neighbourhood)
data$neighbourhood_group <- as.factor(data$neighbourhood_group)
data$room_type <- as.factor(data$room_type)

price_level <- ifelse(
  data$price<100, "[0, 100)",
  ifelse(
    data$price<200, "[100, 200)","[200, +)"
  )
)

data <- data.frame(data, price_level)
data$price_level <- as.factor(data$price_level)

data <- data[,-c(1:4, 10:16)]
summary(data)
View(data)

#test data set and training data set
selected_index <- sort(sample(nrow(data),as.integer(.70*nrow(data))))
test_data <- data[selected_index, ]
training_data <- data[-selected_index,]
summary(test_data)
summary(training_data)


#Perform Naive Bayes 
install.packages(e1071)
naiveBayes<- naiveBayes(price_level~ ., data = training_data)
predict <- predict(naiveBayes, test_data)

#Confusion matrix for finding accuracy
conf_matrix <- table(predict,test_data$price_level)
conf_matrix

#Measure the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x))))}
print(paste("Accuracy:" , accuracy(conf_matrix)))

#sum(na.omit(as.numeric(as.numeric(predict)==as.numeric(test_data$price_level))))/nrow(test_data)



