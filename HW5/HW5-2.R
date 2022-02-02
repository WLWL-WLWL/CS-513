#First name: WEILI
#Last name: LIU
#Student ID:10471020

rm(list = ls())

#reading the file
filename <- file.choose()
imputdata <- read.csv(filename,colClasses = c("Sample" = "character",
                                          "F1"="factor","F2"="factor","F3"="factor","F4"="factor","F5"="factor","F6"="factor","F7"="factor","F8"="factor","F9"="factor","Class"="factor"))
imputdata$Class<- factor(imputdata$Class , levels = c("2","4") , labels = c("Benign","Malignant"))
View(imputdata)   

#Install packages
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")
?install.packages()

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

#CART
index <- sort(sample(nrow(imputdata),round(.25*nrow(imputdata))))
trainning <- imputdata[-index,]
testing <- imputdata[index,]

#Train the model on rpart library and then plot
CART_class <- rpart(Class~.,data = trainning[,-1])
rpart.plot(CART_class)

# Predict using the trained model based on class and then print the confusion matrix
CART_predict <- predict(CART_class,testing,type="class")
table(Actual = testing[,11],CART = CART_predict)

# Predict using the trained model based on probabilities.
CART_predict2 <- predict(CART_class,testing)
str(CART_predict2)

prp(CART_class)
fancyRpartPlot(CART_class)

#Error rate
CART_predict <- predict(CART_class,testing,type="class")
CART_error <- sum(testing[,11]!=CART_predict)
CART_errorrate <- CART_error/length(testing[,11])
CART_errorrate
