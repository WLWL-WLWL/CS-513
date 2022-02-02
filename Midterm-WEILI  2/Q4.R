# Name :  WEILI LIU        
# CWID : 10471020            

#Question 4
rm(list= ls())
filename <- file.choose()
inputdata <- read.csv(filename)
View(inputdata)

# Remove NA values
inputdata <-na.omit(inputdata )

#Split the inputdata into training and testing
train_data = floor(0.7*nrow(inputdata )) 
train_ind = sample(seq_len(nrow(inputdata )),size = train_data)
train =inputdata [train_ind,]
test=inputdata [-train_ind,]

#Install the KNN package
install.packages("kknn") 

# Load
library(kknn)
library(class)

# KNN prediction 
predict_k5<-kknn(formula = Infected~.,train, test, k=5, kernel = "rectangular")
fit<-fitted(predict_k5)

# Displaying Confusion matrix
table(Actual=test$Infected, Fitted=fit)
conf_mat1<-table(Actual = test$Infected, Fitted = fit)
conf_mat1

# Accuracy of the model
accuracy1<-sum(diag(conf_mat1)/nrow(test)) * 100 
accuracy1 
