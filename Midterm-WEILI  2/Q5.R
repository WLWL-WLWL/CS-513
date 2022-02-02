# Name :  WEILI LIU        
# CWID : 10471020            

#Question 5
rm(list= ls())
library(e1071)
filename <- file.choose()
inputdata <- read.csv(filename)
View(inputdata)

# Remove NA values
inputdata<-na.omit(inputdata)

# Discretize the “MonthAtHospital” into “less than 6 months” and “6 or more months”
inputdata$Age<-cut(inputdata$Age , c(0,35,50,60))
inputdata$Age<- factor(inputdata$Age, levels = c("(0,35]","(35,50]","(50,60]") , labels = c("Less 35","35 to 50","More than 50"))

# Discretize the Age into “less than 35”, “35 to 50” and “51 or over”
inputdata$MonthAtHospital<-cut(inputdata$MonthAtHospital, c(0,6,32))
inputdata$MonthAtHospital<- factor(inputdata$MonthAtHospital, levels = c("(0,6]","(6,32]") , labels = c("Less than 6 Months","6 months or more"))

# Split into 70% training  and 30% testing
index<-sort(sample(nrow(inputdata),round(0.30*nrow(inputdata))))
training<-inputdata[-index,]
testing<-inputdata[index,]

# Naive Bayes Algorithm
nbayes_all<-naiveBayes(Infected~.,data = training)

# Predict using the testing data
category_all<-predict(nbayes_all,testing)
table(NBAYES_ALL = category_all,Class = testing$Infected)

# Error Rate
NB_wrong<-sum(category_all!=testing$Infected)
NB_error_rate<-NB_wrong/length(category_all)

# Accuracy  
accuracy<- 1 - NB_error_rate
accuracy
