#First name: WEILI 
#Last name: LIU
#ID: 10471020


rm(list=ls())

filename <- file.choose()
inputdata <- read.csv(filename, na.strings="?")
View(inputdata)

deletemissingrows <- na.omit(inputdata)
deletemissingrows

deletemissingrows$Class <- as.factor(deletemissingrows$Class)
View(deletemissingrows$Class)
is.factor(deletemissingrows$Class)

library(class)
install.packages("e1071",dep=T)
library(e1071)

nBayes<- naiveBayes(as.factor(Class) ~Class, data = deletemissingrows)
category <- predict(nBayes,deletemissingrows)

classdata <- cbind(deletemissingrows, category)
table(Class =deletemissingrows$Class, Class=deletemissingrows$Class )

table(Class =deletemissingrows$Class, NBayes = category)


table(NBayes=category,Class=deletemissingrows$Class)

nBayes <- naiveBayes(Class ~Class, data=inputdata)
category <- predict(nBayes,inputdata)

nBayesalldata<-naiveBayes(Class ~., data=deletemissingrows)

categoryalldata<-predict(nBayesalldata,deletemissingrows)
table(nBayesalldata=categoryalldata,Class=deletemissingrows$Class)

NBwrongdata<-sum(categoryalldata!=deletemissingrows$Class)

NBwrongdata

wrongdatarate<-NBwrongdata/length(categoryalldata)
wrongdatarate

print(paste("Right rate:",(1-wrongdatarate)*100))

