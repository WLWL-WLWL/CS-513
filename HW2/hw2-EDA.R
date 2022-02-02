#First name: WEILI
#Last name: LIU
#ID: 10471020

rm(list = ls())

inputdata <- read.csv("Desktop//breast-cancer-wisconsin.data.csv", header = TRUE, na.strings = '?')
inputdata

#I.	Summarizing each column (e.g. min, max, mean )

summary(inputdata)

#II.	Identifying missing values

inputdata[is.na(inputdata)]
is.na(inputdata)
sum(is.na(inputdata))

#III.	Replacing the missing values with the "mean" of the column.

new_data<-transform(inputdata, F6 = as.integer(F6))
for(i in 1:ncol(new_data)){
  new_data[is.na(new_data[,i]), i] <- mean(new_data[,i], na.rm = TRUE)
}  
View(new_data)

#IV.	Displaying the frequency table of "Class" vs. F6

mydata<-table(Class = inputdata$Class, Sitem = inputdata$F6)
ftable(mydata)

#V.	Displaying the scatter plot of F1 to F6, one pair at a time

plot(inputdata$F1~inputdata$F2)
plot(inputdata$F1~inputdata$F3)
plot(inputdata$F1~inputdata$F4)
plot(inputdata$F1~inputdata$F5)
plot(inputdata$F1~inputdata$F6)
plot(inputdata$F2~inputdata$F3)
plot(inputdata$F2~inputdata$F4)
plot(inputdata$F2~inputdata$F5)
plot(inputdata$F2~inputdata$F6)
plot(inputdata$F3~inputdata$F4)
plot(inputdata$F3~inputdata$F5)
plot(inputdata$F3~inputdata$F6)
plot(inputdata$F4~inputdata$F5)
plot(inputdata$F4~inputdata$F6)
plot(inputdata$F5~inputdata$F6)
plot(inputdata[2:7], main = "Scatter Plot from F1 to F6",pch = 21,col=c("blue","green"))

#VI.	Show histogram box plot for columns F7 to F9

hist(inputdata$F7, main = "Histogram F7", xlab = "columns F7")
hist(inputdata$F8, main = "Histogram F8", xlab = "columns F8")
hist(inputdata$F9, main = "Histogram F9", xlab = "columns F9")

boxplot(inputdata[8:10], main = "Box plot F7", xlab = "columns F7")
boxplot(inputdata$F8, main = "Box plot F8", xlab = "columns F8")
boxplot(inputdata$F9, main = "Box plot F9", xlab = "columns F9")
boxplot(inputdata[8:10], main = "Box Plot for Columns F7 to F9",pch = 21,col=c("blue","green"))


#2- Delete all the objects from your R- environment. Reload the "breast-cancer-wisconsin.data.csv" from canvas into R. Remove any row with a missing value in any of the columns.

rm(list = ls())

inputdata <- read.csv("Desktop//breast-cancer-wisconsin.data.csv", header = TRUE, na.strings = '?')

inputdata2 <- na.omit(inputdata)
inputdata2

