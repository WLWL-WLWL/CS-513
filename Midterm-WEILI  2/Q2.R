
# Name :  WEILI LIU        
# CWID : 10471020            

# Question 2
rm(list= ls())
filename <- file.choose()
inputdata <- read.csv(filename)
View(inputdata)

# Summarizing each column
summary(inputdata)

# Identifying missing values
is.na(inputdata)

# Displaying frequency table of infected vs martialstatus
frequency_table<-table(inputdata$Infected , inputdata$MaritalStatus)
frequency_table

# Creating a new data frame consisting of only Age MaritalStatus and Month at Hospital
new_data <- data.frame(inputdata$Age, inputdata$MaritalStatus, inputdata$MonthAtHospital)

#Displaying the scatter plot of “Age”, “MaritalStatus” and “MonthAtHospital”, one pair at a time
plot(new_data[1:3], main = "Scatter Plot", pch = 21,col = c("red","black","blue"))

#Show box plots for columns:  “Age”and “MonthAtHospital”
boxplot(new_data$inputdata.Age,new_data$inputdata.MonthAtHospital, main = "Box Plot", pch = 21,col = c("red","blue"))

#Replacing the missing values of “Cases” with the “mean” of “Cases”.
inputdata[is.na(inputdata$Cases),"Cases"] <-  mean(inputdata$Cases, na.rm = TRUE)
View(inputdata)
