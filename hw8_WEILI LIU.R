#  Course          : CS513B Knowledge Dis & Data Mining
#  Project         : HW-8
#  Name            : WEILI LIU
#  SWID            : 10471020


#Q1
rm(list=ls())

# Import data
filename<-file.choose()
data<-read.csv(filename)
data<-na.omit(data)
View(data)

data_dist<-dist(data[,-2])
data_h<-hclust(data_dist)
plot(data_h)
hc<-cutree(data_h,2)
table(hc,data[,2])


#Q2
km<- kmeans(data[,-2],2,nstart = 10)
km$cluster
km$centers
table(km$cluster,data[,2])

