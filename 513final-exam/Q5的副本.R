#  Name:WEILI LIU
#  CWID:10471020

#hclust
# remove all objects
rm(list=ls())

# import the dataset
filename<-file.choose()
data<-read.csv(filename)
data<-na.omit(data)
View(data)

data_dist<-dist(data[,-2])
data_resutls<-hclust(data_dist)
plot(data_resutls)
hclust_3<-cutree(data_resutls,4)
table(hclust_3,data[,2])


#kmeans
kmeans_2<- kmeans(data[,-2],4,nstart = 10)
kmeans_2$cluster
kmeans_2$centers
table(kmeans_2$cluster,data[,2])