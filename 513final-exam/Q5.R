#  Name:WEILI LIU
#  CWID:10471020

rm(list=ls())

filename<-file.choose()
data<-read.csv(filename)
data<-na.omit(data)
View(data)

normalize <- function(x)
{
  x <- (x-min(x))/(max(x)-min(x))
  return(x)
}

data$Returns_pct1<- normalize(data$Returns_pct1)
data$Returns_pct2<- normalize(data$Returns_pct2)
data$Returns_pct3<- normalize(data$Returns_pct3)
data$Returns_pct4<- normalize(data$Returns_pct4)
data$Returns_pct5<- normalize(data$Returns_pct5)
data$Returns_pct6<- normalize(data$Returns_pct6)


#Hierarchical clustering
imputdata<-data[,-c(1,2,3,10)]
data_dist<-dist(imputdata)
data_h<-hclust(data_dist)
plot(data_h)
hc<-cutree(data_h,4)
table(hc,data[,10])


#K-means
km<- kmeans(data[,-c(1,2,3,10)],4,nstart = 10)
km$cluster
km$centers
table(km$cluster,data[,10])
