#  Course          : CS513 Knowledge Dis & Data Mining
#  Project         : final_project_hclust
#  First Name      : Yi
#  Last Name       : Rong
#  SWID            : 10472696

# remove all objects
rm(list=ls())

# import the dataset
filename<-file.choose()
data<-read.csv(filename)
View(data)

data <- data[-which(data$price==0),c(5,9:12,15:16)]

# Label some attributes.
price_level <- ifelse(
  data$price<100, "[0, 100)",
  ifelse(
    data$price<200, "[100, 200)","[200, +)"
  )
)

for(i in 1 : length(data[,2])){
  if(data$room_type[i] =="Entire home/apt"){
    data$room_type[i] = 1;
  }else if(data$room_type[i] =="Private room"){
    data$room_type[i] = 2;
  }else if(data$room_type[i]=="Shared room"){
    data$room_type[i] = 3; 
  }else{
    data$room_type[i] = NA;
  }
}

for(i in 1 : length(data[,1])){
  if(data$neighbourhood_group[i] =="Central Region"){
    data$neighbourhood_group[i] = 1;
  }else if(data$neighbourhood_group[i] =="West Region"){
    data$neighbourhood_group[i] = 2;
  }else if(data$neighbourhood_group[i]=="East Region"){
    data$neighbourhood_group[i] = 3; 
  }else{
    data$neighbourhood_group[i] = NA;
  }
}

for(i in 1 : length(data)){
  data[,i]<- as.numeric(data[,i])
}

data <- data.frame(data[,-3],price_level)
data$price_level <- as.factor(data$price_level)
data <- na.omit(data)

# Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
price_level <- data[,7]
data <- data.frame(lapply(data[,-7],normalize),price_level)
View(data)

data$neighbourhood_group <- as.factor(data$neighbourhood_group)
data_dist<-dist(data[,-7])
summary(data_dist)


hclust_resutls<-hclust(data_dist)
plot(hclust_resutls)
#close image plots
dev.off()
hclust_3 <- cutree(hclust_resutls,3)
table(hclust_3)
table(hclust_3,data[,7])

#install.packages("dendextend")
library(dendextend)
dend <- as.dendrogram(hclust_resutls)
dend <- rotate(dend, 1:7356)
# Color the branches based on the clusters:
dend <- color_branches(dend, k=3) 
dend <- hang.dendrogram(dend,hang_height=0.1)
plot(dend)

t<-table(hclust=hclust_3,actual=data[,7])



