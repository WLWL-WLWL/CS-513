#  Course          : CS513 Knowledge Dis & Data Mining
#  Project         : final_project_kmeans
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



kmeans_3<- kmeans(data[,-7],3,nstart = 10)

#close image plots
kmeans_3$cluster
kmeans_3$centers
table(kmeans_3$cluster,data[,7])
