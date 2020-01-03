install.packages('cluster')
install.packages('factoextra')
library(cluster) 
library(factoextra)
setwd('C:/Users/carlo/Desktop/Data science/ASDM/Clustering')
cost_data <-read.table("costpercompany.csv", sep = ",", header= TRUE)

#Inspect the dataset 
names(cost_data)
head(cost_data)
tail(cost_data)
summary(cost_data)
str(cost_data)
nrow(cost_data)
ncol(cost_data)
dim(cost_data) 

#scatterplot matrix to compare thevariables 
pairs(cost_data)

# Plot relationship between the Sales and Fuel cost of the company

plot(Fcost~ Sales, data = cost_data)
with(cost_data,text(Fcost ~ Sales, labels= Company,pos=4,cex=.6))

# Normalization ( ! important before cluestering since Euclidian distance that are sensitive to variations in the magnitude )
# Formula --> normalise <-function(df){return(((df-min(df)) /(max(df)-min(df))*(1-0))+0)}

company<-cost_data[,1]
cost_data_n<-cost_data[,2:9] # remove company column before normalise
cost_data_n<-as.data.frame(lapply(cost_data_n,normalise))
cost_data_n$Company<-company # addcompany column afternormalise
cost_data_n<-cost_data_n[,c(9,1,2,3,4,5,6,7,8)] # rearrange the columns in the dataset after normalising
head(cost_data_n)

# Note that, allowed values for the option methodinclude one of: "euclidean",  "maximum",  "manhattan",  "canberra",  "binary"  and "minkowski
distance <-dist(cost_data_n,method = "euclidean")
print(distance,digits=3)

# Visualisethe distance matrices. Red high similarity  and  Blue low similarity
fviz_dist(distance)

# inspect the first few observations
head(cost_data_n) # inspect the top observations 
rownames(cost_data_n)<-cost_data_n$Company # Set company names as row names
cost_data_n$Company<-NULL  # remove Company columnfrom the dataset
distance <-dist(cost_data_n,method = "euclidean")
fviz_dist(distance)


# Hierarchical clustering. hierarchical clustering tries to create a sequence of nested clusters to explore deeper insights  from  the data

cost_data.hclust <-hclust(distance)
cost_data.hclust
plot(cost_data.hclust,hang=-1)


# plot clusters for chosen number of clusters

rect.hclust(cost_data.hclust, 3) # draw 3 clusters
rect.hclust(cost_data.hclust, 4) # draw 4 clusters

# Cluster using average linkage
hclust.average <-hclust(distance, method = "average")
plot(hclust.average,labels=cost_data$Company)
rect.hclust(hclust.average, 4)

# Cluster using single linkage
hclust.single <-hclust(distance, method = "single")
plot(hclust.single,labels=cost_data$Company)
rect.hclust(hclust.single, 4)

# Cluster using centroid linkage
hclust.centroid<-hclust(distance, method = "centroid")
plot(hclust.centroid,labels=cost_data$Company)
rect.hclust(hclust.centroid, 4)

# Cluster using complete linkage
hclust.complete <-hclust(distance, method = "complete")
plot(hclust.complete,labels=cost_data$Company)
rect.hclust(hclust.complete, 4)


# Compare the cluster membership 
member.centroid <-cutree(hclust.centroid,4)
member.centroid
member.complete <-cutree(hclust.complete,4)
member.complete
table(member.centroid,member.complete)

# K-Means clustering to find homogeneous subgroups

kc<-kmeans(cost_data[,-1],3)#k=3
kc
clusplot(cost_data, kc$cluster, color=TRUE, shade=TRUE,  lines=0) # Plotting results