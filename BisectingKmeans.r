######
# Bisecting K-Means
#####
rm(list = ls(all = T))

library(RColorBrewer)

# set seed to ensure consistent results
set.seed(100)

# Read data file
# When	submitting, ensure that the path to this file is just "hw2-data.csv" 
# and doesn't point	to a path on your machine 
data.df <- read.csv('hw2-data.csv')

# TODO: Implement bisecting k means.
# Input:
# data.df: data frame based on hw2-data.csv
# trials.max: Max. number of trials for kmeans, as per Algorithm 8.2 in textbook. 
# You are allowed to use the pre-defined kmeans() function.
# k: Number of clusters to find in bisecting k-means

# Output:
# Your output/function return value will be a list containing 2 elements
# first element of the list is a vector containing cluster assignments (i.e., values 1 to k assigned to each data point)
# second element of the list is a vector containing SSE of each cluster
# Additional Information:
# When identifying which cluster to split, choose the one with maximum SSE
# When performing kmeans, pick two random points the cluster with largest SSE as centers at every iteration. 
# Be mindful that there might be duplicates in the data.
# terminating condition: when k clusters have been found

renameHeader <- function(clusList){
for(i in 1:length(clusList)){
clusList[[i]][3]<-NULL
names(clusList[[i]]) <- c("band1","band2")
}
clusList
}
findSSE <- function(clusList){
sseList <- c();
for(i in 1:length(clusList)){
xMean = mean(clusList[[i]]$band1)
yMean = mean(clusList[[i]]$band2)
sse = (clusList[[i]]$band1-xMean)^2 +(clusList[[i]]$band2-yMean)^2
sse_sum = sum(sse)
sseList[[i]] <-sse_sum

}
which.max(sseList)

}
getSSE <- function(clusList){
sseList <- c();
for(i in 1:length(clusList)){
xMean = mean(clusList[[i]]$band1)
yMean = mean(clusList[[i]]$band2)
sse = (clusList[[i]]$band1-xMean)^2 +(clusList[[i]]$band2-yMean)^2
sse_sum = sum(sse)
sseList[[i]] <-sse_sum

}
sseList

}

bisectingkmeans <- function(data.df, trials.max, k){
 # start your implementation here
 k1<-data.frame(data.df)
k1$ID<-NULL
clusList <- list()
clusList[[1]] <- k1
while(length(clusList)!=k){
	nextCluster <- clusList[[findSSE(clusList)]]
	clusList <- clusList[-findSSE(clusList)]
	kmean <- kmeans(nextCluster,2,iter.max = trials.max)
	k1cl <- data.frame(nextCluster,f=kmean$cluster)
	clist <- split(k1cl, f = k1cl$f)
	clusList[[length(clusList)+1]] <- data.frame(clist[1])
	clusList[[length(clusList)+1]] <- data.frame(clist[2])
	clusList <- renameHeader(clusList)
}
res<-clusList
for(i in 1:length(res)){
res[[i]]$cluster = i;
#print(i)
}
resMerged<-list()
for(i in 1:length(res)){
	resMerged <- do.call("rbind", list(resMerged,res[[i]]))
}
resMerged$index <- as.numeric(row.names(resMerged))
resMerged<-resMerged[order(resMerged$index),]
cluster<-resMerged$cluster
sseList<-getSSE(clusList=res)
retRes <- list(cluster,sseList)
retRes
  
}

# Write code for comparing result from bisecting kmeans here - Part b
kmeans_comparison <- function(data.df, result, k){
	k1<-data.df
	k1$ID<- NULL
	#print(k)
	kmean <- kmeans(k1,centers=k,iter.max = 25)
	
	#totSS <- sum(result[[2]])
	#print(kmean$cluster) 
	plot(k1$band1,k1$band2,col=kmean$cluster)
	plot(k1$band1,k1$band2,col=result[[1]])
	#kmean
}

# Don't edit anything beyond this line
# Please note, TA will test with different configurations of trails.max and k.
k=5
result <- bisectingkmeans(data.df, trials.max = 25 , k)
plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',
     cex = 3)

kmeans_comparison(data.df, result, k)

