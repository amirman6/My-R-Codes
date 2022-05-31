# Cluster Analysis
#  https://www.youtube.com/watch?v=otjWCaMcVaA

mydata <- read.csv(choose.files()) # load the car_cluster.csv file
str(mydata)
head(mydata)
apply(is.na(mydata),2,sum)
# nan filling
mydata$Engine.Cylinders[is.na(mydata$Engine.Cylinders)]<-mean(mydata$Engine.Cylinders,na.rm=TRUE)
mydata$Engine.Displacement[is.na(mydata$Engine.Displacement)]<-mean(mydata$Engine.Displacement,na.rm=TRUE)

pairs(mydata[,-1]) # pair plots wants only numeric data


# Normalize, finding the z-score 
z = mydata[,-1]
means = apply(z,2,mean)
sds = apply(z,2,sd)
nor = scale(z,center=means,scale=sds) # scale it to the mean = 0 and std = 1 for each columns
# check
mean(nor[,1]) # should be zero
sd(nor[,1]) # should be zero

##calculate distance matrix (default is Euclidean distance)
distance = dist(nor)
# it's euclidean distance:first run nor and see how it calculates the euclidean distance among all 8 variables for each companies
print(distance, digits = 4)

# Hierarchical agglomerative clustering using default complete linkage 
mydata.hclust = hclust(distance) # better than 'average method'
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$Make,main='Default from hclust',cex=0.6) # cex controls the x-axis text font size
plot(mydata.hclust,hang=-1,labels=mydata$Make,main='Default from hclust',cex=0.6)

# Hierarchical agglomerative clustering using "average" linkage 
mydata.hclust<-hclust(distance,method="average")
plot(mydata.hclust,hang=-1,labels=mydata$Make,main='Default from hclust',cex=0.6)



# Scree Plot: elbow plot, typlically you plot this at the begining of the clustering
# to figure out how many cluster size would be better
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 


# seperating the cluster with boxes and choose the k from above elbow method
rect.hclust(mydata.hclust, k = 4 , border = 'red') # it automatically finds for what height it gives the 3 clusters etc


# Cluster membership, finds out how may cars make are in each clusters
member = cutree(mydata.hclust,4)
table(member) # members in each cluster

#Characterizing clusters , check the video around 51.20
aggregate(nor,list(member),mean) # mean of each cluster to see which variable is contributing more in cluster formation
# if you put k = 5, it will calculate for means for 5 clusters group etc
aggregate(mydata[,-c(1,1)],list(member),mean) # for actual data

# silhouette plot, explained around 56 min
library(cluster)
plot(silhouette(cutree(mydata.hclust,4),distance)) # means higher the positive bars, the healthier the clusters are.





# K-means clustering
set.seed(123)
kc<-kmeans(nor,4) # change the no of cluster and check kc, the between cluster ss is higher the better but CHECK WHICH GIVES YOU BIG JUMPS
kc # run this and see, it provides the mean of each variables for each column vector
# and the sum of squares for within the cluster, (lower is better for within the cluster)
# it's similar to PCA with two component plots
# also check: between_SS/total_SS: higher the better cause clusters are seperated clearly means higher%
clusplot(mydata,
         kc$cluster,
         color = T,
         shade = T,
         labels = 2,
         lines = 0)

# try few things
kc$cluster
kc$withinss
kc$tot.withinss
kc$betweenss
kc$centers







