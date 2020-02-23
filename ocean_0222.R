setwd("/Users/vivi/Documents/04-UMD/10-case competition/0222-datachallenge/data/clustering")
ocean.cluster <- read.csv("Data_1.csv", header=TRUE, stringsAsFactors = FALSE)

View(ocean.cluster)

ocean.cluster$type[ocean.cluster$Cleanup.Type=="Land (beach, shoreline and inland)"] <- 0
ocean.cluster$type[ocean.cluster$Cleanup.Type=="Watercraft (powerboat, sailboat, kayak or canoe)"] <- 1
ocean.cluster$type[ocean.cluster$Cleanup.Type=="Underwater"] <- 2

o_cluster <- subset(ocean.cluster, select= -c(1:12,60))
o_cluster <- o_cluster[-37895,]
View(o_cluster)



####DETERMINE THE ###

set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- o_cluster
wss <- sapply(1:k.max, function(k){kmeans(o_cluster, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")





# Clustering
library(cluster) 
install.packages("factoextra")
library(factoextra)



plot(ocean.cluster)
help(silhouette)

silhouette_score <- function(k){
  km <- kmeans(ocean.cluster.re, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(ocean.cluster.re))
  mean(ss[, 3])
}
k <- 2:4
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)




####kmeans####
ocean.cluster.re.ID <- subset(ocean.cluster, select= -c(Cleanup.Type,X))

set.seed(20)
oceanCluster_9 <- kmeans(o_cluster, 9)
oceanCluster_2 <- kmeans(o_cluster, 2)
oceanCluster_9$size
oceanCluster_2$centers




View(o_cluster)
View(oceanCluster)
View(oceanCluster_2$centers)

kmean_withinss(2)





#### Cluster Plot against 1st 2 principal components ####
# vary parameters for most readable graph
library(cluster)
clusplot(o_cluster, oceanCluster_9$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(o_cluster, oceanCluster_2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)





# Centroid Plot against 1st 2 discriminant functions
install.packages("fpc")
library(fpc)
plotcluster(o_cluster, oceanCluster_9$cluster)
plotcluster(o_cluster, oceanCluster_2$cluster)









mydata <- d
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")











ocean.cluster <- subset(ocean, select= -c())

st.re <-  subset(st, select= -c(Institution.Filter, Micromaster.Completion.Flag, Number.of.Records, institution,
                                Micromaster.Course.Count, Micromasters.Program, City))


unique(ocean$Total.Items.Collected)
which(is.na(ocean$Total.Items.Collected))

sum(is.na(ocean))
which(is.na(ocean), useNames = T, arr.ind = T)
which


names(ocean)



#delete this totals column
ocean[22307,]




#group name
state, zone can be gps






set.seed(20)
oceanCluster <- kmeans(ocean[, 15:60], 5)
oceanCluster




























