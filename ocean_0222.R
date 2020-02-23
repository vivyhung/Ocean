setwd("/Users/vivi/Documents/04-UMD/10-case competition/0222-datachallenge/data/clustering")
ocean.cluster <- read.csv("Data_1.csv", header=TRUE, stringsAsFactors = FALSE)

View(ocean.cluster)

#recode cleanup type
ocean.cluster$type[ocean.cluster$Cleanup.Type=="Land (beach, shoreline and inland)"] <- 0
ocean.cluster$type[ocean.cluster$Cleanup.Type=="Watercraft (powerboat, sailboat, kayak or canoe)"] <- 1
ocean.cluster$type[ocean.cluster$Cleanup.Type=="Underwater"] <- 2

#ready for cluster
o_cluster <- subset(ocean.cluster, select= -c(1:12,60))
o_cluster <- o_cluster[-37895,]
View(o_cluster)


####elbow_2_ determine how many clusters####
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
wss <- sapply(1:15, function(k){kmeans(o_cluster[,-1], k, nstart=25 ,iter.max = 15 )$tot.withinss})
wss
plot(1:15, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


####kmeans (2, 9 clusters)####
set.seed(22)
oceanCluster_9 <- kmeans(o_cluster[,-1], 9, nstart = 25)
oceanCluster_9$size

# oceanCluster_9$size
#[1]     3    15     1     6  1194    75    85     1 36514

set.seed(12)
oceanCluster_2 <- kmeans(o_cluster[,-1], 2, nstart = 25)
oceanCluster_2$size
#> oceanCluster_2$size
#[1]    16 37878


# Cluster Plot against 1st 2 principal components
o<-o_cluster[,-1]
clusplot(o, oceanCluster_9$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(o, oceanCluster_2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#plot for reduced dimensions (PCA) - fviz_cluster
installed.packages("factoextra")
library(factoextra)
library(ggplot2)

fviz_cluster(oceanCluster_9, data = o)
fviz_cluster(oceanCluster_2, data = o)



















