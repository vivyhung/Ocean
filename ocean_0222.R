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


####kmeans (8 clusters)####
set.seed(20)
oceanCluster_8 <- kmeans(o_cluster[,-1], 8, nstart = 25)
oceanCluster_8$size

# oceanCluster_8$size
# [1]    94     6  1427    11 36218    16   118     4

















