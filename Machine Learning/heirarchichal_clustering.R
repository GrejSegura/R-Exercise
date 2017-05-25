## THIS IS An HEIRARCHICHAL CLUSTERING EXERCISE ## JULY 13, 2O16


##load the wine quality data
data1 <- read.csv(file.choose(), header = T, sep = ",")

#remove the classifying variable --- quality, class

data2 <- data1[, -c(12,13)]

#scale the variables

data3 <- scale(data2, center = T, scale = T)

head(data3)

##create a distance matrix (proximity matrix)

data3.dist <- dist(data3, method = "euclidean") ##expirement on the method to see better result

##implement the clustering

hcluster <- hclust(data3.dist, method = "mcquitty") ##expirement on the method to see better result
## the ff are the different linkage methods for hclust: "ward.D", "ward.D2", "single", "complete", 
## "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC)

##visualize by using a dendogram

plot(hcluster)

##visualize more by creating groups through rectangles

rect.hclust(hcluster, k = 7) # --->>> not functioning

##cut the tree to the desired number of groups, k = 7 this time

group <- cutree(hcluster, 7)

##compare the clusters to the original classification

predicted <- data.frame(data1[,12], group)

View(predicted)

##create a confusion matrix

table(data1[,12],group)
