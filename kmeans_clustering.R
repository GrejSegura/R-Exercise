##k means clustering exercise JULY 12, 2016


##load the data --
##the data is called wine quality data which classifies the wines into 9 different quality types
data1 <- read.csv(file.choose(), header = T, sep = ",")

#remove the classifying variable --- quality, class

kdata <- data1[, -c(12,13)]

#scale the variables

kdata1 <- scale(kdata, center = T, scale = T)

##check the data again

head(kdata1)

##start clustering using the kmeans function in stats package

result <- kmeans(kdata1, 7)

##check the result

result

result$cluster

##compare the resulting cluster to the actual classification (data1$quality):
table(data1$quality, result$cluster)


##plot the clusters using ggplot2
library(ggplot2)

#add the result$cluster to the kdata
cluster <- as.data.frame(factor(result$cluster))
nrow(cluster)
kdata2 <- as.data.frame(cbind(kdata1, result$cluster))
names(kdata2)[12] <- "newcluster"

head(kdata2)

##make a graph
g <- ggplot(data = kdata2, aes(x=kdata2$sulphates, y=kdata2$total.sulfur.dioxide, col = factor(kdata2$newcluster))) + geom_point()
g
##show the centroids using the result$centers
centroid <-as.data.frame(result$centers)

g1 <- g + geom_point(data=centroid, aes(x=centroid$sulphates, y=centroid$total.sulfur.dioxide, col = "Center")) +
  geom_point(data=centroid, aes(x=centroid$sulphates, y=centroid$total.sulfur.dioxide, col = "Center"), size=20, alpha=.3)
g1
