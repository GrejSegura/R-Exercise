library(ggplot2)
winedata <- read.csv( file.choose(), header = T, sep =";")
w <- ggplot(winedata, aes(quality))+geom_bar( binwidth=.5, color= "#339999", fill = "#339999" )
w <- w + xlab("QUALITY")
w <- w + theme_minimal()


# another example
# using the landdata price
library(ggplot2)
library(ggthemes)

landdata <- read.csv(file.choose(),header=T, sep=",")

# graph the average home price
# to do this use plyr to summarize the data using the averages
library(plyr)


priceAve <- ddply(landdata, .(STATE, Date), summarize, HomeValue = mean(Home.Value))
head(priceAve)

#plot using ggplot2 with geom_point

g1 <- ggplot(priceAve, aes(x= Date , y= HomeValue, color = STATE)) + geom_point(shape = 1, size= 2)
g1 + geom_point(shape = 1, size = 1.8) + geom_point(shape = 1, size = 1.6)
g2 <- g1 + scale_x_continuous(breaks = seq(1975, 2015, by = 5))
g3 <- g2 + ggtitle("Average Home Values per Year")

# graph for Arkansas only
# make a subset data
AKPrice <- subset(priceAve, STATE == "AK")
head(AKPrice)

#make a graph
g1 <- ggplot(AKPrice, aes(x= Date , y= HomeValue)) + geom_point(shape = 1, size= 2)
g1 + geom_point(shape = 1, size = 1.8) + geom_point(shape = 1, size = 1.6)
g2 <- g1 + scale_x_continuous(breaks = seq(1975, 2015, by = 5))
g3 <- g2 + ggtitle("Average Home Values per Year")
g3
