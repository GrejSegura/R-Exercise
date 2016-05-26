## MY GRAPHING PRACTICE
## this is a sample code to show line graph and bar graph in one chart
library(plyr)
library(ggplot2)
library(RColorBrewer)
#load landdata
landdata <- read.csv(file.choose(),header=T, sep=",")
#create new dataframe by averaging home value per state
AveHome <- ddply(landdata, .(STATE), summarize, HomeValue = mean(Home.Value))
#create new dataframe averaging structure cost per state
AveCost <- ddply(landdata, .(STATE), summarize, CostAve = mean(Structure.Cost))

##############################GRAPHING STARTS HERE##############################

#graph the bar graph first using home value
g1 <- ggplot(AveCost, aes(x=STATE, y=CostAve)) + geom_bar(fill="#1963a4",stat="identity")
#include the Cost Average by using line graph
g1 <- g1 + geom_line(data = AveHome, colour="#00203d", size=.7, aes(x= STATE, y = HomeValue), group="STATE")
# add some points
g1 <- g1 + geom_point(data = AveHome, color="#00530a", 
                      shape = 1, size=3, aes(x= STATE, y = HomeValue))

g1<- g1 + geom_point(data = AveHome, color="#00530a", 
                shape = 1, size=3.5, aes(x= STATE, y = HomeValue))
g1<- g1 + geom_point(data = AveHome, color="#00530a", 
           shape = 1, size=4, aes(x= STATE, y = HomeValue))

g1<- g1 + geom_point(data = AveHome, color="#00530a", 
           shape = 1, size=3.2, aes(x= STATE, y = HomeValue))
# change labels
f1 <- g1 + labs(title="Average Structure Cost and Home Value per State in the US", 
                x="US States", y="Structure Cost and Home Value (in thousands)", color="#00101e")
#format Y axis
f2 <- f1 + scale_y_continuous(breaks=seq(0,450000,50000), labels=c(0,50,100,150,200,250,300,350,400,450))

#format background colors
f3 <- f2 + theme(panel.background = element_rect(fill = "#e5edf4"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 plot.background = element_rect(fill = "#6697c2"),
                 axis.text = element_text(color = "#00182e")
                 )
f3
