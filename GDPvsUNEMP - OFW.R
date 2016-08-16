#GDP PER CAPITA VS UNEMPLOYMENT FOR TOP 20 OFW'S DESTINATION COUNTRIES

ofwdata <- read.csv(file.choose(), header = T, sep = ',')

head(ofwdata)

#load the necessary libraries
library(ggplot2)
library(dplyr)
library(ggrepel)
# to load the windows fonts to be used
library(extrafont)
windowsFonts(Courier=windowsFont("Courier New"))

#sort the countries with the most number of ofw
?arrange
data <- ofwdata %>% arrange(desc(X2012))

data$gdp <- data$gdp/1000
data$X2012 <- data$X2012/1000

head(data)

#plot the chart

g1 <- ggplot(data, aes(x = gdp, y = unemp, size = X2012, color = country_of_destination_2)) + geom_point(alpha = 0.7) + scale_size_area(max_size = 30)
g1

#label the points

list <- data[,'country_of_destination_3']
list

g2 <- g1 + geom_text_repel(aes(label = country_of_destination_3), 
                           color = "gray20", 
                           data = subset(data, country_of_destination_3 %in% list), 
                           force = 10, size = 4, family = 'Courier')
g2

g3 <- g2 + scale_y_continuous(name = 'Unemployment Rate\n', limits = c(0,30), breaks = seq(0,30,by = 10))
g3 <- g3 + scale_x_continuous(name = '\nGDP per capita PPP (in USD1000)', limits = c(0,150), breaks = seq(0,150,by = 30))

g3

#create different colors for every datapoints 
colorvalue = c("#88DBA1", "#C0A468", "#D96534", "#D12F19",
               "#88DBA1", "#D12F19", "#52071C")


g4 <- g3 + scale_color_manual(values = colorvalue) + ggtitle('GDP PER CAPITA VS UNEMPLOYMENT RATE \nOF THE TOP 20 DESTINATIONS OF THE OFWs\n')
g4 

g5 <- g4 +  theme_minimal() + theme(text = element_text(color = "gray20"),
                                    plot.title = element_text(family = 'Courier', size = 14, face = "bold", vjust = 5),
                                    plot.margin = unit(c(1,1,1,1), "cm"),
                                    legend.position = 'none',
                                    axis.text = element_text(family = 'Courier'),
                                    axis.title.x = element_text(family = 'Courier', vjust = -1, size = 12, face = 'bold'), # move title away from axis, size is the font size
                                    axis.title.y = element_text(family = 'Courier', vjust = 1, size = 12, face = 'bold'), # move away for axis, size is the font size
                                    axis.ticks.y = element_blank(), # element_blank() is how we remove elements
                                    axis.line = element_line(color = "gray40", size = 0.5),
                                    axis.line.y = element_blank(),
                                    panel.grid.major = element_line(color = "gray50", size = 0.5),
                                    panel.grid.major.x = element_blank())
g5

?legend.position
?geom_jitter
