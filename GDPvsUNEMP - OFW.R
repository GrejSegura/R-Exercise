#GDP PER CAPITA VS UNEMPLOYMENT FOR TOP 20 OFW'S DESTINATION COUNTRIES
#GET THE DATA STORED IN https://github.com/GrejSegura

library(RCurl)

url <- getURL('https://raw.githubusercontent.com/GrejSegura/DataBank/master/Philippine%2520Open%2520Data/ofw%20population%20per%20country.csv')
ofwdata <- read.csv(text = url, header = T)
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

#replace the long strings to be used later on labels
list <- as.vector(data[,'country_of_destination_3'])
list <- replace(list, list == "United States of America", "USA")
list <- replace(list, list == "United Arab Emirates", "UAE")
list <- replace(list, list == "United Kingdom", "UK")

data$country_of_destination_3 <- list


head(data)

#plot the chart

g1 <- ggplot(data, aes(x = gdp, y = unemp)) + geom_jitter(aes(size = X2012), alpha = 0.2, color = '#A61407') + scale_size_area(max_size = 29) + geom_jitter(aes(size = X2012, color = country_of_destination_2), alpha = 0.7) + scale_size_area(max_size = 30)


#label the points

g2 <- g1 + geom_text_repel(aes(label = country_of_destination_3), 
                           color = "gray10", 
                           data = subset(data, country_of_destination_3 %in% list), 
                           segment.color = 'gray10',
                           segment.size = .01,
                           force = 10, 
                           size = 4, 
                           family = 'Courier')
g2

g3 <- g2 + scale_y_continuous(name = 'Unemployment Rate\n', limits = c(0,30), breaks = seq(0,30,by = 10))
g3 <- g3 + scale_x_continuous(name = '\nGDP per capita PPP (in USD1000)', limits = c(0,150), breaks = seq(0,150,by = 30))

g3

#create different colors for every datapoints 
colorvalue = c("#5B0400", "#fb2e01", "#B38235", "#A61407", "#00515C", "#6fcb9f",
               "#00515C")


g4 <- g3 + scale_color_manual(values = colorvalue) + ggtitle('GDP PER CAPITA VS UNEMPLOYMENT RATE \nOF THE TOP DESTINATIONS OF THE OFWs\n')
g4 <- g4 + labs(colour = "Region", size = "OFW Population\n in thousands")

g5 <- g4 +  theme_minimal() + theme(text = element_text(color = "gray20"),
                                    plot.title = element_text(family = 'Courier', size = 14, face = "bold", vjust = 5),
                                    plot.margin = unit(c(.4,.4,.4,.4), "cm"),
                                    #for the legends
                                    legend.position = 'right',
                                    legend.title = element_text(family = 'Courier', face = 'bold'),
                                    legend.text = element_text(family = 'Courier', face = 'bold'),
                                    
                                    
                                    #for the axis
                                    axis.text = element_text(family = 'Courier'),
                                    axis.title.x = element_text(family = 'Courier', vjust = 1, size = 12, face = 'bold'), # move title away from axis, size is the font size
                                    axis.title.y = element_text(family = 'Courier', vjust = 1, size = 12, face = 'bold'), # move away for axis, size is the font size
                                    axis.ticks.y = element_blank(), # element_blank() is how we remove elements
                                    axis.line = element_line(color = "gray40", size = 0.5),
                                    axis.line.y = element_blank(),
                                    
                                    panel.grid.major = element_line(color = "gray50", size = 0.5),
                                    panel.grid.major.x = element_blank())
g5
