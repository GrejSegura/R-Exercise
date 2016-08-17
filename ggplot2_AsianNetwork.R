### EXERCISE USING RANDOMLY GENERATED DATA NETWORK FOR ASIAN COUNTRIES

library(RCurl)
library(ggplot2)
library(dplyr)
library(ggrepel)

# to load the windows fonts to be used
library(extrafont)
windowsFonts(gothic = windowsFont("century gothic"))

url <- getURL('https://raw.githubusercontent.com/GrejSegura/DataBank/master/location_asia.csv')

data <- read.csv(text = url)
View(data)
str(data)

#convert all x and y to numeric

data$lat2 <- as.numeric(data$lat2)
data$long2 <- as.numeric(data$long2)



m2 <- ggplot(data = data, aes(x = long1, y = lat1)) + geom_segment(data = data,
                                            aes(x = long1, y = lat1, xend = long2, yend = lat2, type = 'closed'),
                                            alpha = 0.008, inherit.aes = TRUE, color = 'white')
m2


m6 <- m2 + ggtitle('Random Network - Asian Countries')

m7 <- m6 +  theme_minimal() + theme(text = element_text(color = "white"),
                                    plot.title = element_text(family = 'gothic', size = 15, vjust = 1),
                                    plot.margin = unit(c(1,1,1,1), "cm"),
                                    legend.position = 'none',
                                    axis.text = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    axis.ticks.y = element_blank(), # element_blank() is how we remove elements
                                    axis.line = element_blank(),
                                    axis.line.y = element_blank(),
                                    
                                    plot.background = element_rect(fill = 'black', colour = 'black'),
                                    
                                    panel.background = element_rect(fill = 'black', colour = 'black'),
                                    panel.grid.major.y = element_blank(),
                                    panel.grid.major.x = element_blank(),
                                    panel.grid.minor.x = element_blank(),
                                    panel.grid.minor.y = element_blank())
m7

ggsave(filename = "asiannetwork.png", plot = m7, dpi = 400, width = 11, height = 7)
