###GGMAP EXERCISE USING RANDOMLY GENERATED DATA NETWORK FOR ASIAN COUNTRIES

library(RCurl)
library(ggmap)
library(ggplot2)
library(dplyr)
library(ggrepel)

# to load the windows fonts to be used
library(extrafont)
windowsFonts(gothic = windowsFont("century gothic"))


data <- read.csv(file.choose())
View(data)
str(data)

#convert all x and y to numeric

data$lat2 <- as.numeric(data$lat2)
data$long2 <- as.numeric(data$long2)


map1 <- get_googlemap(center = c(75.153628, 44.048599), zoom = 2, maptype = 'satellite', size = c(540, 350))
m1 <- ggmap(map1, darken = 1)
m1
m2 <- m1 + coord_cartesian() + geom_segment(data = data,
                                          aes(x = long1, y = lat1, xend = long2, yend = lat2, type = 'closed'),
                                          alpha = 0.02, inherit.aes = TRUE, color = 'white')
m2


m6 <- m2 + ggtitle('Random Network - Asian Countries')

m7 <- m6 +  theme_minimal() + theme(text = element_text(color = "white"),
                                    plot.title = element_text(family = 'gothic', size = 22, vjust = 1),
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
                                    panel.grid.major = element_blank(),
                                    panel.grid.major.x = element_blank())
m7





ggsave(filename = "asiannetwork.png", plot = m7, dpi = 600, width = 11, height = 7)
