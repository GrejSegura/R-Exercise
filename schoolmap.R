
map2 <- get_googlemap(location = 'Davao City', maptype = 'terrain')
a1 <- ggmap(map2)
a1library(RCurl)
library(ggplot2)
library(dplyr)
library(ggrepel)

# to load the windows fonts to be used
library(extrafont)
windowsFonts(gothic = windowsFont("century gothic"))

url <- getURL('https://raw.githubusercontent.com/GrejSegura/DataBank/master/Philippine%252520Open%252520Data/SY%202015%20ENROLMENT%20DATA%20with%20GEOLOCATIONS%20-%20ELEMENTARY.csv')

data <- read.csv(text = url)
View(data)
str(data)



#convert all x and y to numeric

data$latitude <- as.numeric(as.character(data$latitude))
data$longitude <- as.numeric(data$longitude)
count(any(is.na(data$latitude)))


m2 <- ggplot(data = data, aes(x = longitude, y = latitude)) + 
  geom_point(data = data,
  aes(x = longitude, y = latitude),
  alpha = 0.1, size = 0.00000005, color = 'white')
m2 <- m2 + coord_cartesian()


m6 <- m2 + ggtitle('So this is what our public elementary schools\nlook like when plotted.')

m7 <- m6 +  theme_minimal() + theme(text = element_text(color = "white"),
                                    plot.title = element_text(family = 'gothic', size = 20, hjust = -.001,vjust = 1),
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

ggsave(filename = "schoolmap.png", plot = m7, dpi = 600, width = 11, height = 7)
