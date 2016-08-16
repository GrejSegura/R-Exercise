###GGMAP EXERCISE

library(RCurl)
library(ggmap)
library(ggplot2)
library(dplyr)
library(ggrepel)

# to load the windows fonts to be used
library(extrafont)
windowsFonts(Courier=windowsFont("Courier New"))


#load the data from github
url <- getURL('https://raw.githubusercontent.com/GrejSegura/DataBank/master/Philippine%252520Open%252520Data/SY%202015%20ENROLMENT%20DATA%20with%20GEOLOCATIONS%20-%20ELEMENTARY.csv')

elem <- read.csv(text = url, header = T)

head(elem)

#filter the data to show only the maragusan municipality
elemmars <- elem %>% filter(municipality == "Maragusan (San Mariano)") %>% mutate(total = grade_1_male + grade_1_female + grade_2_male + grade_2_female + grade_3_male + grade_3_female + grade_4_male + grade_4_female + grade_5_male + grade_5_female + grade_6_male + grade_6_female)

##make sure to convert lat and long to numeric
elemmars$latitude <- as.character(elemmars$latitude)
elemmars$longitude <- as.character(elemmars$longitude)

elemmars$latitude <- as.numeric(elemmars$latitude)
elemmars$longitude <- as.numeric(elemmars$longitude)

elemmars <- elemmars[-26,]

str(elemmars)
head(elemmars)
#get the appropriate map using get_map()

map1 <- get_googlemap(center = c(126.153628, 7.348599), zoom = 12, maptype = 'satellite', size = c(640, 640))
m1 <- ggmap(map1)
m1
m2 <- m1 + geom_point(aes(x = longitude, y = latitude, size = total, color = school_name), data = elemmars, alpha = 0.8)
m3 <- m2 + scale_size_area(max_size = 25)
m3


m4 <- m3 + geom_label_repel(data = elemmars,aes(x = longitude, y = latitude, label = school_name), 
                            
                            
                            segment.color = 'white', force = 15, size = 3, fill = 'gray50', alpha = .5, color = 'white' ,family = 'Courier')
m4


m5 <- m4 + scale_y_continuous(name = 'Latitude\n')
m6 <- m5 + scale_x_continuous(name = '\nLongitude')
m6


#create different colors for every datapoints 
colorvalue = c("#216c95", "#C0A468", "#D96534", "#D12F19",
               "#88DBA1", "#d08504")

m6 <- m6 + ggtitle('Size Comparison of Enrollees in \nMaragusan Public Elementary Schools\n for SY 2015\n')

m7 <- m6 +  theme_minimal() + theme(text = element_text(color = "gray20"),
                                    plot.title = element_text(family = 'Courier', size = 12, face = "bold", vjust = 5),
                                    plot.margin = unit(c(.7,.7,.7,.7), "cm"),
                                    legend.position = 'none',
                                    axis.text = element_text(family = 'Courier'),
                                    axis.title.x = element_text(family = 'Courier', vjust = -1, size = 12, face = 'bold'), # move title away from axis, size is the font size
                                    axis.title.y = element_text(family = 'Courier', vjust = 1, size = 12, face = 'bold'), # move away for axis, size is the font size
                                    axis.ticks.y = element_blank(), # element_blank() is how we remove elements
                                    axis.line = element_line(color = "gray40", size = 0.5),
                                    axis.line.y = element_blank(),
                                    panel.grid.major = element_line(color = "gray50", size = 0.5),
                                    panel.grid.major.x = element_blank())
m7
