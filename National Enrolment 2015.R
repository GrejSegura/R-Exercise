###GGMAP EXERCISE

library(RCurl)
library(ggmap)
library(ggplot2)
library(dplyr)
library(ggrepel)

# to load the windows fonts to be used
library(extrafont)
windowsFonts(lucida = windowsFont("lucida grande"))
windowsFonts(courier = windowsFont("courier new"))

#load the data from github
url <- getURL('https://raw.githubusercontent.com/GrejSegura/DataBank/master/Philippine%252520Open%252520Data/SY%202015%20ENROLMENT%20DATA%20with%20GEOLOCATIONS%20-%20ELEMENTARY.csv')

elem <- read.csv(text = url)
nrow(elem)
head(elem)
str(elem)
#latitude is factor, convert the latitude to numeric

elem$latitude


elem$latitude <- as.numeric(as.character(elem$latitude))
elem$longitude <- as.numeric(elem$longitude)

#filter the data to show only the maragusan municipality
elem1 <- elem %>% 
  group_by(province, region) %>% 
  mutate(total = grade_1_male + grade_1_female + grade_2_male + grade_2_female + 
           grade_3_male + grade_3_female + grade_4_male + grade_4_female + 
           grade_5_male + grade_5_female + grade_6_male + grade_6_female) %>% 
  summarise(latitude = mean(latitude, na.rm = TRUE), 
            longitude = mean(longitude, na.rm = TRUE), total = sum(total, na.rm = TRUE))


elem2 <- elem1[,c(1,3,4)]


View(elem1)

#data is ready for graphing now

philmap <- get_googlemap(center = c(122.726331, 12.060213), zoom = 6, maptype = 'hybrid', size = c(640, 640))
a1 <- ggmap(philmap)
a1
a2 <- a1 + geom_jitter(aes(x = longitude, y = latitude, size = total, color = region), data = elem1, alpha = 0.6)
a2
a3 <- a2 + scale_size_area(max_size = 15)
a3 <- a3 + guides(size = FALSE)
a3

a4 <- a3
a4
#create different colors for every datapoints 
colorvalue = c("#216c95", "#C0A468", "#D96534", "#D12F19","#88DBA1",
               "#3AB1CC","#E8AC2C", "#A49C82", "#DFA08F", "#CB480E",
               "#90A830","blue", "#F0C018", "#D86048", "#90A830", "#216c95")

a6 <- a4 + scale_color_manual(name = "Region", values = colorvalue) + ggtitle('Relative Size of Public Elementary School \nEnrollees Outside NCR')

a7 <- a6 +  theme_minimal() + theme(text = element_text(color = "gray20"),
                                    plot.title = element_text(family = 'courier', size = 14, face = "bold", 
                                                               color = 'white'),
                                    plot.margin = unit(c(1,1,1,1), "cm"),
                                    
                                    legend.position = 'right',
                                    legend.text = element_text(family = 'courier', size = 8, color = 'white'),
                                    legend.title = element_text(family = 'courier', size = 14, color = 'white'),
                                    legend.title.align = .5,
                                    
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
a7

ggsave(filename = "NationaEnrollees.png", plot = a7, dpi = 300, width = 9, height = 7)
