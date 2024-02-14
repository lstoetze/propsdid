###############################################################################################################################
#################Figure 1: Spatial distribution of support for PSOE in 2016 and 2019 in Spanish Municipalities
###############################################################################################################################


## Install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rgeos")
install.packages("rgdal")
install.packages("sf")
install.packages(c("maps", "mapdata"))
install.packages("RColorBrewer")
install.packages("ggpubr")

library(ggplot2)
library (dplyr)
library (rgdal)
library(maps)
library(mapdata)
library(raster)
library(sf)
library(tidyverse) 
library(broom) #to convert shapefiles into the data frame structure we need
library(RColorBrewer)
library(readxl)
library(ggpubr)

#set your working directory
setwd("~/replication_just_transition")

electoral_data <- read_excel("2016elecciones.xlsx" , sheet = "PSOE2016")
electoral_data2 <- read_excel("2019elecciones.xlsx" , sheet = "PSOE2019")

map <- readOGR("ESP_adm4.shp")
map_fortified <- tidy(map, region = "NAME_4")
map_data_spain <- map_fortified %>% left_join(electoral_data, by = c("id" = "municipalities"))   %>% fill(PSOE2016)
rm(data_spain) 
rm(electoral_data)

canaries_line <- data.frame(long = c(-36e4, 15e4, 15e4),
                            lat = c(405e4, 405e4, 385e4))

#set the legend
theme_diane_maps <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "white", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
      plot.caption = element_text(size = 7.5, color = "grey40"),
      legend.title = element_text(color = "grey40", size = 6),
      legend.text = element_text(color = "grey40", size = 6, hjust = 0),
      legend.position = "bottom",
      legend.text.align = 0,
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(2,0.2,.2,0.2), "cm"))
}

pal1 <-brewer.pal(n=5, name = "Reds")

canaries <- data.frame(lat = c(28.291565), lon= c (-16.629129))

#Create the coalmines
d <- data.frame(lat=c(42.768),  
                lon=c(-6.62514))

e<- data.frame(lat=c(43.334333),
               lon=c(-6.411729))

f<- data.frame(lat=c(43.012114),
               lon=c (-6.792236))

g<- data.frame(lat=c(43.149702),
               lon=c (-5.687874))

h<- data.frame(lat=c(43.207156),
               lon=c (-5.888154))

i<- data.frame(lat=c(43.249719),
               lon=c (-5.778528))

j<- data.frame(lat=c(43.317734),
               lon=c (-5.707796))

k<- data.frame(lat=c(43.288204),
               lon=c (-5.660834))

l<- data.frame(lat=c(43.260555),
               lon=c (-5.579375))

m<- data.frame(lat=c(43.275129),
               lon=c (-5.608749))

n<- data.frame(lat=c(43.177384),
               lon=c (-6.549840))

o<- data.frame(lat=c(43.182840),
               lon=c (-3.987843))

p<- data.frame(lat=c(42.939861),
               lon=c (-6.568330))

q<- data.frame(lat=c(42.937329584),
               lon=c (-6.568997724))

r<- data.frame(lat=c(42.772206),
               lon=c (-6.584942))

s<- data.frame(lat=c(42.60769),
               lon=c (-6.36675))

t<- data.frame(lat=c(42.741319),
               lon=c (-6.223396))

u<- data.frame(lat=c(42.829108),
               lon=c (-5.113968))

v<- data.frame(lat=c(42.617364),
               lon=c (-6.416166))

w<- data.frame(lat=c(42.833371),
               lon=c (-4.866257))

x<- data.frame(lat=c(42.815575),
               lon=c (-4.946873))

y<- data.frame(lat=c(42.941571),
               lon=c (-6.327653))

z<- data.frame(lat=c(42.85658 ),
               lon=c (-5.66768))

a<- data.frame(lat=c(40.854637 ),
               lon=c (-0.633446))

b<- data.frame(lat=c(41.355918),
               lon=c (0.383708))

c<- data.frame(lat=c(40.9225537),
               lon=c (-0.2659163))

da<- data.frame(lat=c(41.025597),
                lon=c (-0.558929))

ea<- data.frame(lat=c(38.657938),
                lon=c (-4.136053))

#Set up for the 2016 map : without Canary Islands
quantile(map_data_spain$PSOE2016, probs = c(.2,.4,.6,.8), na.rm = TRUE)
pretty_breaks <- c(16.17162,20.92452,25.17233,33.10188)

minVal <- min(map_data_spain$PSOE2016, na.rm = T)
maxVal <- max(map_data_spain$PSOE2016, na.rm = T)

#All together:
brks <- c(minVal, pretty_breaks, maxVal)
#Labels
labels <- c()
# round the extremes
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}
labels <- labels[1:length(labels)-1]

#Bringing into df
map_data_spain$brks <- cut(map_data_spain$PSOE2016, 
                           breaks = brks, 
                           include.lowest = TRUE, 
                           labels = labels)
brks_scale <- levels(map_data_spain$brks)
labels_scale <- rev(brks_scale)

#Create the map for 2016
figure2016 <- ggplot()+
  #we use brks for the fill and resuce the size of the borders
  geom_polygon(data=map_data_spain, aes(fill=brks, x=long, y=lat, group= group ), color = "white", size = 0.05)+
  geom_point(data= d, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2, show.legend = NA) +
  geom_point(data= e, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= f, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= g, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= h, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= i, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= j, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= k, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= l, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= m, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= n, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= o, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= p, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= q, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= r, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= s, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= t, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= u, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= v, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= w, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= x, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= y, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= z, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= a, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= b, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= c, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= da, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= ea, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  coord_sf(xlim = c (-13.8531, 6.3), ylim = c(35.6686, 46.874), expand = FALSE)  +
  #Adding the color palette 
  #AND setting how I want the scale to look like
  scale_fill_manual(
    values = rev(pal1), #I use rev so that red is for lowest values 
    breaks = rev(brks_scale),
    name = "PSOE Vote Share",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(direction = "horizontal",
                         keyheight = unit(2, units = "mm"),
                         keywidth = unit(50 / length(labels), units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 1,
                         nrow = 1,
                         byrow = T,
                         reverse = T,
                         label.position = "bottom"))+
  labs(title="2016 PSOE Support in Spanish Municipalities")+
  theme_diane_maps()

# Create in 2019
#year 2019
map_data_spain1 <- map_fortified %>% left_join(electoral_data2, by = c("id" = "municipalities"))   %>% fill(PSOE)
quantile(map_data_spain1$PSOE, probs = c(.2,.4,.6,.8), na.rm = TRUE)
pretty_breaks1 <- c(21.49111,27.08861,31.07417,37.33576)

minVal1 <- min(map_data_spain1$PSOE, na.rm = T)
maxVal1 <- max(map_data_spain1$PSOE, na.rm = T)

#All together:
brks1 <- c(minVal1, pretty_breaks1, maxVal1)

#Labels
labels <- c()
# round the extremes
for(idx in 1:length(brks1)){
  labels <- c(labels,round(brks1[idx + 1], 2))
}
labels <- labels[1:length(labels)-1]

#Bringing into df
map_data_spain1$brks1 <- cut(map_data_spain1$PSOE, 
                             breaks = brks1, 
                             include.lowest = TRUE, 
                             labels = labels)
brks_scale1 <- levels(map_data_spain1$brks1)
labels_scale1 <- rev(brks_scale1)

#Create Map for 2019
figure2019 <-ggplot()+
  #we use brks for the fill and resuce the size of the borders
  geom_polygon(data=map_data_spain1, aes(fill=brks1, x=long, y=lat, group= group ), color = "white", size = 0.05)+
  geom_point(data= d, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2, show.legend = NA) +
  geom_point(data= e, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= f, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= g, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= h, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= i, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= j, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= k, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= l, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= m, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= n, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= o, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= p, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= q, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= r, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= s, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= t, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= u, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= v, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= w, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= x, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= y, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= z, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= a, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= b, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= c, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= da, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  geom_point(data= ea, aes(x = lon, y = lat), alpha = .7, 
             color = "black", size=2) +
  coord_sf(xlim = c (-13.8531, 6.3), ylim = c(35.6686, 46.874), expand = FALSE)  +
  #Adding the color palette 
  #AND setting how I want the scale to look like
  scale_fill_manual(
    values = rev(pal1), #I use rev so that red is for lowest values 
    breaks = rev(brks_scale1),
    name = "PSOE Vote Share",
    drop = FALSE,
    labels = labels_scale1,
    guide = guide_legend(direction = "horizontal",
                         keyheight = unit(2, units = "mm"),
                         keywidth = unit(50 / length(labels), units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 1,
                         nrow = 1,
                         byrow = T,
                         reverse = T,
                         label.position = "bottom"))+
  labs(title="2019 PSOE Support in Spanish Municipalities")+
  theme_diane_maps()

figure <- ggarrange(figure2016, figure2019,
                     ncol = 2, nrow = 1)

ggsave(figure, filename="figure_1.pdf")