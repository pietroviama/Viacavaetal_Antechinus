###############################################################################
###############################################################################
library(ggplot2)
library(maps)
library(plyr)
library(dplyr)


#extract map of Australia (or any other country/region) border points

world_map <- map_data("world")
australia <- subset(world_map, world_map$region=="Australia")
australia

#create a base plot with ggplot2

p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#Add map to base plot

australia_messy <- p + geom_polygon(data=australia, aes(x=long, y=lat, group=group), 
                                    colour="black", fill="white")

australia_messy

#clean up the map

cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

australia_clean <- australia_messy + cleanup

australia_clean

#read GPS coordinates

#Load the classifier
antechinusdata <- read.csv("C:/Users/pietro/Desktop/Pietro/Projects/Viacavaetal_Antechinus/Data/Antechinus_cranialvariation_data.csv", header=T)


is.factor(antechinusdata$Species)

#plot data points in map

australiamap_data <- 
  australia_clean+
  geom_point(data=antechinusdata, 
             aes(x=Longitude, y=Latitude, fill =Species)
             ,pch=21, size=4, alpha=I(0.7))+
  theme(legend.position = c(1,0.8), legend.justification = c(0,1), legend.key = element_blank(), legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))+
  labs(fill="Species")


australiamap_data

#######################################################################################
#######################################################################################

library(ggplot2)
library(ggmap)
library(maps)

antechinusdata <- read.csv("C:/Users/pietro/Desktop/Pietro/Projects/Viacavaetal_Antechinus/Data/Antechinus_cranialvariation_data.csv", header=T)

antechinusdata1 <- antechinusdata[-c(184,186),]

range(antechinusdata$Longitude, na.rm = TRUE, finite = TRUE)
range(antechinusdata$Latitude, na.rm = TRUE, finite = TRUE)

base = get_map(location=c(145,-37,155,-25), zoom=7, maptype="terrain-background")

map1 = ggmap(base)
map1

map2 <- 
  map1+
  geom_point(data=antechinusdata1, 
             aes(x=Longitude, y=Latitude, fill = Species, shape = Species), color="white", cex=4) +
  scale_fill_manual(values = c("red", "blue"), labels=c("stuartii", "subtropicus"), name=NULL) +
  scale_shape_manual(values = c(24,22), labels=c("stuartii", "subtropicus"), name=NULL) + # define shape/color scales
  labs(x="Latitude", y="Longitude", title="Antechinus distribution map") + # label the axes
  theme_bw() + theme(legend.position="bottom", axis.text = element_text(size = rel(0.75)), legend.key = element_rect(colour = "white"), axis.text.x = element_text(angle=45, vjust=0.5)) # tweak the plot's appearance and legend position

map2

