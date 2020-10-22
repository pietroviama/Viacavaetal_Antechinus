library(ggplot2)
library(ggmap)
library(maps)



range(antechinusdata$Longitude, na.rm = TRUE, finite = TRUE)
range(antechinusdata$Latitude, na.rm = TRUE, finite = TRUE)

base = get_map(location=c(145,-37,155,-25), zoom=7, maptype="terrain-background")

map1 = ggmap(base)
map1

map2 <- 
  map1+
  geom_point(data=antechinusdata, 
             aes(x=Longitude, y=Latitude, fill = Population, shape = Population), color="white", cex=6) +
  scale_fill_manual(values = c("grey", "red", "blue", "darkgreen"), labels=c("?", "stuartii north", "stuartii south", "subtropicus"), name=NULL) +
  scale_shape_manual(values = c(25,24,23,21,22,21), labels=c("?", "stuartii north", "stuartii south", "stuartii neotype", "subtropicus", "subtropicus holotype"), name=NULL) + # define shape/color scales
  labs(x="Longitude", y="Latitude", title="Antechinus distribution map") + # label the axes
  theme_bw() + theme(legend.position="bottom", axis.text = element_text(size = rel(0.75)), legend.key = element_rect(colour = "white"), axis.text.x = element_text(angle=45, vjust=0.5)) # tweak the plot's appearance and legend position

map2
