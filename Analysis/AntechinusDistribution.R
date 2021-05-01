library(ggplot2)
library(ggmap)
library(maps)
library(gghighlight)


range(antechinusdata$Longitude, na.rm = TRUE, finite = TRUE)
range(antechinusdata$Latitude, na.rm = TRUE, finite = TRUE)

base = get_map(location=c(145,-37,155,-25), zoom=7, maptype="terrain-background")

map1 = ggmap(base)
map1

map2 <- 
  map1+
  geom_point(data = antechinusdata, 
             aes(x=Longitude, y=Latitude, fill = Population, shape = Sex),  cex=6, alpha = 0.9, stroke = 2) +
    scale_shape_manual(values = c(7, 12, 21, 21, 21), breaks = c("A. stuartii neotype", "A. subtropicus holotype"),
                     labels = c((expression(paste(italic("A. stuartii"), " neotype"))),
                                (expression(paste(italic("A. subtropicus"),  " holotype")))),
                     guide = guide_legend(override.aes = list(alpha = 1, size = 6))) +
  scale_fill_manual(values = c("darkgoldenrod2", "deeppink", "darkgreen", "#ced0ce"), breaks = c("A. stuartii south", "A. stuartii north", "A. subtropicus", "Unknown"),
                    labels = c((expression(paste(italic("A. stuartii"), " south"))),
                               (expression(paste(italic("A. stuartii"), " north"))),
                               (expression(paste(italic("A. subtropicus")))),
                               (expression(paste("Unknown"))))) +
  labs(x="Longitude", y="Latitude", fill = "", shape = "") + # label the axes
  theme_bw() + theme(legend.position="bottom", legend.box = "vertical", axis.text = element_text(size = 15), text = element_text(size = 15), legend.key = element_rect(colour = "white"), axis.text.x = element_text(angle=45, vjust=0.5), legend.text = element_text(size = 15)) +
  guides(fill=guide_legend(override.aes=list(shape=21)))# tweak the plot's appearance and legend position

map2


