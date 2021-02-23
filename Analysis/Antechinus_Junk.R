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
antechinusdata <- read.csv("C:/Users/pietro/Desktop/Pietro/Projects/Viacavaetal_Antechinus/Data/Antechinus_data.csv", header=T)


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



``` {r variance of each landmark}
library(matrixStats)

gpaallmshape <- mshape(gpaall$coords)

lmvector <- coordinates.difference(gpaall$coords, gpaallmshape, type = "vector")

lmvectorarr <- simplify2array(lmvector)

lmvectorlength <- lmvectorarr[,1,]



lmdf <- data.frame((lmvectorlength), row.names = c(1:412))

lmvars <- rowVars(as.matrix(lmdf[1:412,]))

plot(c(1:412), lmvars[1:412])


lmvarsbplot <- ggplot(lmvars, aes(x=reorder(rownames(lmvars)[1:10], lmvars[,1:10], FUN = median), y = lmvars[,1:10])) +
  geom_boxplot()

plot(lmvarsbplot)


```


```{r}

plotAllSpecimens(gpaall$coords, plot.param = list(pt.cex=0.3))





```

MR3.pts<-A_reorder[,,"MR3"]
open3d()
spheres3d(MR3.pts,radius = 0.2,col = 1)
text3d(MR3.pts, texts = c(1:82), asp = FALSE, cex = 2, col="blue", pos = 3)


MRG4.pts<-A_reorder[,,"MRG4"]
open3d()
spheres3d(MRG4.pts,radius = 0.2,col = 1)
text3d(MRG4.pts, texts = c(1:82), asp = FALSE, cex = 2, col="blue", pos = 3)



plot3d(PCAante$pc.scores[,1], PCAante$pc.scores[,2], PCAante$pc.scores[,3], col = cols_dat, type = "s", radius = .002, xlab="Principal Component 1 (19.92%)", ylab="Principal Component 2 (8.42%)", zlab = "Principal Component 3 (6.2%)", ann = FALSE, axes = FALSE)
box3d()


#PCA sex

PCAantesex <- gm.prcomp(gdfantesex$coords)
summary(PCAantesex)

Sex <- gdfantesex$Sex
dataPCAsex <- data.frame(PCAantesex$x[,1], PCAantesex$x[,2], Sex)

plotPCAsex <- ggplot(dataPCAsex, aes(x=-PCAantesex.x...1., y=PCAantesex.x...2., colour = Sex, shape = Sex)) +
  labs(x = "PC 1 (19.06%)", y = "PC 2 (11.05%)") +
  scale_color_manual(values = c("yellow4", "purple4")) +
  geom_point(size = 8, alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 25), legend.title = element_text(size = 25), axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), legend.justification = "center")

plot(plotPCAsex)


#Are Sexes different in shape, adjusting for population? 

fit.shapesexadjpop <- procD.lm(coords~Population+Sex, data = gdfantesex, effect.type = "F")
summary(fit.shapesexadjpop)

#Are Sexes intrinsically different in shape by population? 

fit.shapesexbypop <- procD.lm(coords~Population*Sex, data = gdfantesex, effect.type = "F")
summary(fit.shapesexbypop)

#PCA on pops

PCAantepop <- gm.prcomp(gdfantepop$coords)
summary(PCAantepop)

Populationpop <- gdfantepop$Population
Sexpop <- gdfantepop$Sex
dataPCApop <- data.frame(PCAantepop$x[,1], PCAantepop$x[,2], Populationpop, Sexpop)

plotPCApop <- ggplot(dataPCApop, aes(x=PCAantepop.x...1., y=PCAantepop.x...2., colour = Populationpop)) +
  labs(x = "PC 1 (19.43%)", y = "PC 2 (10.71%)") +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  geom_point(size = 8, alpha = 0.8) +
  stat_ellipse(aes(x=PCAantepop.x...1., y=PCAantepop.x...2., colour = Populationpop), level = 0.95) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 25), legend.title = element_text(size = 25), axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), legend.justification = "center")

plot(plotPCApop)

#check how much size is accounted for in shape PCA

summary(lm(gdfantepop$Csize~PCAantepop$x[,1]))
summary(lm(gdfantepop$Csize~PCAantepop$x[,2]))
summary(lm(gdfantepop$Csize~PCAantepop$x[,3]))


#shape changes associated with allometry 

predshapesize <- shape.predictor(gpaallpop$coords, x=gpaallpop$Csize, Intercept = TRUE, predmin = min(gpaallpop$Csize), predmax = max(gpaallpop$Csize))

allomshapediff <- coordinates.difference(predshapesize$predmin, predshapesize$predmax, type = "spherical")
procrustes.var.plot(predshapesize$predmin, predshapesize$predmax, col = list(grDevices::heat.colors, "black"), allomshapediff[[1]][,"radius"], col.range = c(0, 0.015), pt.size = 0.5)

which(gpaallpop$Csize== max(gpaallpop$Csize))
which(gpaallpop$Csize== min(gpaallpop$Csize))

procrustes.var.plot(gpaallpop$coords[,,which(gpaallpop$Csize== min(gpaallpop$Csize))], gpaallpop$coords[,,which(gpaallpop$Csize== max(gpaallpop$Csize))], col = list(grDevices::heat.colors, "black"), allomshapediff[[1]][,"radius"], col.range = c(0, 0.01), pt.size = 0.5)

tps3dallommin <- tps3d(G34.ply, A_reorder[,,which(antechinusdata$ID=="G34")], predshapesize$predmin)
tps3dallommax <- tps3d(G34.ply, A_reorder[,,which(antechinusdata$ID=="G34")], predshapesize$predmax)

warpmovie3d(tps3dallommin, tps3dallommax, n=15, col = "grey", palindrome = TRUE,
            folder = "C:/Users/pietro/Desktop/Pietro/Projects/Viacavaetal_Antechinus/Figures/warp3dmovies", movie = "warpmovieallom", add = FALSE, close = TRUE,
            countbegin = 0, ask = TRUE, radius = NULL, xland = NULL,
            yland = NULL, lmcol = "black")



#shape changes associated with precipitation seasonality

predshapeppseas <- shape.predictor(gdfante$coords, x=gdfante$ppseasworldclim, Intercept = TRUE, predmin = min(gdfante$ppseasworldclim), predmax = max(gdfante$ppseasworldclim))

ppseasshapediff <- coordinates.difference(predshapeppseas$predmin, predshapeppseas$predmax, type = "spherical")
procrustes.var.plot(predshapeppseas$predmin, predshapeppseas$predmax, col = list(grDevices::heat.colors, "black"), ppseasshapediff[[1]][,"radius"], col.range = c(0, 0.015), pt.size = 0.5)


#shape changes associated with temperature 

predshapetemp <- shape.predictor(gdfante$coords, x=gdfante$tempworldclim, Intercept = TRUE, predmin = min(gdfante$tempworldclim), predmax = max(gdfante$tempworldclim))

tempshapediff <- coordinates.difference(predshapetemp$predmin, predshapetemp$predmax, type = "spherical")
procrustes.var.plot(predshapetemp$predmin, predshapetemp$predmax, col = list(grDevices::heat.colors, "black"), tempshapediff[[1]][,"radius"], col.range = c(0, 0.015), pt.size = 0.5)


#shape changes associated with XXX

predshapeLongitude <- shape.predictor(gdfante$coords, x=gdfante$Longitude, Intercept = TRUE, predmin = min(gdfante$Longitude), predmax = max(gdfante$Longitude))

Longitudeshapediff <- coordinates.difference(predshapeLongitude$predmin, predshapeLongitude$predmax, type = "spherical")
procrustes.var.plot(predshapeLongitude$predmin, predshapeLongitude$predmax, col = list(grDevices::heat.colors, "black"), Longitudeshapediff[[1]][,"radius"], col.range = c(0, 0.015), pt.size = 0.5)

#heatmaps of sex shape differences

femalecoords <- gdfantesex$coords[,,which(gdfantesex$Sex=="Female")]
mshapefemale <- mshape(femalecoords)
malecoords <- gdfantesex$coords[,,which(gdfantesex$Sex=="Male")]
mshapemale <- mshape(malecoords)

mshapefemalemale <- coordinates.difference(mshapefemale, mshapemale, type = "spherical")
procrustes.var.plot(mshapefemale, mshapemale, col = list(grDevices::heat.colors, "black"), pt.size = 0.5, col.val = mshapefemalemale[[1]][,"radius"])

G34.ply <- read.ply(file = "C:/Users/pietro/Desktop/Pietro/Projects/Viacavaetal_Antechinus/Data/3Dmodels/AntechinusAll3Dmodels/G34.ply")

femalewarp <- warpRefMesh(G34.ply, mesh.coord = gdfantesex$coords[,,which(gdfantesex$ID=="G34")], ref = mshapefemale)

malewarp <- warpRefMesh(G34.ply, mesh.coord = gdfantesex$coords[,,which(gdfantesex$ID=="G34")], ref = mshapemale)


#northwarp <- warpRefMesh(G34.ply, mesh.coord = gdfantepop$coords[,,which(gdfantepop$ID=="G34")], ref = mshapenorth)
#rgl.viewpoint(10, 60, fov = 0, zoom = 0.5)
#rgl.snapshot("C:/Users/pietro/Desktop/Pietro/Projects/Viacavaetal_Antechinus/Figures/mshapenorth", fmt = "png")


#shape driven by PC1

mshapePC1minmax<-coordinates.difference(PCAantepop$pc.shapes$PC1min,PCAantepop$pc.shapes$PC1max, type = "spherical")

procrustes.var.plot(PCAantepop$pc.shapes$PC1min, PCAantepop$pc.shapes$PC1max, col = list(grDevices::heat.colors, "black"), col.val = mshapePC1minmax[[1]][,"radius"], col.range = c(0, 0.015), pt.size = 0.5, magnitude = 1)

#warp PC1min vs PC1max

tps3dPC1minmax <- tps3d(G34.ply, A_reorder[,,which(antechinusdata$ID=="G34")], PCAantepop$pc.shapes$PC1min)
tps3dPC1maxmin <- tps3d(G34.ply, A_reorder[,,which(antechinusdata$ID=="G34")], PCAantepop$pc.shapes$PC1max)

warpmovie3d(tps3dPC1minmax, tps3dPC1maxmin, n=15, col = "grey", palindrome = TRUE,
            folder = "C:/Users/pietro/Desktop/Pietro/Projects/Viacavaetal_Antechinus/Figures/warp3dmovies", movie = "tps3dPC1", add = FALSE, close = TRUE,
            countbegin = 0, ask = TRUE, radius = NULL, xland = NULL,
            yland = NULL, lmcol = "black")

#shape driven by PC2

mshapePC2minmax<-coordinates.difference(PCAantepop$pc.shapes$PC2min,PCAantepop$pc.shapes$PC2max, type = "spherical")

procrustes.var.plot(PCAantepop$pc.shapes$PC2min, PCAantepop$pc.shapes$PC2max, col = list(grDevices::heat.colors, "black"), col.val = mshapePC2minmax[[1]][,"radius"], col.range = c(0, 0.015), pt.size = 0.5)

#warp PC2min vs PC2max

tps3dPC2minmax <- tps3d(G34.ply, A_reorder[,,which(antechinusdata$ID=="G34")], PCAantepop$pc.shapes$PC2min)
tps3dPC2maxmin <- tps3d(G34.ply, A_reorder[,,which(antechinusdata$ID=="G34")], PCAantepop$pc.shapes$PC2max)

warpmovie3d(tps3dPC2minmax, tps3dPC2maxmin, n=15, col = "grey", palindrome = TRUE,
            folder = "C:/Users/pietro/Desktop/Pietro/Projects/Viacavaetal_Antechinus/Figures/warp3dmovies", movie = "tps3dPC2", add = FALSE, close = TRUE,
            countbegin = 0, ask = TRUE, radius = NULL, xland = NULL,
            yland = NULL, lmcol = "black")

fitshapedistwatermain <- procD.lm(coords~distanceTowater, data = gdfantepop, effect.type = "F")
summary(fitshapedistwatermain)

lmsizedistwatermain <- lm(gdfantepop$Csize~gdfantepop$distanceTowater)
anova(lmsizedistwatermain)
summary(lmsizedistwatermain)

``` {r analyses by sex, include = FALSE}


#Here we extract only males and only females to analyze populations by sex


A_reorderpopmale <- A_reorderpop[,,-c(which(antechinusdatapop$Sex=="Unknown"), which(antechinusdatapop$Sex=="Female"))]

A_reorderpopfemale <- A_reorderpop[,,-c(which(antechinusdatapop$Sex=="Unknown"), which(antechinusdatapop$Sex=="Male"))]


antechinusdatapopmale <- antechinusdatapop[-c(which(antechinusdatapop$Sex=="Unknown"), which(antechinusdatapop$Sex=="Female")),]
antechinusdatapopmale <- droplevels(antechinusdatapopmale)

antechinusdatapopfemale <- antechinusdatapop[-c(which(antechinusdatapop$Sex=="Unknown"), which(antechinusdatapop$Sex=="Male")),]
antechinusdatapopfemale <- droplevels(antechinusdatapopfemale)


gpamale<-gpagen(A_reorderpopmale, curves = NULL, surfaces = NULL, PrinAxes = TRUE,
                max.iter = NULL, ProcD = TRUE, Proj = TRUE, print.progress = TRUE)

gpafemale<-gpagen(A_reorderpopfemale, curves = NULL, surfaces = NULL, PrinAxes = TRUE,
                  max.iter = NULL, ProcD = TRUE, Proj = TRUE, print.progress = TRUE)



#frame data for geomorph (MALES)

gdfantemale <- geomorph.data.frame(coords = gpamale$coords, Csize = gpamale$Csize, Population = antechinusdatapopmale$Population, ID = antechinusdatapopmale$ID, Sex = antechinusdatapopmale$Sex, Latitude = antechinusdatapopmale$Latitude, Longitude = antechinusdatapopmale$Longitude) 

gdfantefemale <- geomorph.data.frame(coords = gpafemale$coords, Csize = gpafemale$Csize, Population = antechinusdatapopfemale$Population, ID = antechinusdatapopfemale$ID, Sex = antechinusdatapopfemale$Sex, Latitude = antechinusdatapopfemale$Latitude, Longitude = antechinusdatapopfemale$Longitude) 

PCAantemale <- plotTangentSpace(gdfantemale$coords, warpgrids = FALSE, mesh = FALSE, groups = gdfantemale$Population, legend = gdfantemale$Population)

summary(PCAantemale)



Populationmale <- gdfantemale$Population
dataPCAmale <- data.frame(PCAantemale$pc.scores[,1], PCAantemale$pc.scores[,2], Populationmale)

plotPCAmale <- ggplot(dataPCAmale, aes(x=PCAantemale.pc.scores...1., y=PCAantemale.pc.scores...2., colour = Populationmale)) +
  labs(x = "PC 1 (19.56%)", y = "PC 2 (9.56%)") +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  geom_point(size = 8, alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 25), legend.title = element_text(size = 25), axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), legend.justification = "center")

plot(plotPCAmale)

#frame data for geomorph (FEMALES)

gdfantefemale <- geomorph.data.frame(coords = gpafemale$coords, Csize = gpafemale$Csize, Population = antechinusdatapopfemale$Population, ID = antechinusdatapopfemale$ID, Sex = antechinusdatapopfemale$Sex, Latitude = antechinusdatapopfemale$Latitude, Longitude = antechinusdatapopfemale$Longitude) 

PCAantefemale <- plotTangentSpace(gdfantefemale$coords, warpgrids = FALSE, mesh = FALSE, groups = gdfantefemale$Population, legend = gdfantefemale$Population)

summary(PCAantefemale)



Populationfemale <- gdfantefemale$Population
dataPCAfemale <- data.frame(PCAantefemale$pc.scores[,1], PCAantefemale$pc.scores[,2], Populationfemale)

plotPCAfemale <- ggplot(dataPCAfemale, aes(x=PCAantefemale.pc.scores...1., y=PCAantefemale.pc.scores...2., colour = Populationfemale)) +
  labs(x = "PC 1 (23.69%)", y = "PC 2 (8.04%)") +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  geom_point(size = 8, alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 25), legend.title = element_text(size = 25), axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), legend.justification = "center")

plot(plotPCAfemale)

```

``` {r residual shape analyses, include = FALSE}

#calculate residual shape

fit.size <- procD.lm(f1 = coords~Csize, data = gdfante, print.progress = FALSE)

main_residuals3D <- arrayspecs(fit.size$residuals, 412, 3)

allom_resplusconsensus <- main_residuals3D + array(gpaall$consensus,dim(main_residuals3D))

allom_resplusconsensuspop <- allom_resplusconsensus[,,-c(which(antechinusdata$Population=="?"), which(antechinusdata$Population=="subtropicus holotype"))]


gparespop<-gpagen(allom_resplusconsensuspop, curves = NULL, surfaces = NULL, PrinAxes = TRUE,
                  max.iter = NULL, ProcD = TRUE, Proj = TRUE, print.progress = TRUE)

gdfresantepop <- geomorph.data.frame(coords = gparespop$coords, Csize = gparespop$Csize, Species = antechinusdatapop$Species, Population = antechinusdatapop$Population, ID = antechinusdatapop$ID, Sex = antechinusdatapop$Sex, Latitude = antechinusdatapop$Latitude, Longitude = antechinusdatapop$Longitude, Altitude = antechinusdatapop$Altitude, Dat_coll = antechinusdatapop$Date.collected, Collection = antechinusdatapop$Collection, ppworldclim = dfwtemppop$Prec, tempworldclim = dfwtemppop$Temp, Age = antechinusdatapop$Age, doubt = antechinusdatapop$doubt, distanceTowater = antechinusdatapop$distanceTowater, elevation = antechinusdatapop$elevation) 

PCAresantepop <- gm.prcomp(gdfresantepop$coords)

summary(PCAresantepop)



Populationpop <- gdfantepop$Population
Sexpop <- gdfantepop$Sex
dataPCArespop <- data.frame(PCAresantepop$x[,1], PCAresantepop$x[,2], Populationpop, Sexpop)

plotPCArespop <- ggplot(dataPCArespop, aes(x=PCAresantepop.x...1., y=PCAresantepop.x...2., colour = Populationpop, shape = Sexpop)) +
  labs(x = "PC 1 (14.74%)", y = "PC 2 (11.26%)") +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  geom_point(size = 8, alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 25), legend.title = element_text(size = 25), axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), legend.justification = "center")

plot(plotPCArespop)

#Are populations different in residual shape?

fitpopres <- procD.lm(coords~Population, data = gdfresantepop, iter = 999, effect.type = "F")
anova(fitpopres)

#yes, which ones are different?

PWpopres <- pairwise(fitpopres, groups = gdfresantepop$Population)
PWpopres.sum <- summary(PWpopres, test.type = "dist", confidence = 0.95, stat.table = FALSE)
PWpopres.sum

#disparity differences on residual shape

disparitypopsres <- morphol.disparity(fitpopres, groups = ~ Population, data = gdfresantepop, iter = 999)
summary(disparitypopsres)

# residual shapes of populations

northrescoords <- gdfresantepop$coords[,,which(gdfresantepop$Population=="north")]
mshapenorthres <- mshape(northrescoords)
southrescoords <- gdfresantepop$coords[,,which(gdfresantepop$Population=="south")]
mshapesouthres <- mshape(southrescoords)
subtropicusrescoords <- gdfresantepop$coords[,,which(gdfresantepop$Population=="subtropicus")]
mshapesubtropicusres <- mshape(subtropicusrescoords)


mshaperessouthnorth <- coordinates.difference(mshapesouthres, mshapenorthres, type = "spherical")
mshaperesnorthsubtropicus <- coordinates.difference(mshapenorthres, mshapesubtropicusres, type = "spherical")
mshaperessouthsubtropicus <- coordinates.difference(mshapesouthres, mshapesubtropicusres, type = "spherical")

procrustes.var.plot(mshapesouthres, mshapenorthres, col = list(grDevices::heat.colors, "black"), pt.size = 0.5, col.val = mshaperessouthnorth[[1]][,"radius"])

procrustes.var.plot(mshapenorthres, mshapesubtropicusres, col = list(grDevices::heat.colors, "black"), pt.size = 0.5, col.val = mshaperesnorthsubtropicus[[1]][,"radius"])

procrustes.var.plot(mshapesouthres, mshapesubtropicusres, col = list(grDevices::heat.colors, "black"), pt.size = 0.5, col.val = mshaperessouthsubtropicus[[1]][,"radius"])


```

dfintlm <- data.frame(inf=(interlmkdist(A_reorder,c(79,81)) + interlmkdist(A_reorder,c(80,82)))/2
                      , mapf=(interlmkdist(A_reorder,c(75,77)) + interlmkdist(A_reorder,c(76,78)))/2
                      , intp=(interlmkdist(A_reorder,c(77,79)) + interlmkdist(A_reorder,c(78,80)))/2
                      , total=interlmkdist(A_reorder,c(1,4)), Population=antechinusdata$Population, ID=antechinusdata$ID)




plotdist1 <- ggplot(dfintlm, aes(x=inf, y=mapf, colour = Population)) +
  labs(x = "inf", y = "mapf") +
  scale_color_manual(values = c("grey", "red", "blue", "darkgreen", "orange")) +
  geom_point(size = 8, alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 25), legend.title = element_text(size = 25), axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), legend.justification = "center")

plot(plotdist1)


plotdist2 <- ggplot(dfintlm, aes(x=inf, y=total, colour = Population)) +
  labs(x = "inf", y = "total") +
  scale_color_manual(values = c("grey", "red", "blue", "darkgreen", "orange")) +
  geom_point(size = 8, alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 25), legend.title = element_text(size = 25), axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), legend.justification = "center")

plot(plotdist2)


plotdist3 <- ggplot(dfintlm, aes(x=mapf, y=intp, colour = Population)) +
  labs(x = "mapf", y = "intp") +
  scale_color_manual(values = c("grey", "red", "blue", "darkgreen", "orange")) +
  geom_point(size = 8, alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 25), legend.title = element_text(size = 25), axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), legend.justification = "center")

plot(plotdist3)

plotdist4 <- ggplot(dfintlm, aes(x=inf, y=intp, colour = Population)) +
  labs(x = "inf", y = "intp") +
  scale_color_manual(values = c("grey", "red", "blue", "darkgreen", "orange")) +
  geom_point(size = 8, alpha = 0.8) +
  theme_bw() +
  stat_ellipse(aes(x=inf, y=intp, colour = Population), level = 0.95) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 25), legend.title = element_text(size = 25), axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), legend.justification = "center")

plot(plotdist4)


