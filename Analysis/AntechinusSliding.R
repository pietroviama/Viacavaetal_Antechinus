
library(Morpho)
library(rgl)
library(Rvcg)
library(geomorph)

setwd('C:/Users/pietro/Desktop/Pietro/Projects/Viacavaetal_Antechinus/Data')

## import meshes and points cloud


subtropicus.holo<-file2mesh('3Dmodels/Asubtropicus/J17407.ply',readcol=F,clean=T)
subtropicus.p<-t(read.csv('LMcoordstest.csv', header = TRUE))
dim(subtropicus.p)
A <- arrayspecs(subtropicus.p, 412, 3)
dim(A)
Ahead <- head(dimnames(A))[3]
template.p<-A[,,2]
shade3d(subtropicus.holo,col=8)
spheres3d(template.p[145:227,],radius=0.3,col=1)#fixed. nb of landmark = (z-1)/3
spheres3d(template.p[228:412,],radius=0.2,col=2) #curves
spheres3d(template.p[1:144,],radius=0.1,col=3) #patches


### 2nd step build symmetric patches, you can do this or not, it depends on your question.
## don't run now

#rpm<-c(1:27)
#lpm<-c(82:108)
#pm<-cbind(rpm,lpm)
#rfp<-c(28:50)
#lfp<-c(115:137)
#fp<-cbind(rfp,lfp)
#rn<-c(64:72)
#ln<-c(73:81)
#n<-cbind(rn,ln)

#sepe, oc and bo to do symmetrize

#symme1<-Morpho::symmetrize(c(template.p[1:27,], template.p[82:108]),pm)#128
#symme2<-Morpho::symmetrize(template.p[373:422,],fp)#50
#symme3<-Morpho::symmetrize(template.p[423:472,],n)#50

#reprojecting the points to the surface
#proji<-projRead(symme,pilio)
#proji1<-projRead(symme1,pilio)
#proji2<-projRead(symme2,pilio)
#proji3<-projRead(symme3,pilio)



#sym.patch<-t(proji$vb)[,-4]
#sym.patch1<-t(proji1$vb)[,-4]
#sym.patch2<-t(proji2$vb)[,-4]
#sym.patch3<-t(proji3$vb)[,-4]


## new set of points with symmetrically places points

#pilio.new<-rbind(pilio.p[1:40,],pilio.curves,pilio.p[117:522,])#sym.patch,sym.patch1,sym.patch2,sym.patch3,sym.patch4,sym.patch5)

#shade3d(pilio,col=8)
#spheres3d(pilio.new[505:522,],radius=0.5,col=1)
#text3d(pilio.new[505:522,1],pilio.new[505:522,2],pilio.new[505:522,3],c(505:522),cex=1.5,adj =1.5)

#spheres3d(pilio.new[41:116,],radius=0.5,col=2)
#spheres3d(pilio.new[117:522,],radius=0.5,col=3)

## read all points
#export in pts

#setwd("/Users/colonnello/Desktop/New Brain/points")
#nm<-list.files(path="/Users/colonnello/Desktop/New Brain/points")
#list.pts<-do.call(list,lapply(nm,function(x) read.pts(x)))

#try.arr<-array(unlist(list.pts),dim=c(40,3,9))
#40 = lms , 3 dimensions and 9 specimens
## all the meshes 
#setwd("/Users/colonnello/Desktop/New Brain/prova")
#nm2<-list.files(path="/Users/colonnello/Desktop/New Brain/prova")
#listish<-do.call(list,lapply(nm2,function(x) file2mesh(x,readcol=F,clean=T)))

## match names, very important

#nmi<-substr(nm2,1,nchar(nm2)-4)

#dimnames(try.arr)[[3]]<-nmi


#### 3rd step, create the template

fixi<-pilio.new[1:40,]
patchi<-pilio.new[41:522,]
rownames(patchi)<-c(1:482)


template.1<-createAtlas(pilio,landmarks=fixi,patch=patchi,
                        
         patchCurves=list(c(1:10),c(11:20),c(21:27),c(28:30),c(31:35),c(36:40),c(41:43),c(44:50),c(51:55),
                          c(56:58),c(59:63),c(64:68),c(69:71),c(72:76),
                                         
                            c(77:140),c(141:204),c(205:268),c(269:331),c(332:356),
                  c(357:381),c(382:406),c(407:431),c(432:447),c(448:463),c(464:472),c(473:482)))


plotAtlas(template.1)


### 4th step apply the template to all the other specimens, remember to play with inflate/deflate

speci<-placePatch(template.1,try.arr,path="/Users/colonnello/Desktop/New Brain/prova",
                  inflate=1)

### look at the results

mfrow3d(4,3,sharedMouse = T)

shade3d(pilio,col=8)
spheres3d(pilio.new[1:40,],radius=0.35,col=1)
spheres3d(pilio.new[41:116,],radius=0.35,col=2)
spheres3d(pilio.new[117:522,],radius=0.35,col=3)

for(i in 1:dim(speci)[[3]]){
  
  next3d()
  shade3d(listish[[i]],col=8)
  spheres3d(speci[1:40,,i],radius=0.35,col=1)
  spheres3d(speci[41:116,,i],radius=0.35,col=2)
  spheres3d(speci[117:522,,i],radius=0.35,col=3)
  
}


### a few curves may present distortion, so make them equidistant again 
### (can use Bezier, but result are similar)

equi<-matrix(NA,ncol =3)
disto<-NULL
for(i in 1:dim(speci)[[3]]){
  ccc<-speci[51:60,,i]
  disto[i]<-list(equidistantCurve(ccc,iterations=10,increment=6,smoothit=4,mesh=listish[[i]]))
  equi<-array(as.numeric(unlist(disto)),dim=dim(speci[51:60,,]))
}


equi2<-matrix(NA,ncol =3)
disto.1<-NULL
for(i in 1:dim(speci)[[3]]){
  ccc1<-speci[41:50,,i]
  disto.1[i]<-list(equidistantCurve(ccc1,iterations=10,increment=8,smoothit=6,mesh=listish[[i]]))
  equi2<-array(as.numeric(unlist(disto.1)),dim=dim(speci[41:50,,]))
}



new.specis<-bindArr(speci[1:40,,],equi2,equi,speci[61:522,,],along=1)


listo<-listish[c(2,8,6)]
speco<-new.specis[,,c(2,8,6)]


mfrow3d(2,2,sharedMouse = T)

shade3d(pilio,col=8)
spheres3d(pilio.new[1:40,],radius=0.35,col=1)
spheres3d(pilio.new[41:116,],radius=0.35,col=2)
spheres3d(pilio.new[117:522,],radius=0.35,col=3)

for(i in 1:dim(speco)[[3]]){
  
  next3d()
  shade3d(listo[[i]],col=8)
  spheres3d(speco[1:40,,i],radius=0.5,col=1)
  spheres3d(speco[41:116,,i],radius=0.5,col=2)
  spheres3d(speco[117:522,,i],radius=0.5,col=3)
  
}





### Final, sliding process

setwd('/Users/colonnello/Desktop/New Brain')

fix<-c(1:40)

outlinesi<-list(c(41:50),c(51:60),c(61:67),c(68:70),c(71:75),c(76:80),c(81:83),c(84:88),c(89:95),
                c(96:98),c(99:103),c(104:108),c(109:111),c(112:116))

surfi<-c(117:522)

slidi<-slider3d(new.specis, SMvector = fix, deselect = T, surp=surfi, meshlist = listish, outlines=outlinesi )

slids<-slidi$dataslide

plot(slidi)



sli<-slids[,,c(2,8,6)]


mfrow3d(2,2,sharedMouse = T)

shade3d(pilio,col=8)
spheres3d(pilio.new[1:40,],radius=0.35,col=1)
spheres3d(pilio.new[41:116,],radius=0.35,col=2)
spheres3d(pilio.new[117:522,],radius=0.35,col=3)

for(i in 1:dim(speco)[[3]]){
  
  next3d()
  shade3d(listo[[i]],col=8)
  spheres3d(sli[1:40,,i],radius=0.5,col=1)
  spheres3d(sli[41:116,,i],radius=0.5,col=2)
  spheres3d(sli[117:522,,i],radius=0.5,col=3)
  
}





