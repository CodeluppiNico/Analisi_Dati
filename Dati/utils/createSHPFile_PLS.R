
library(ggplot2)
library(ggmap)
library(rgdal)
library(sp)
library(dplyr)
library(maptools)
library(leaflet)
library(geosphere)

dir = '../shp/'
file = 'MoQuakeArea_20150401_00_20150522_00'
#file = 'BolognaRavennaCesena_20150401_00_20150522_00'
shpFile = paste(dir,file,'.shp',sep='')

m = read.csv(paste(dir,file,'.map',sep=''), header=F,stringsAsFactors=F)
m$radius = 10
names(m) = c('id','lng','lat','radius')
m = m[!duplicated(m$id), ]

getCellBorder = function(id,x,y,size=0.001) {
  m = matrix(c(x,y,
               x+size,y,
               x+size,y+size,
               x,y+size),
             nrow=4,ncol=2,byrow=TRUE)
  
  #m = destPoint(c(x,y), seq(0,360,30), size)
  
  return(Polygons(list(Polygon(m)),ID=id))
}


x = c(1:dim(m)[1])
z = lapply(x,function(i) {
  getCellBorder(m[i,]$id,m[i,]$lng,m[i,]$lat)
})

rnames = lapply(z,function(zi){print(zi@ID)})

z = SpatialPolygons(z)
attr <- data.frame(val=rep(0,length(z)))
rownames(attr) = rnames
z = SpatialPolygonsDataFrame(z,attr,match.ID = TRUE)
proj4string(z) = CRS("+init=epsg:4326")
writePolyShape(z,shpFile)

