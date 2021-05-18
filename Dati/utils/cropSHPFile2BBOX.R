
library(maptools)
library(raster)   
library(rgeos)

mid_mapx <- rgdal::readOGR(dsn='../shp/sezioniER.shp',stringsAsFactors = F)
bb = c(11.221439,44.101773,12.290967,44.418104)
outfile = '../shp/sezioniBolognaRavennaCesena.shp'





#mid_mapx <- rgdal::readOGR(dsn='../shp/sezioniER.shp',stringsAsFactors = F)
#bb = c(10.519588,44.454968,11.761396,44.951228)
#outfile = '../shp/sezioniMoQuakeArea.shp'

#mid_mapx <- rgdal::readOGR(dsn='../shp/sezioniMarche.shp',stringsAsFactors = F)
#bb = c(13.001457, 42.992867, 13.941032, 43.661072)
#outfile = '../shp/sezioniMarcheQuakeArea.shp'


#mid_mapx <- rgdal::readOGR(dsn='../shp/Comuni2016/comuni2016.shp',stringsAsFactors = F)
#bb = c(11.8333,42.1554,14.4067,43.7679)
#outfile = '../shp/ComuniCentroItalia.shp'



CP <- as(extent(bb[1], bb[3], bb[2], bb[4]), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(mid_mapx))

plot(CP, axes = TRUE)

z <- gIntersection(mid_mapx, CP, byid=TRUE)
rnames = lapply(z@polygons,function(zi){print(zi@ID)})
z = SpatialPolygons(z@polygons)
attr <- data.frame(val=rep(0,length(z)))
rownames(attr) = rnames
z = SpatialPolygonsDataFrame(z,attr,match.ID = TRUE)


writePolyShape(z,outfile)

