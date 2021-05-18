
library(ggplot2)
library(ggmap)
library(rgdal)
library(sp)

library(maptools)
library(leaflet)


dir = '../shp/'

## Create a SHP file
scalef = 1

# This is good for LUME data
suffix = "LUME"
hdr = readLines("Nrealtime_Emilia-Romagna_15_20170816_0845.hdr")

# This is good for old TIM data
#suffix = "ERL"
#hdr = readLines("Emilia_CityLive_TrafficErl.hdr")

# This is good for new TIM data (Emilia)
#suffix = "EMILIA"
#hdr = readLines("new-data-emilia.hdr")

# This is good for new TIM data (Centro Italia)
#suffix = "CENTRO_IT"
#hdr = readLines("new-data-centro-it.hdr")



#shpFile = paste(dir,'Parma',scalef,suffix,'.shp',sep='')
#bbox = data.frame(10.217622,44.749180,10.446793,44.849781)

#shpFile = paste(dir,'Ancona',scalef,suffix,'.shp',sep='')
#bbox = data.frame(13.444855,43.560216,13.670762,43.656172)

#shpFile = paste(dir,'CentroITQuakeArea',scalef,suffix,'.shp',sep='')
#bbox = data.frame(12.591332,42.561883,14.294887,43.616290)

#shpFile = paste(dir,'MarcheQuakeArea',scalef,suffix,'.shp',sep='')
#bbox = data.frame(13.001457, 42.992867, 13.941032, 43.661072)

shpFile = paste(dir,'BolognaRavennaCesena',scalef,suffix,'.shp',sep='')
bbox = data.frame(11.221439,44.101773,12.290967,44.418104)

#shpFile = paste(dir,'MoQuakeArea',scalef,suffix,'.shp',sep='')
#bbox = data.frame(10.519588,44.454968,11.761396,44.951228)

#shpFile = paste(dir,'ModenaLG',scalef,suffix,'.shp',sep='')
#bbox = data.frame(10.697413,44.515198,11.055379,44.803501)

#shpFile = "ModenaErlang.shp"
#bbox = data.frame(10.886880,44.617892,10.953896,44.657667)

#shpFile = "BLQ.shp"
#bbox = data.frame(11.281752,44.520770,11.309602, 44.536488)







hdr = strsplit(hdr,split=" ",fixed = TRUE)
hdr = as.data.frame(hdr,stringsAsFactors = FALSE)
names(hdr) = tolower(hdr[1,])
hdr = hdr[2,]

ulxmap = as.numeric(hdr$ulxmap)
ulymap = as.numeric(hdr$ulymap)
xdim = as.numeric(hdr$xdim)
ydim = as.numeric(hdr$ydim)


xdim = xdim * scalef
ydim = ydim * scalef



names(bbox) = c("ll.lon","ll.lat","ur.lon","ur.lat")


minj = floor((bbox$ll.lon - ulxmap + xdim/2)/xdim)
maxi = ceiling((ulymap - bbox$ll.lat + ydim/2)/ydim)
maxj = ceiling((bbox$ur.lon - ulxmap + xdim/2)/xdim)
mini = floor((ulymap - bbox$ur.lat + ydim/2)/ydim)

nrows = maxi - mini
ncols = maxj - minj

ox = ulxmap + (minj * xdim)
oy = ulymap - (mini * ydim)


getCellBorder = function(i,j,ox,oy,xdim,ydim) {
  # bottom left corner
  x = ox + (j * xdim) - xdim/2
  y = oy - (i * ydim) - ydim/2
  m = matrix(c(x,y,
               x+xdim,y,
               x+xdim,y+ydim,
               x,y+ydim),
             nrow=4,ncol=2,byrow=TRUE)
  return(Polygons(list(Polygon(m)),ID=paste((mini+i),(minj+j),sep="-")))
  #return(data.frame(id=paste((mini+i),(minj+j),sep="-"),lon=m[,1],lat=m[,2],stringsAsFactors = F))
}

tot = nrows * ncols
z = vector("list",tot)
ii = 1
for(i in 1:nrows) {
  for(j in 1:ncols) {
    z[ii] = getCellBorder(i,j,ox,oy,xdim,ydim)
    ii = ii+1
  }
  #print(i)
}

rnames = lapply(z,function(zi){print(zi@ID)})



z = SpatialPolygons(z)
attr <- data.frame(val=rep(0,length(z)))
rownames(attr) = rnames
z = SpatialPolygonsDataFrame(z,attr,match.ID = TRUE)
proj4string(z) = CRS("+init=epsg:4326")
writePolyShape(z,shpFile)



#z <- readShapePoly(shpFile,IDvar="SP_ID")

#leaflet(z) %>%
#  addPolygons(stroke = TRUE, color = "#000", weight=1, fillOpacity = 0.5, fillColor = "#03F", smoothFactor = 0.5, popup = ~SP_ID) %>%
#  addTiles() #adds a map tile, the default is OpenStreetMap


