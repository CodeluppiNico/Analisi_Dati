

# serie temporali
library(lubridate)
library(zoo)
library(tseries)

# operazioni su data-frame
library(dplyr)
library(tidyr)
library(broom)

# machine learning (modelli per le serie temporali, e computazione multicore)
library(forecast)
library(parallel)
library(vars)
library(entropy)

# visualizzazione
library(ggplot2)
library(ggmap)
library(corrplot)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(beepr)
###############################################

#ts_dir = 'timeseries/PLS/'
#file = 'sezioniMoQuakeArea_20150401_00_20150522_00'

# nota che non c'è il base_map perchè la timeseries 
# è direttamente espressa in termini di sezioni (midmap)

#mid_map_file = 'shp/sezioniMoQuakeArea.shp'
#region_map_file = 'shp/Comuni2012/emilia-romagna.shp'
#crateri_file = 'other_data/crateriEmilia.csv'



quake_datasetPLS = function(ts_dir, file, mid_map_file, region_map_file, crateri_file, ts_freq=24) {
  
  if(file.exists(paste('dataset/',file,'_PLS.csv',sep=''))) {
    print(paste(file,'already there!'))
    return(0)
  }
  
  
  plot = F
  output="html"
  ts_freq = 24
  

  
  
  #######################################################
  # PRIMA AGGREGAZIONE 
  # MAPPIAMO DALLA MAPPA BASE ALLA MAPPA MID
  #######################################################
  
  
  mid_map <- rgdal::readOGR(dsn=mid_map_file,stringsAsFactors = F)
  
  
  #######################################################
  # MAPPIAMO DALLA MAPPA MID ALLA MAPPA REGION
  #######################################################
  
  region_map <- rgdal::readOGR(dsn=region_map_file, stringsAsFactors = F)
  
  
  # if(is.null(output) || output == "html") {
  #   leaflet(region_map) %>%
  #     addPolygons(stroke = TRUE, color = "#000", weight=1, 
  #                 fillOpacity = 0.5, fillColor = "#08F", smoothFactor = 0.5, 
  #                 popup = ~COMUNE) %>%
  #     addTiles() #adds a map tile, the default is OpenStreetMap
  # } else {
  #   plot(region_map)
  # }
  
  proj4string(mid_map) = proj4string(region_map)
  
  pi <- intersect(mid_map, region_map)
  pi$area <- raster::area(pi)
  
  whichMaxComune = function(id) {
    q = filter(pi@data,SP_ID==id)
    x = q[which.max(q$area),]$COMUNE
    if(length(x) == 0) {
      x = NA
    }
    return(x)
  }
  
  #whichMaxComune('180-330')
  mid_map$assoc_comuni = sapply(mid_map$SP_ID,whichMaxComune)
  mid_map = subset(mid_map,!is.na(assoc_comuni))
  
  if(plot) {
    factpal <- colorFactor(rainbow(20), mid_map@data$assoc_comuni)
    if(is.null(output) || output == "html") {
      leaflet(mid_map) %>%
        addPolygons(stroke = FALSE, color = "#000", weight=1, 
                    fillOpacity = 0.5, fillColor = ~factpal(assoc_comuni), smoothFactor = 0.5,
                    popup = ~paste(SP_ID,assoc_comuni)) %>%
        addTiles() #adds a map tile, the default is OpenStreetMap
    } else {
      plot(mid_map)
    }
  }
  
  ###############################################################
  
  # guardo i file dei crateri del terremoto
  # faccio il join con i mid_map per avere anche quella
  # informazione
  
  crateri = read.csv(crateri_file,sep=';', header=TRUE,stringsAsFactors=FALSE)
  if(names(crateri)[1] == 'Provincia') {
    # Nome;Prov;Codice_Istat;RER_32;RER_53;RER_58
    names(crateri) = c('Prov','Nome','RER_32')
    
  }
  
  mid_map@data = mid_map@data %>% left_join(crateri, by=c('assoc_comuni'='Nome'))
  mid_map@data$RER_32[is.na(mid_map@data$RER_32)] = 0
  
  if(plot) {
    color = c('#0f0','#f00')
    if(is.null(output) || output == "html") {
      leaflet(mid_map) %>%
        addPolygons(stroke = TRUE, color = "#000", weight=1, 
                    fillOpacity = 0.5, fillColor = color[mid_map@data$RER_32+1], smoothFactor = 0.5,
                    popup = ~assoc_comuni) %>%
        addTiles() #adds a map tile, the default is OpenStreetMap
    } else {
      plot(mid_map)
    }
    
    test_map = region_map
    test_map@data = test_map@data %>% left_join(crateri, by=c('COMUNE'='Nome'))
    color = c('#0f0','#f00')
    if(is.null(output) || output == "html") {
      leaflet(test_map) %>%
        addPolygons(stroke = TRUE, color = "#000", weight=1, 
                    fillOpacity = 0.5, fillColor = color[test_map@data$RER_32+1], smoothFactor = 0.5,
                    popup = ~COMUNE) %>%
        addTiles() #adds a map tile, the default is OpenStreetMap
    } else {
      plot(mid_map)
    }
  }
  ##############################################################
  ##############################################################
  ##############################################################
  
  # leggo il file con le serie temporali
  
  data = read.csv(paste(ts_dir,file,sep='/'),header=FALSE,stringsAsFactors=FALSE, na.strings = c("65535","65535.0"))
  names(data) = c("time","cell","value")
  data$cell = as.character(data$cell)
  data$time = as.character(data$time)
  data$rtime = as.POSIXct(fast_strptime(data$time, "%Y%m%d_%H%M"))
  head(data)
  
  datai = data %>%
    group_by(cell) %>%
    #mutate(valueI = na.approx(value, na.rm=FALSE, method='constant', rule=2))
    mutate(valueI = ifelse(is.na(value),mean(value, na.rm=T),value))
  datai$value = datai$valueI

  datai = datai %>% dplyr::select(-valueI)
  
  cellsWithNa = unique(filter(datai,is.na(value))$cell)
  datai = filter(datai, !(cell %in% cellsWithNa))
  
  data = ungroup(datai)

  if(plot) {
    id = filter(data,value==max(data$value,na.rm=T))$cell
    x = ts(filter(datai,cell==id)$value, freq=ts_freq)
    plot(x,main=id,ylab="num. people",xlab="time (days)")
    
    
    sum(is.na(x))
    length(x)
    x = na.approx(x)
    plot(x,main=id,ylab="num. people",xlab="time (days)")
  }
  # join data with base_map@data 
  # per estrarre il legame con mid_map
  
  
  
  data = data %>% left_join(mid_map@data, by=c('cell'='SP_ID')) %>% dplyr::select(-val)
  names(data)[2]='base2mid'
  names(data)[3]='val'
  #extra = mid_map@data %>% dplyr::select(SP_ID,assoc_comuni,RER_32)
  #data = data %>% left_join(extra, by=c('base2mid'='SP_ID','assoc_comuni'='assoc_comuni'))
  
  if(plot) {
    id = filter(data,val==max(data$val,na.rm=T))$base2mid
    #id = '70 1'
    x = ts(filter(data,base2mid==id)$val, freq=ts_freq)
    x = x / 10 * 3
    plot(x,main=id,ylab="num. people",xlab="time (days)")
    
    sum(is.na(x))
    length(x)
    x = na.approx(x)
    plot(x,main=id,ylab="num. people",xlab="time (days)")
  }
  
  
  
  write.csv(data,paste('dataset/',file,'_PLS.csv',sep=''),quote=F, row.names = F)
  beep()
}

dir = '\\\\mamei-nas/DATASET'

quake_datasetPLS(paste(dir,'/timeseries/PLS/',sep=''),'sezioniMoQuakeArea_20150401_00_20150522_00','shp/sezioniMoQuakeArea.shp','shp/Comuni2012/emilia-romagna.shp','other_data/crateriEmilia.csv')

