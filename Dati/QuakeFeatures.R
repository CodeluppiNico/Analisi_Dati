

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
library(dtw)

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


plot = F

#file = c('MoQuakeArea_20110516_20110527_NewSpeechCalls_24')
#file = c('MoQuakeArea_20110516_20110527_TrafficErl_24')
#file = c('MoQuakeArea_20150401_00_20150522_00_PLS')
#file = c('sezioniMoQuakeArea_20150401_00_20150522_00_PLS')
#file = c('MoQuakeArea_20160516_20160527_24')

#file = c('MarcheQuakeArea_20160816_20160901_24')
#file = c('MoQuakeArea_20110401_20110417_NewSpeechCalls_24')


file = c('MarcheQuakeArea_20160816_20160901_24')
type_of_norm = 'stl'
type_of_sync = 'maxccf'

quakefs = function(file, type_of_norm, type_of_sync) {
  print(paste(file,' TYPE OF  NORM = ',type_of_norm,'TYPE OF SYNC =',type_of_sync))
  quakef(c(file),file, type_of_norm, type_of_sync)
}


quakef = function(file,ofile, type_of_norm, type_of_sync) {
  
  
  dir = paste('feat/FEAT/',ofile,'_',type_of_norm,'_',type_of_sync,sep='')
  
  if(file.exists(dir)) {
    print(paste(dir,'already there!'))
    return(0)
  }
  
  
  ts_freq = 24


  data = read.csv(paste('dataset/',file[1],'.csv',sep=''),header=T,stringsAsFactors = F)
  #dim(data)
  
  if(length(file) > 1) {
    for(i in 2:length(file)) {
      print(file[i])
      odata = read.csv(paste('dataset/',file[i],'.csv',sep=''),header=T,stringsAsFactors = F)
      data = rbind(data,odata)
    }
  }
  data$rtime = as.POSIXct(fast_strptime(data$time, "%Y%m%d_%H%M"))
  
  #dim(data)
  head(data)
  
  
  
  if(plot) {
    id = filter(data$val==max(data$val,na.rm=T))$base2mid
    id = '11045 1'
    x = ts(filter(data,base2mid==id)$val, freq=ts_freq)
    #x = x / (4*10) * 3
    plot(x,main=id,ylab="num. people",xlab="time (days)")
    
    sum(is.na(x))
    length(x)
    x = na.approx(x)
    plot(x,main=id,ylab="num. people",xlab="time (days)")
  }
  
  
  
  # total activity
  # weekday/weekend
  # day/night
  # sync
  
  ### total activity
  
  total = ungroup(data) %>% dplyr::select(val,assoc_comuni,RER_32) %>% group_by(assoc_comuni,RER_32) %>% summarize(tot = sum(val, na.rm  = T))
  
  
  
  
  ### weekday / weekend -- day / night
  
  data5 = ungroup(data) %>% mutate(weekday = wday(rtime)) %>% dplyr::select(-RER_32)
  data6 = ungroup(data) %>% mutate(hour = hour(rtime)) %>% dplyr::select(-RER_32)
  weekday = data5 %>% filter(weekday %in% 2:6) %>% dplyr::select(val,assoc_comuni) %>% group_by(assoc_comuni) %>% summarize(weekday = sum(val, na.rm  = T))
  weekend = data5 %>% filter(!weekday %in% 2:6) %>% dplyr::select(val,assoc_comuni) %>% group_by(assoc_comuni) %>% summarize(weekend = sum(val, na.rm  = T))
  day = data6 %>% filter(hour > 6 & hour < 22) %>% dplyr::select(val,assoc_comuni) %>% group_by(assoc_comuni) %>% summarize(day = sum(val, na.rm  = T))
  night = data6 %>% filter(hour < 7 | hour > 21) %>% dplyr::select(val,assoc_comuni) %>% group_by(assoc_comuni) %>% summarize(night = sum(val, na.rm  = T))
  
  
  ### sync
  
  data7 = ungroup(data) %>% dplyr::select(-time,-base2mid) %>% group_by(rtime, assoc_comuni,RER_32) %>% summarize(val = sum(val, na.rm  = T))
  
  if(plot) {
    #data7f = filter(data7,rtime >= fast_strptime('20150504_0000', "%Y%m%d_%H%M") & rtime <= fast_strptime('20150515_0000', "%Y%m%d_%H%M"))
    #data7f = filter(data7,RER_32==1)
    data7f = data7
    id = filter(data7f,val==max(data7f$val,na.rm=T))$assoc_comuni
    #id = 'Camerino'
    #x = ts(filter(data7f,assoc_comuni==id)$val, freq=ts_freq)
    x = zoo(filter(data7f,assoc_comuni==id)$val, filter(data7f,assoc_comuni==id)$rtime, freq=ts_freq)
    #x = x / (4*10) * 3
    title = paste(id,year(data7f[1,]$rtime),month(data7f[1,]$rtime),sep='-')
    plot(x,main=title,ylab="people",xlab="time (days)",xaxt ="n")
    tt <- time(x)
    ix <- seq(1, length(tt), by=24) # 24 hours
    fmt <- "%b-%d-%a" # format for axis labels
    labs <- format(tt[ix], fmt)
    axis(side = 1, at = tt[ix], labels = labs,  cex.axis = 0.5)
    #abline(v=as.POSIXct("2016-08-24 05:36:32 UTC"), col="red")
   
    
    
  }
  
  # data 7 va bene se voglio fare la sync a livello di comunui
  # se voglio fare la sync a livello di regioni intermedie, per poter
  # calcolare within e between, devo usare data4 e usare base2mid
  # al posto di assoc_comuni
  
  sample = unique(data7$assoc_comuni)
  sample <- sample[!is.na(sample)]
  
  
  get_allstl = function() {
    allstl = lapply(sample,function(id) {
      #print(id)
      #id = 'Rocca San Casciano'
      fid = filter(data7,assoc_comuni==id)
      fid$val[fid$val == 0] = NA
      x = zoo(fid$val, order.by = fid$rtime)
      s = ts(data.frame(s = x)$s, freq = ts_freq)
      s = na.locf(s,na.rm = F)
      s = na.locf(s,fromLast = T,na.rm = F)
      
      if(length(s) > sum(is.na(s))) {
        s = na.approx(s)
        if (type_of_norm=='stl') {
          stl = stl(s,s.window = 'periodic', t.window = ts_freq)
        }
        if (type_of_norm=='none') {
          stl = s
        }
        if (type_of_norm=='divbymean') {
          stl = s / mean(s,na.rm = T)
        }
        list(id,stl)
      }
    })
    allstl = setNames(allstl,sample)
  }
  
  allstl = get_allstl()
  allstl[sapply(allstl, is.null)] <- NULL
  sample= names(allstl)

  # id1 = 'Modena'
  # t1 = allstl[[id1]][[2]]$time.series[,'trend']
  # plot(t1)
  # id2 = 'Carpi'
  # t2 = allstl[[id2]][[2]]$time.series[,'trend']
  # plot(t2)
  # ccf = ccf(t1,t2,plot=F)
  # max(abs(ccf$acf))
  # abs(ccf$acf[ceiling(length(ccf$acf)/2)])
  # mi.plugin(discretize2d(t1,t2,numBins1 = 5, numBins2 = 5))
  
  count = 0
  sync_matrix = sapply(sample, function(id1) {
    count <<- count +1
    #print(paste(count,"/",length(sample)))
    
    if(type_of_norm=='stl') {
      t1 = allstl[[id1]][[2]]$time.series[,'trend']
    }
    else {
      t1 = allstl[[id1]][[2]]
    }
    sapply(sample, function(id2) {
      
      if(is.null(allstl[[id2]])) {
        return(NA)
      }
      if(type_of_norm=='stl') {
        t2 = allstl[[id2]][[2]]$time.series[,'trend']
      }
      else {
        t2 = allstl[[id2]][[2]]
      }
      
      if(type_of_sync == 'maxccf') {
        ccf = ccf(t1,t2,plot=FALSE)
        return(max(abs(ccf$acf)))
        #return(abs(ccf$acf[ceiling(length(ccf$acf)/2)]))
      }
      if(type_of_sync == 'mi') {
        return(tryCatch(mi.plugin(discretize2d(t1,t2,numBins1 = 5, numBins2 = 5)), error=function(e) NA))
      }
      if(type_of_sync == 'dtw') {
        return(tryCatch(dtw(t1,t2,step.pattern = asymmetric)$distance, error=function(e) NA))
      }
      
    })
  })
  
  
  # Simple test to checl mi.plugin result
  # x = c(1:200)
  # t1 = sin(x/5)
  # t2 = sin(2*x/5)
  # mi.plugin(discretize2d(t1,t2,numBins1 = 5, numBins2 = 5))
  
            
  hquake_regions = intersect(sample, unique(filter(data7, RER_32 == 1)$assoc_comuni))
  no_hquake_regions = intersect(sample, unique(filter(data7, RER_32 == 0)$assoc_comuni))
  
  all_sync = rowMeans(sync_matrix,na.rm = T)
  hquake_sync = rowMeans(sync_matrix[,hquake_regions],na.rm = T)
  nohquake_sync = rowMeans(sync_matrix[,no_hquake_regions],na.rm = T)
  
  sync = data.frame(sample,all_sync,hquake_sync,nohquake_sync,row.names = NULL)
  names(sync) = c('assoc_comuni','all.sync','hquake.sync','nohquake.sync')
  
  
  ### merge all results
  
  all = Reduce(function(x,y) full_join(x,y,by='assoc_comuni',all=T),
               list(total,weekday,weekend,day,night,sync))
  

  
  write.csv(all,dir,quote=F, row.names = F)
  
  
  
  ########################################################
  ########################################################
  ########################################################
  ## draw colorpleth
  
  if(plot) {
    all_map = readShapePoly(region_map_file, IDvar="COMUNE")
    all_map@data$COMUNE = as.character(all_map@data$COMUNE)
    all_map@data = all_map@data %>% left_join(all, by=c('COMUNE'='assoc_comuni'))
    all_map = subset(all_map, !is.na(tot))
    
    #all_map@data$tot = all_map@data$day / all_map@data$night
    all_map@data$tot = all_map@data$weekday / all_map@data$weekend
    
    
    qpal = colorNumeric("Blues",all_map@data$tot,na.color = "#FFFFFF")
    if(output == "html") {
      leaflet(all_map) %>%
        addPolygons(stroke = TRUE, color = qpal(all_map$tot), weight=1, fillOpacity = 0.5, 
                    smoothFactor = 0.5,popup= paste(all_map$COMUNE,'=',all_map$tot)) %>%
        addTiles() %>% 
        #addTiles(urlTemplate="http://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}.png") %>%
        addLegend("bottomright", pal = qpal, values = ~tot, title = "tot. activity",opacity = 1)
    } else {
      #plot(total_map,col= qpal(total_map$tot))
      tmap = tidy(all_map)
      tmap = tmap %>% inner_join(all, by=c('id'='assoc_comuni'))
      bbox = bbox(mid_map)#data.frame(10.886880,44.617892,10.953896,44.657667)
      names(bbox) = c("ll.lon","ll.lat","ur.lon","ur.lat")
      amap = get_map(location = all_map@bbox, source="stamen", maptype="toner",color="bw",crop=TRUE)
      ggmap(amap, extent = "normal", maprange = FALSE) +
        #ggplot()+
        geom_polygon(data = tmap,
                     aes(long, lat, group = group, fill=tot),
                     colour = "black", alpha = 0.5) +
        theme_bw() +
        scale_fill_distiller(palette='Blues',direction=1)+
        coord_map(projection="mercator",
                  xlim=c(attr(amap, "bb")$ll.lon, attr(amap, "bb")$ur.lon),
                  ylim=c(attr(amap, "bb")$ll.lat, attr(amap, "bb")$ur.lat)) 
    }
  }
  
  beep()
}

#type_of_norm = 'none|stl|divbymean'
#type_of_sync = 'maxccf|mi|dtw' 
 

norm_sync_m = matrix(c('none','maxccf',
                       'stl','maxccf',
                       'divbymean','mi',
                       'divbymean','dtw'), ncol=2, byrow=T)

norm_sync_m = matrix(c('divbymean','maxccf'), ncol=2, byrow=T)


for(i in seq(1,nrow(norm_sync_m))) {
  type_of_norm = norm_sync_m[i,][1]
  type_of_sync = norm_sync_m[i,][2]
  
  # 2016 AGOSTO MARCHE
  quakefs('MarcheQuakeArea_20160816_20160901_24',type_of_norm,type_of_sync)
  
  # 2017 LUME
  quakefs('BolognaRavennaCesena_20170516_20170527_LUME_24',type_of_norm,type_of_sync)
  quakefs('BolognaRavennaCesena_20170416_20170427_LUME_24',type_of_norm,type_of_sync)
  quakefs('MoQuakeArea_20170516_20170527_LUME_24',type_of_norm,type_of_sync)
  quakefs('MoQuakeArea_20170416_20170427_LUME_24',type_of_norm,type_of_sync)
  
  # 2011
  for(place in c('MoQuakeArea','BolognaRavennaCesena')) {
    for (s in c('TrafficErl_24','NewSpeechCalls_24')) {
      for(month in c('04','05','09')) {
        quakefs(paste(place,'_2011',month,'16_2011',month,'27_',s,sep=''),type_of_norm,type_of_sync)
      }
    }
  }
  quakefs('MoQuakeArea_20110426_20110517_NewSpeechCalls_24',type_of_norm,type_of_sync)
  quakefs('MoQuakeArea_20110401_20110417_NewSpeechCalls_24',type_of_norm,type_of_sync)
  
  # 2015,2016,2017 new data
  for(place in c('MoQuakeArea','MarcheQuakeArea','BolognaRavennaCesena')) {
    for(year in c('2015','2016','2017')) {
      for(month in c('04','05','09')) {
        quakefs(paste(place,'_',year,month,'16_',year,month,'27_24',sep=''),type_of_norm,type_of_sync)
      }
    }
  }
  
  # 2015 PLS Data
  for(place in c('MoQuakeArea','BolognaRavennaCesena')) {
    quakefs(paste(place,'_20150401_00_20150522_00_PLS',sep=''),type_of_norm,type_of_sync)
  }
  quakefs('sezioniMoQuakeArea_20150401_00_20150522_00_PLS',type_of_norm,type_of_sync)
  
}