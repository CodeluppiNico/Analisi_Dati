

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

quakef_sync_s = function(file,type_of_norm, type_of_sync) {
  print(paste(file,' TYPE OF  NORM = ',type_of_norm,'TYPE OF SYNC =',type_of_sync))
  quakef_sync(c(file),file,type_of_norm, type_of_sync)
}


quakef_sync = function(file,ofile,type_of_norm, type_of_sync) {
  
  dir = paste('feat/SYNC/',ofile,'_',type_of_norm,'_',type_of_sync,sep='')
  
  if(file.exists(dir)) {
    print(paste(dir,'already there!'))
    return(0)
  }
  
  ts_freq = 24
  output="html"
  
  data = read.csv(paste('dataset/',file[1],'.csv',sep=''),header=T,stringsAsFactors = F)
  if(length(file) > 1) {
    for(i in 2:length(file)) {
      #print(file[i])
      odata = read.csv(paste('dataset/',file[i],'.csv',sep=''),header=T,stringsAsFactors = F)
      data = rbind(data,odata)
    }
  }
  #dim(data)
  head(data)
  
  ### sync
  # se voglio fare la sync a livello di regioni intermedie, per poter
  # calcolare within e between, devo usare data e usare base2mid
  # al posto di assoc_comuni
  
  hq = unique(filter(data,RER_32==1)$base2mid)
  nhq = unique(filter(data,RER_32==0)$base2mid)
  hq <- hq[!is.na(hq)]
  nhq <- nhq[!is.na(nhq)]
  #sample = uni
  if(!exists('ssample')) {
    ssample <<- c(sample(hq,dim_sample),sample(nhq,dim_sample))
  }
  
  get_allstl = function() {
    allstl = lapply(ssample,function(id) {
      #id = '11625 1'
      # print(id)
      fid = filter(data,base2mid==id)
      fid$val[fid$val == 0] = NA
      x = zoo(fid$val, order.by = fid$rtime)
      if(length(x) > 2*ts_freq & sum(is.na(x)) < length(x) ) {
        s = ts(data.frame(s = x)$s, freq = ts_freq)
        s = na.locf(s,na.rm = F)
        s = na.locf(s,fromLast = T,na.rm = F)
        tryCatch({s = na.approx(s)}, error=function(e) {print(paste('error on:',id))})
        
        
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
    allstl = setNames(allstl,ssample)
  }
  
  allstl = get_allstl()
  allstl[sapply(allstl, is.null)] <- NULL
  ssample= names(allstl)
  
  
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
  sync_matrix = sapply(ssample, function(id1) {
    count <<- count +1
    #print(paste(count,"/",length(ssample)))
    #print(paste('-->',id1))
    if (type_of_norm=='stl') {
      t1 = allstl[[id1]][[2]]$time.series[,'trend']
    }
    else {
      t1 = allstl[[id1]][[2]]
    }
    sapply(ssample, function(id2) {
      if(type_of_norm=='stl') {
        t2 = allstl[[id2]][[2]]$time.series[,'trend']
      }
      else {
        t2 = allstl[[id2]][[2]]
      }
      
      tryCatch({
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
      }, error=function(e) {
        return(NA)
      })
    })
  })
  
  
  # Simple test to checl mi.plugin result
  # x = c(1:200)
  # t1 = sin(x/5)
  # t2 = sin(2*x/5)
  # mi.plugin(discretize2d(t1,t2,numBins1 = 5, numBins2 = 5))
  
  #sample_data = data
  sample_data = filter(data, base2mid %in% ssample)
            
  hquake_cells = unique(filter(sample_data, RER_32 == 1)$base2mid)
  no_hquake_cells = unique(filter(sample_data, RER_32 == 0)$base2mid)
  
  within_hquake_sync = rowMeans(sync_matrix[hquake_cells,hquake_cells],na.rm = T)
  within_nohquake_sync = rowMeans(sync_matrix[no_hquake_cells,no_hquake_cells],na.rm = T)
  between_hquake_sync = rowMeans(sync_matrix[hquake_cells,no_hquake_cells],na.rm = T)
  between_nohquake_sync = rowMeans(sync_matrix[no_hquake_cells,hquake_cells],na.rm = T)
  
  hquake_sync = data.frame(hquake_cells,within_hquake_sync,between_hquake_sync,1, row.names = NULL)
  names(hquake_sync) = c('tract','within_sync','between_sync','RER_32')
  
  no_hquake_sync = data.frame(no_hquake_cells,within_nohquake_sync,between_nohquake_sync,0, row.names = NULL)
  names(no_hquake_sync) = c('tract','within_sync','between_sync','RER_32')
  
  sync = rbind(hquake_sync,no_hquake_sync)
  extra = distinct(data,base2mid,.keep_all = TRUE) %>% dplyr::select(c(base2mid,assoc_comuni))
  sync = left_join(sync,extra,by=c('tract'='base2mid'))
  
  
  
  write.csv(sync,dir,quote=F, row.names = F)
  beep()
}

########################################################


dim_sample = 300
#type_of_norm = 'none|stl|divbymean'
#type_of_sync = 'maxccf|mi|dtw' 


norm_sync_m = matrix(c('none','maxccf',
                       'stl','maxccf',
                       'divbymean','mi',
                       'divbymean','dtw'), ncol=2, byrow=T)


norm_sync_m = matrix(c('divbymean','maxccf'), ncol=2, byrow=T)


#########################################################
## MODENA
if(exists('ssample')) { rm(ssample) }


for(i in seq(1,nrow(norm_sync_m))) {
  type_of_norm = norm_sync_m[i,][1]
  type_of_sync = norm_sync_m[i,][2]
  
  
  # 2011
  for(place in c('MoQuakeArea')) {
    for (s in c('NewSpeechCalls_24')) {
      for(month in c('04','05')) {
        quakef_sync_s(paste(place,'_2011',month,'16_2011',month,'27_',s,sep=''),type_of_norm,type_of_sync)
      }
    }
  }
  quakef_sync_s('MoQuakeArea_20110426_20110517_NewSpeechCalls_24',type_of_norm,type_of_sync)
  quakef_sync_s('MoQuakeArea_20110401_20110417_NewSpeechCalls_24',type_of_norm,type_of_sync)
  
  # 2015 PLS Data
  for(place in c('MoQuakeArea','sezioniMoQuakeArea')) {
    quakef_sync_s(paste(place,'_20150401_00_20150522_00_PLS',sep=''),type_of_norm,type_of_sync)
  }
  
  # 2015,2016,2017 new data
  for(place in c('MoQuakeArea')) {
    for(year in c('2015','2016','2017')) {
      for(month in c('04','05','09')) {
        quakef_sync_s(paste(place,'_',year,month,'16_',year,month,'27_24',sep=''),type_of_norm,type_of_sync)
      }
    }
  }
  # 2017 LUME
  #  for(place in c('MoQuakeArea')) {
  #    for(month in c('04','05')) {
  #      quakef_sync_s(paste(place,'_2017',month,'16_2017',month,'27_LUME_24',sep=''),type_of_norm,type_of_sync)
  #    }
  #  }
  
}

#########################################################
## MARCHE
if(exists('ssample')) { rm(ssample) }


for(i in seq(1,nrow(norm_sync_m))) {
  type_of_norm = norm_sync_m[i,][1]
  type_of_sync = norm_sync_m[i,][2]
  
  # 2015,2016,2017 new data
  for(place in c('MarcheQuakeArea')) {
    for(year in c('2015','2016','2017')) {
      for(month in c('04','05','09')) {
        quakef_sync_s(paste(place,'_',year,month,'16_',year,month,'27_24',sep=''),type_of_norm,type_of_sync)
      }
    }
  }
  
  # 2016 AGOSTO MARCHE
  quakef_sync_s('MarcheQuakeArea_20160816_20160901_24',type_of_norm,type_of_sync)
}  







