# ----- Clear Workspace and Load Required Packages
package.list <- list("dplyr", "ggplot2", "readr", "tidyr", "ggthemes", "readr", "ggmap", "lubridate","maps","reshape2")
source('R_functions/load_packages.R') # clear workspace, clear console, load packages
source("R_functions/auxiliar_functions_geographic.R")
source('R_functions/get_extreme_events_functions.R')

get_portfolio_exceedance_TS<-function(portfolio_name,
                                      portfolio_file, 
                                      climate_data,
                                      event_type, 
                                      threshold_type, 
                                      threshold, 
                                      window,
                                      output_path){
  ##################################################################################################################################
  ## 1. get portfolio information
  ##################################################################################################################################  
  #
  portfolio=read.csv(paste0(portfolio_file, portfolio_name, '.csv'))
  portfolio=data.frame(asset=portfolio$Asset.Name, longitude=portfolio$Longitude, latitude=portfolio$Latitude)
  portfolio=subset(portfolio, portfolio$asset!='Other')
  nAssets=nrow(portfolio)
  ##################################################################################################################################
  
  ##################################################################################################################################
  ## 2. define parameters to handle climate data
  ##################################################################################################################################
  ## information regarding the precipitation rate reanalysis data used
  if(climate_data=='NOAA.CIRES.V2c'){
    daily_TS_file_path='NOAAV2c_land_TS_files/'
    daily_TS_file_list='NOAAV2c.config.files.txt'
    
    start.year=1851
    end.year=2014
    nyears=end.year-start.year+1
  }  else if (climate_data=='ECMWF.ERA.20C'){
    daily_TS_file_path='ERA20C_land_TS_files/'
    daily_TS_file_list='ERA20C.config.files.txt'
    
    start.year=1900
    end.year=2010
    nyears=end.year-start.year+1
  }

  ##################################################################################################################################
  ## 3. compute exceedance_TS
  ##################################################################################################################################
  #
  time.start = proc.time()
  exceedance_TS=data.frame(year=(start.year:end.year))
  if(event_type=="heavy_rainfall"){
    prob=1-threshold
  }else if (event_type=='drought'){
    prob=threshold
  }
  
  for (k in 1:nAssets){
    if (is.na(portfolio$lon[k]) | is.na(portfolio$lat[k])){
      exceedance_TS=cbind(exceedance_TS,  rep(0,nyears))
      colnames(exceedance_TS)[ncol(exceedance_TS)]=paste0('grid_block', toString(k))
    } else{
      ## obtain the time series corresponding to asset k
      asset_lon=portfolio$longitude[k]
      asset_lon=transform.long.sys(asset_lon, 0, 1)
      asset_lat=portfolio$latitude[k]
      
      prate_TS_file=as.character(find.part.file(asset_lon, asset_lat, daily_TS_file_path, daily_TS_file_list)[1])
      
      load(file=paste0(daily_TS_file_path,prate_TS_file))
      
      lat = prate$lat
      lon = prate$lon
      prate.field = prate$prate[,-1]
      time = prate$prate[,1]
      
      matLonLat=cbind(lon, lat)
      Idx=findIdxLonLat(asset_lon, asset_lat, matLonLat)
      
      asset_prate=cbind(time, prate.field[,Idx])
      rownames(asset_prate)<-NULL
      colnames(asset_prate)=c('time', 'prcp')
      asset_prate=as.data.frame(asset_prate)
      asset_prate$date=as.Date(asset_prate$time, origin='1800-01-01')
      asset_prate$year=format(asset_prate$date, '%Y')
      asset_prate2=data.frame(year=asset_prate$year, prcp=asset_prate$prcp)
      
      ## compute the value of the threshold in terms of precipitation
      if (threshold_type == 'absolute'){
        extremes.threshold<-threshold
      } else if (event_type=='heavy_rainfall'){
        extremes.threshold<-get_heavy_rain_threshold(rain_ts=asset_prate2, window=window, prob=prob, 
                                                     start.year, end.year, nyears)
      } else if (event_type=='drought'){
        extremes.threshold<-get_drought_threshold(rain_ts=asset_prate2, window=window, prob=prob,
                                                  start.year, end.year, nyears)
      }
      
      asset_prate3=data.frame(time=asset_prate$time, prcp=asset_prate$prcp)
      if (event_type=='heavy_rainfall'){
        if(window>1){ 
          extreme_TS<-get_heavy_rainfall_dates_several_day_window(rain_ts=asset_prate3, window=window, threshold=extremes.threshold)
        } else{
          extreme_TS<-get_heavy_rainfall_dates_1_day_window(rain_ts=asset_prate3, window=window, threshold=extremes.threshold)
        }
      } else if (event_type=='drought'){
        extreme_TS<-get_drought_dates_several_day_window(rain_ts=asset_prate3, window=window, threshold=extremes.threshold)
      }  
      
      extreme_TS$date=as.Date(extreme_TS$time, origin='1800-01-01')
      extreme_TS$year=format(extreme_TS$date, '%Y')
      
      extreme_counts=table(extreme_TS$year)
      extreme_counts=as.data.frame(extreme_counts)
      
      if(nrow(extreme_counts)>0){
        colnames(extreme_counts)=c("year", "counts")
        all_years=data.frame(year=(start.year:end.year))
        yearly_counts=merge(all_years, extreme_counts, by='year', all.x=T)
        yearly_counts$counts[is.na(yearly_counts$counts)]<-0
      } else {
        yearly_counts=data.frame(year=c(start.year:end.year), counts=rep(0,nyears))
      }
       
      exceedance_TS=cbind(exceedance_TS,  yearly_counts$counts)
      colnames(exceedance_TS)[ncol(exceedance_TS)]=paste0('grid_block', toString(k))
    }
  }
  print(time.start- proc.time())
  
  exceedance=list(exceedance_TS=exceedance_TS, lon=portfolio$lon, lat=portfolio$lat)
  dir.create(output_path, showWarnings = F)
  save(exceedance, 
       file=paste0(output_path, 'exceedance_TS_', portfolio_name, '_data_', climate_data, '_', event_type,'_thr_', threshold, 
                   '_window_', window, '_days'))
}

