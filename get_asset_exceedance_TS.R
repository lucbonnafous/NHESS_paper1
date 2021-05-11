# ----- Clear Workspace and Load Required Packages

source("R_functions/auxiliar_functions_geographic.R")
source('R_functions/asset_level_functions.R')
get_climate_data_specifics<-function(climate_data){
  if(climate_data=='NOAA CIRES 20th Century Reanalysis V2c'){
    daily_TS_file_path='climate_data/NOAAV2c_land_TS_files/'
    daily_TS_file_list='NOAAV2c.config.files.txt'
    var_name='precipitation (mm)'
    resolution='daily'
    start.year=1851
    end.year=2014
  } else if (climate_data=='ECMWF ERA 20C'){
    daily_TS_file_path='climate_data/ERA20C_land_TS_files/'
    daily_TS_file_list='ERA20C.config.files.txt'
    var_name='precipitation (mm)'
    resolution='daily'
    start.year=1900
    end.year=2010
  }else if(climate_data=='Dai PDSI index (drought only)'){
    daily_TS_file_path='climate_data/Dai_pdsi_files/'
    daily_TS_file_list='Dai.pdsi.config.files.txt'
    var_name='PDSI'
    resolution='monthly'
    start.year=1950
    end.year=2014
  }
  return(daily_TS_file_path, daily_TS_file_list, var_name, resolution, start.year, end.year)
}

get_asset_exceedance_TS_rl<-function(asset_lon, asset_lat, event_type, window, return_level, climate_data){
  
  ##################################################################################################################################
  ## 1. define parameters to handle climate data
  ##################################################################################################################################
  ## information regarding the precipitation rate reanalysis data used
  
  if(climate_data=='NOAA CIRES 20th Century Reanalysis V2c'){
    daily_TS_file_path='climate_data/NOAAV2c_land_TS_files/'
    daily_TS_file_list='NOAAV2c.config.files.txt'
    var_name='precipitation (mm)'
    resolution='daily'
    start.year=1851
    end.year=2014
  } else if (climate_data=='ECMWF ERA 20C'){
    daily_TS_file_path='climate_data/ERA20C_land_TS_files/'
    daily_TS_file_list='ERA20C.config.files.txt'
    var_name='precipitation (mm)'
    resolution='daily'
    start.year=1900
    end.year=2010
  }else if(climate_data=='Dai PDSI index (drought only)'){
    daily_TS_file_path='climate_data/Dai_pdsi_files/'
    daily_TS_file_list='Dai.pdsi.config.files.txt'
    var_name='PDSI'
    resolution='monthly'
    start.year=1950
    end.year=2014
  }
  nyears=end.year-start.year+1
  asset_lat=asset_coord$lat
  asset_lon=transform.long.sys(asset_coord$lon, 0, 1)
  climate_data_file=as.character(find.part.file(asset_lon, asset_lat, daily_TS_file_path, daily_TS_file_list)[1])

  ##################################################################################################################################
  ## 3. compute exceedance_TS
  ##################################################################################################################################
  #
  prob=1/return_level
  
  climate_var_TS_file=as.character(find.part.file(asset_lon, asset_lat, daily_TS_file_path, daily_TS_file_list)[1])
  
  load(file=paste0(daily_TS_file_path,climate_var_TS_file))
  
  lat = climate_var$lat
  lon = climate_var$lon
  climate_var.field = climate_var$climate_var[,-1]
  time = climate_var$climate_var[,1]
  
  matLonLat=cbind(lon, lat)
  Idx=findIdxLonLat(asset_lon, asset_lat, matLonLat)
  
  asset_climate_var=cbind(time, climate_var.field[,Idx])
  rownames(asset_climate_var)<-NULL
  colnames(asset_climate_var)=c('time', 'climate_var')
  asset_climate_var=as.data.frame(asset_climate_var)
  asset_climate_var$date=as.Date(asset_climate_var$time, origin='1800-01-01')
  asset_climate_var$year=format(asset_climate_var$date, '%Y')
  asset_climate_var2=data.frame(year=asset_climate_var$year, climate_var=asset_climate_var$climate_var)
  
  ## compute the value of the threshold in terms of precipitation
  extremes.threshold<-get_threshold(climate_var_ts=asset_climate_var2, event_type, window, prob, start.year, end.year, nyears)
  print('sdf')
  asset_climate_var3=data.frame(time=asset_climate_var$time, climate_var=asset_climate_var$climate_var)
  if (event_type=='heavy_rainfall'){
    if(window>1){ 
      extreme_TS<-get_heavy_rainfall_dates_several_day_window(rain_ts=asset_climate_var3, window=window, threshold=extremes.threshold)
    } else{
      extreme_TS<-get_heavy_rainfall_dates_1_day_window(rain_ts=asset_climate_var3, window=window, threshold=extremes.threshold)
    }
  } else if (event_type=='drought'){
    extreme_TS<-get_drought_dates_several_day_window(rain_ts=asset_climate_var3, window=window, threshold=extremes.threshold)
  }  
  
  extreme_TS$date=as.Date(extreme_TS$time, origin='1800-01-01')
  extreme_TS$year=format(extreme_TS$date, '%Y')
  
  extreme_counts=table(extreme_TS$year)
  extreme_counts=as.data.frame(extreme_counts)
  
  if(nrow(extreme_counts)>0){
    colnames(extreme_counts)=c("year", "counts")
    all_years=data.frame(year=(start.year:end.year))
    asset_yearly_exceedance_TS=merge(all_years, extreme_counts, by='year', all.x=T)
    asset_yearly_exceedance_TS$counts[is.na(asset_yearly_exceedance_TS$counts)]<-0
  } else {
    asset_yearly_exceedance_TS=data.frame(year=c(start.year:end.year), counts=rep(0,nyears))
  }
 return(asset_yearly_exceedance_TS)


}

