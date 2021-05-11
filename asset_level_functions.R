# ----- Clear Workspace and Load Required Packages

source("R_functions/geographic_functions.R")
source('R_functions/get_extreme_events_functions.R')
get_climate_data_specifics<-function(climate_data){
  if(climate_data=='NOAA CIRES 20th Century Reanalysis V2c'){
    climate_data_file_path='climate_data/20CR_land_TS_files/'
    climate_data_file_list='20CR_land.config.files.txt'
    var_name='precipitations (mm)'
    resolution='daily'
    start_year=1851
    end_year=2014
    origin='1800-01-01'
    mult=3600*8
  } else if (climate_data=='ECMWF ERA 20C'){
    climate_data_file_path='climate_data/ERA20C_land_TS_files/'
    climate_data_file_list='ERA20C_land.config.files.txt'
    var_name='precipitations (mm)'
    resolution='daily'
    start_year=1900
    end_year=2010
    origin='1800-01-01'
    mult=1000
  }else if(climate_data=='Dai PDSI index (drought only)'){
    climate_data_file_path='climate_data/DAI_PDSI_TS_files/'
    climate_data_file_list='Dai.pdsi.config.files.txt'
    var_name='PDSI'
    resolution='monthly'
    start_year=1950
    end_year=2014
    origin='1800-01-01'
    mult=1
    
  }
  return(list(climate_data_file_path=climate_data_file_path, 
              climate_data_file_list=climate_data_file_list, 
              var_name=var_name, 
              resolution=resolution, 
              start_year=start_year, end_year=end_year, origin=origin, mult=mult))
}
plot_asset_TS<-function(info){
  asset_lon=as.numeric(transform.long.sys(info$asset_lon, 0, 1))
  asset_lat=info$asset_lat
  climate_data_file_path=info$climate_data_file_path
  climate_data_file_list=info$climate_data_file_list
  var_name=info$var_name
  resolution=info$resolution
  start_year=info$start_year
  end_year=info$end_year
  origin=info$origin
  mult=info$mult
  
  nyears=end_year-start_year+1
  climate_data_file=paste0(climate_data_file_path, as.character(find.part.file(asset_lon, asset_lat, climate_data_file_path, climate_data_file_list)[1]))
  
  p<-ggplot()
  if(climate_data_file=='NA'){
    print('Input coordinates do not correspond to an inland location')
    return(p)
  }else{
    load(file=climate_data_file)
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
    asset_climate_var$climate_var=asset_climate_var$climate_var*mult
    asset_climate_var$date=as.Date(asset_climate_var$time, origin=origin)
    
    
    p<-p+
      geom_line(data=asset_climate_var, aes(x=asset_climate_var$date, 
                                            y=asset_climate_var$climate_var))+
      
      xlab('Time')+
      ylab(var_name)+
      ggtitle(paste0('Time series of ', resolution, ' ', var_name))
    return(p)
  } 
}


get_asset_climate_var_yearly_ext_TS_rl<-function(info){
  
  asset_lon=info$asset_lon 
  asset_lat=info$asset_lat 
  event_type=info$event_type
  w=info$w
  rl=info$return_level
  start_year=info$start_year
  end_year=info$end_year
  origin=info$origin
  nyears=end_year-start_year+1
  climate_data_file_path=info$climate_data_file_path
  climate_data_file_list=info$climate_data_file_list
  mult=info$mult
  ##################################################################################################################################
  ## 1. define parameters to handle climate data
  ##################################################################################################################################
  ## information regarding the precipitation rate reanalysis data used
  
  climate_data_file=as.character(find.part.file(asset_lon, asset_lat, climate_data_file_path, climate_data_file_list)[1])
  
  ##################################################################################################################################
  ## 3. compute exceedance_TS
  ##################################################################################################################################
  #
  
  load(file=paste0(climate_data_file_path,climate_data_file))
  
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
  asset_climate_var$date=as.Date(asset_climate_var$time, origin=origin)
  asset_climate_var$year=format(asset_climate_var$date, '%Y')
  asset_climate_var$accumulated <- stats::filter(asset_climate_var$climate_var, rep(1,w), sides=1)
  asset_climate_var$accumulated[is.na(asset_climate_var$accumulated)]<-0
  asset_climate_var_yearly_ext_TS=rep(NA, nyears)
  
  for (y in (start_year:end_year)){
    yearly_TS=subset(asset_climate_var, year==y)
    if(event_type=='extreme rainfall'){
      asset_climate_var_yearly_ext_TS[(y-start_year+1)]=max(yearly_TS$accumulated)
    }else if(event_type=='drought'){
      asset_climate_var_yearly_ext_TS[(y-start_year+1)]=min(yearly_TS$accumulated)
    }
  }

  return(asset_climate_var_yearly_ext_TS*mult)
}

