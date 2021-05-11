
library('grDevices')
library('ggplot2')


plot_asset_TS<-function(asset_coord, climate_data){
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
  p<-ggplot()
  if(climate_data_file=='NA'){
    print('Input coordinates do not correspond to an inland location')
    return(p)
  }else{
    load(file=paste0(daily_TS_file_path, climate_data_file))
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
    if(climate_data=='NOAA CIRES 20th Century Reanalysis V2c'){
      asset_climate_var$climate_var=asset_climate_var$climate_var*3600*8
    }else if(climate_data=='ECMWF ERA 20C'){
      asset_climate_var$climate_var=asset_climate_var$climate_var*1000
    }
    
    p<-p+
      geom_line(data=asset_climate_var, aes(x=asset_climate_var$date, 
                                            y=asset_climate_var$climate_var))+
      xlab('Time')+
      ylab(var_name)+
      ggtitle(paste0('Time series of ', resolution, ' ', var_name, ' using the ', climate_data, 
                     ' dataset'))
    return(p)
  } 
}