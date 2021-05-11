

m1<-function(info){
  gridblock_lon=info$asset_lon 
  gridblock_lat=info$asset_lat 
  w=info$w
  rl=info$return_level
  start_year=info$start_year
  end_year=info$end_year
  event_type=info$event_type
  
  climate_data_file_path=info$climate_data_file_path
  climate_data_file_list=info$climate_data_file_list
  origin=info$origin
  
  climate_var_TS_file=as.character(find.part.file(
    vlon=transform.long.sys(gridblock_lon, 0, 1), vlat=gridblock_lat, 
    path=climate_data_file_path, file.config=climate_data_file_list)[1])
  
  load(file=paste0(climate_data_file_path,climate_var_TS_file))
  
  lat = climate_var$lat
  
  lon = climate_var$lon
  climate_var.field = climate_var$climate_var[,-1]
  time = climate_var$climate_var[,1]
  
  matLonLat=cbind(lon, lat)
  Idx=findIdxLonLat(gridblock_lon, gridblock_lat, matLonLat)
  gridblock_climate_var=cbind(time, climate_var.field[,Idx])
  rownames(gridblock_climate_var)<-NULL
  colnames(gridblock_climate_var)=c('time', 'climate_var')
  gridblock_climate_var=as.data.frame(gridblock_climate_var)
  gridblock_climate_var$date=as.Date(gridblock_climate_var$time, origin=origin)
  gridblock_climate_var$year=format(gridblock_climate_var$date, '%Y')
  gridblock_climate_var=subset(gridblock_climate_var, 
                               gridblock_climate_var$year>=start_year & gridblock_climate_var$year<=end_year)
  
  prob=1/rl
  if(w==1){
    if(event_type=='extreme rainfall'){
      gridblock_climate_var %>% group_by(year) %>%
        summarise(gridblock_climate_var_yearly=max(climate_var)) -> gridblock_climate_var_yearly_ext
      extremes.threshold=quantile(gridblock_climate_var_yearly_ext$gridblock_climate_var_yearly, (1-prob))
      gridblock_climate_var$extreme_uniq <- ifelse(gridblock_climate_var$climate_var > extremes.threshold,1,0)
    }else if (event_type=='drought'){
      gridblock_climate_var %>% group_by(year) %>%
        summarise(gridblock_climate_var_yearly=min(climate_var)) -> gridblock_climate_var_yearly_ext
      extremes.threshold=quantile(gridblock_climate_var_yearly_ext$gridblock_climate_var_yearly, prob)
      gridblock_climate_var$extreme_uniq <- ifelse(gridblock_climate_var$climate_var < extremes.threshold,1,0)
      
    }
  }else {
    if(event_type=='extreme rainfall'){
      gridblock_climate_var$accumulated <- stats::filter(gridblock_climate_var$climate_var, rep(1,w), sides=1)
      gridblock_climate_var$accumulated[is.na(gridblock_climate_var$accumulated)]<-0
      gridblock_climate_var %>% group_by(year) %>%
        summarise(gridblock_climate_var_yearly=max(accumulated)) -> gridblock_climate_var_yearly_ext
      threshold=quantile(gridblock_climate_var_yearly_ext$gridblock_climate_var_yearly, (1-prob))
      gridblock_climate_var$extreme <- ifelse(gridblock_climate_var$accumulated > threshold,1,0)
      gridblock_climate_var$extreme_uniq = 0
      for(dd in c(which(gridblock_climate_var$extreme==1))){
        if(sum(gridblock_climate_var$extreme_uniq[(dd-w+1):(dd-1)]) < 1){
          gridblock_climate_var[dd, c("extreme_uniq")] = 1
        }
      }
    }else if(event_type=='drought'){
      gridblock_climate_var$accumulated <- stats::filter(gridblock_climate_var$climate_var, rep(1,w), sides=1)
      gridblock_climate_var$accumulated[is.na(gridblock_climate_var$accumulated)]<-9999
      gridblock_climate_var %>% group_by(year) %>%
        summarise(gridblock_climate_var_yearly=min(accumulated)) -> gridblock_climate_var_yearly_ext
      threshold=quantile(gridblock_climate_var_yearly_ext$gridblock_climate_var_yearly, prob)
      gridblock_climate_var$extreme <- ifelse(gridblock_climate_var$accumulated < threshold,1,0)
      gridblock_climate_var$extreme_uniq = 0
      for(dd in c(which(gridblock_climate_var$extreme==1))){
        if(sum(gridblock_climate_var$extreme_uniq[(dd-w+1):(dd-1)]) < 1){
          gridblock_climate_var[dd, c("extreme_uniq")] = 1
        }
      }
    }
  }
  gridblock_climate_var %>% group_by(year) %>%
    summarise(extremes=sum(extreme_uniq)) -> extreme_counts
  extreme_counts=extreme_counts$extremes
  return(extreme_counts)
}