##
#
get_heavy_rain_threshold<-function(rain_ts, window, prob, start.year, end.year, nyears){
  rain_ts$accumulated <- stats::filter(rain_ts$prcp, rep(1,window), sides=1)
  rain_ts$accumulated[is.na(rain_ts$accumulated)]<-0
  asset_prate_yearly_max=rep(NA, nyears)
    
  for (y in (start.year:end.year)){
    yearly_TS=subset(rain_ts, year==y)
    asset_prate_yearly_max[(y-start.year+1)]=max(yearly_TS$accumulated)
  }
  extremes.threshold = quantile(asset_prate_yearly_max,prob)
  return(extremes.threshold)
}

#
get_drought_threshold<-function(rain_ts, window, prob , start.year, end.year, nyears){
  rain_ts$accumulated <- stats::filter(rain_ts$prcp, rep(1,window), sides=1)
  rain_ts$accumulated[is.na(rain_ts$accumulated)]<-9999
  asset_prate_yearly_min=rep(NA, nyears)
  
  for (y in (start.year:end.year)){
    yearly_TS=subset(rain_ts, year==y)
    asset_prate_yearly_min[(y-start.year+1)]=min(yearly_TS$accumulated)
  }
  extremes.threshold = quantile(asset_prate_yearly_min,prob)
  return(extremes.threshold)
}

#
get_heavy_rainfall_dates_1_day_window <- function(rain_ts, window, threshold){
  # calculate the extreme events that are > threshold, as defined by the max over an window moving window
  # Required inputs:
  # threshold: numeric to indicate the minimum size of the extreme events to find
  # window: defines length of moving window
  # rain_ts: precipitation with a date and prcp columns
  # date format should be: "yyyy-mm-dd"
  
  # Mark whether an event is above the threshold
  rain_ts$extreme <- ifelse(rain_ts$prcp > threshold,1,0)
  rain_ts <- rain_ts[rain_ts$extreme == 1,]
  # return something readable and useful
  rain_ts <- rain_ts %>% select(time, prcp) %>%
    arrange(desc(-time)) %>%
    tbl_df()
  
  return(rain_ts)
}

get_heavy_rainfall_dates_several_day_window<- function(rain_ts, window, threshold){
  # calculate the extreme events that are > threshold, as defined by the max over an window moving window
  # Required inputs:
    # threshold: numeric to indicate the minimum size of the extreme events to find
    # window: defines length of moving window
    # rain_ts: precipitation with a date and prcp columns
	  # date format should be: "yyyy-mm-dd"
  rain_ts$accumulated <- stats::filter(rain_ts$prcp, rep(1,window), sides=1)
  rain_ts$accumulated[is.na(rain_ts$accumulated)]<-0  
  
  # Mark whether an event is above the threshold
  rain_ts$extreme <- ifelse(rain_ts$accumulated > threshold,1,0)
  
  # Select only the unique events
  rain_ts$extreme_uniq = 0
  for(dd in c(which(rain_ts$extreme==1))){
    if(sum(rain_ts$extreme_uniq[(dd-window+1):(dd-1)]) < 1){
      rain_ts[dd, c("extreme_uniq")] = 1
    }
  }
  
  # Select only unique extreme events
  #rain_ts_extremes <- rain_ts[rain_ts$extreme_uniq == 1,]
  rain_ts <- rain_ts[rain_ts$extreme_uniq == 1,]
  # return something readable and useful
  rain_ts <- rain_ts %>% select(time, prcp, accumulated) %>%
    arrange(desc(-time)) %>%
    tbl_df()
  
  return(rain_ts)
}

get_drought_dates_several_day_window<- function(rain_ts, window, threshold){
  # calculate the extreme events that are > threshold, as defined by the max over an window moving window
  # Required inputs:
  # threshold: numeric to indicate the minimum size of the extreme events to find
  # window: defines length of moving window
  # rain_ts: precipitation with a date and prcp columns
  # date format should be: "yyyy-mm-dd"
  rain_ts$accumulated <- stats::filter(rain_ts$prcp, rep(1,window), sides=1)
  rain_ts$accumulated[is.na(rain_ts$accumulated)]<-9999
  
  # Mark whether an event is above the threshold
  rain_ts$extreme <- ifelse(rain_ts$accumulated < threshold,1,0)
    
  # Select only the unique events
  rain_ts$extreme_uniq = 0
  for(dd in c(which(rain_ts$extreme==1))){
    if(sum(rain_ts$extreme_uniq[(dd-window+1):(dd-1)]) < 1){
      rain_ts[dd, c("extreme_uniq")] = 1
    }
  }
  
  # Select only unique extreme events
  #rain_ts_extremes <- rain_ts[rain_ts$extreme_uniq == 1,]
  rain_ts <- rain_ts[rain_ts$extreme_uniq == 1,]
  # return something readable and useful
  rain_ts <- rain_ts %>% select(time, prcp, accumulated) %>%
    arrange(desc(-time)) %>%
    tbl_df()
  
  return(rain_ts)
}




