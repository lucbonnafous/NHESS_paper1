get_GEV_rl_plot<-function(asset_lon, 
                          asset_lat, 
                          event_type, 
                          window,
                          return_level,
                          climate_data_file_path, 
                          climate_data_file_list,
                          var_name,
                          resolution,
                          start_year,
                          end_year){
  nyears=end_year-start_year+1
  asset_prate_yearly_max=data.frame(year=c(1:nyears))
  asset_prate_yearly_max$year2=asset_prate_yearly_max$year^2
  asset_prate_yearly_max$year3=asset_prate_yearly_max$year^3
  asset_prate_yearly_max$year4=asset_prate_yearly_max$year^4
  asset_prate_yearly_max$yearly_max_TS<-get_asset_climate_var_yearly_max_TS_rl(asset_lon, 
                                                                             asset_lat, 
                                                                             event_type, 
                                                                             window,
                                                                             return_level,
                                                                             climate_data_file_path, 
                                                                             climate_data_file_list,
                                                                             var_name,
                                                                             resolution,
                                                                             start_year,
                                                                             end_year)


  x=c(start.year:end.year)
  y=asset_prate_yearly_max$yearly_max  
  
  fitGEV <- fevd(yearly_max, data = asset_prate_yearly_max)
  look <- summary(fitGEV, silent=TRUE)
  look <- look$BIC
  
  fitGEV10 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year)
  look10 <- summary(fitGEV10, silent=TRUE)
  look10 <- look10$BIC
  
  fitGEV20 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2)
  look20 <- summary(fitGEV20, silent=TRUE)
  look20 <- look20$BIC
  
  fitGEV30 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2+year3)
  look30 <- summary(fitGEV30, silent=TRUE)
  look30 <- look30$BIC
  
  fitGEV40 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2+year3+year4)
  look40 <- summary(fitGEV40, silent=TRUE)
  look40 <- look40$BIC
  
  fitGEV01 <- fevd(yearly_max, data = asset_prate_yearly_max, scale.fun=~year)
  look01 <- summary(fitGEV01, silent=TRUE)
  look01 <- look01$BIC
  
  fitGEV02 <- fevd(yearly_max, data = asset_prate_yearly_max, scale.fun=~year+year2)
  look02 <- summary(fitGEV02, silent=TRUE)
  look02 <- look02$BIC
  
  fitGEV03 <- fevd(yearly_max, data = asset_prate_yearly_max, scale.fun=~year+year2+year3)
  look03 <- summary(fitGEV03, silent=TRUE)
  look03 <- look03$BIC
  
  fitGEV04 <- fevd(yearly_max, data = asset_prate_yearly_max, scale.fun=~year+year2+year3+year4)
  look04 <- summary(fitGEV04, silent=TRUE)
  look04 <- look04$BIC
  
  fitGEV11 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year, scale.fun=~year)
  look11 <- summary(fitGEV11, silent=TRUE)
  look11 <- look11$BIC
  
  fitGEV12 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year, scale.fun=~year+year2)
  look12 <- summary(fitGEV12, silent=TRUE)
  look12 <- look12$BIC
  
  fitGEV13 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year, scale.fun=~year+year2+year3)
  look13 <- summary(fitGEV13, silent=TRUE)
  look13 <- look13$BIC
  
  fitGEV14 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year, scale.fun=~year+year2+year3+year4)
  look14 <- summary(fitGEV14, silent=TRUE)
  look14 <- look14$BIC
  
  fitGEV21 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2, scale.fun=~year)
  look21 <- summary(fitGEV21, silent=TRUE)
  look21 <- look21$BIC
  
  fitGEV22 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2, scale.fun=~year+year2)
  look22 <- summary(fitGEV22, silent=TRUE)
  look22 <- look22$BIC
  
  fitGEV23 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2, scale.fun=~year+year2+year3)
  look23 <- summary(fitGEV23, silent=TRUE)
  look23 <- look23$BIC
  
  fitGEV24 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2, scale.fun=~year+year2+year3+year4)
  look24 <- summary(fitGEV24, silent=TRUE)
  look24 <- look24$BIC
  
  fitGEV31 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2+year3, scale.fun=~year)
  look31 <- summary(fitGEV31, silent=TRUE)
  look31 <- look31$BIC
  
  fitGEV32 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2+year3, scale.fun=~year+year2)
  look32 <- summary(fitGEV32, silent=TRUE)
  look32 <- look32$BIC
  
  fitGEV33 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2+year3, scale.fun=~year+year2+year3)
  look33 <- summary(fitGEV33, silent=TRUE)
  look33 <- look33$BIC
  
  fitGEV34 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2+year3, 
                   scale.fun=~year+year2+year3+year4)
  look34 <- summary(fitGEV34, silent=TRUE)
  look34 <- look34$BIC
  
  fitGEV41 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2+year3+year4, scale.fun=~year)
  look41 <- summary(fitGEV41, silent=TRUE)
  look41 <- look41$BIC
  
  fitGEV42 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2+year3+year4, scale.fun=~year+year2)
  look42 <- summary(fitGEV42, silent=TRUE)
  look42 <- look42$BIC
  
  fitGEV43 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2+year3+year4, 
                   scale.fun=~year+year2+year3)
  look43 <- summary(fitGEV43, silent=TRUE)
  look43 <- look43$BIC
  
  fitGEV44 <- fevd(yearly_max, data = asset_prate_yearly_max, location.fun=~year+year2+year3+year4, 
                   scale.fun=~year+year2+year3+year4)
  look44 <- summary(fitGEV44, silent=TRUE)
  look44 <- look44$BIC
  
  scores=data.frame(model=c(fitGEV, fitGEV01, fitGEV02, fitGEV03, fitGEV04, 
                            fitGEV10, fitGEV11, fitGEV12, fitGEV13, fitGEV14,
                            fitGEV20, fitGEV21, fitGEV22, fitGEV23, fitGEV24,
                            fitGEV30, fitGEV31, fitGEV32, fitGEV33, fitGEV34,
                            fitGEV40, fitGEV41, fitGEV42, fitGEV43, fitGEV44),
                    BIC=c(look, look01, look02, look03, look04, 
                          look10, look11, look12, look13, look14, 
                          look20, look21, look22, look23, look24,
                          look30, look31, look32, look33, look34, 
                          look40, look41, look42, look43, look44))
  
  scores2=scores[ order(scores$BIC, na.last=T), ]
  
  
  
  #axis(1, at=att, labels=lab)
  #title('10-year return level', xlab='Year', ylab='Return level')
  
  return(plot(fitGEV, type='rl', rperiods=info$return_level,xaxt='n', ann=FALSE))
}