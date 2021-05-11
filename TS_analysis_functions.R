source('R_functions/functions_trend.R')

library('grDevices')
library('ggplot2')

plot_portfolio_TS<-function(exceedance_TS_path, portfolio_file_path, output_path, portfolio_file_name, simulation_type, portfolio_name, event_name, climate_data, event_type, threshold, window){
  dir.create(output_path, showWarnings = F)
  load(paste0(exceedance_TS_path, 'exceedance_TS_', portfolio_file_name, '_data_', climate_data, '_', event_type,'_thr_', threshold, 
              '_window_', window, '_days'))
  exceedance_TS=exceedance$exceedance_TS
  #exceedance_TS=subset(exceedance_TS, exceedance_TS$year>1899 & exceedance_TS$year<2011)
  grid_block_TS=exceedance_TS[,-1]
  asset_number=ncol(grid_block_TS)
  if (simulation_type=='NAV'){
    nyears=nrow(exceedance_TS)
    portfolio=read.csv(paste0( portfolio_file_path,portfolio_file_name, '.csv'),stringsAsFactors=F, sep=';')
    grid_block_TS$other=rep(0, nyears)
    grid_block_TS=as.matrix(grid_block_TS)
    portfolio22=as.matrix(portfolio$NAV)
    grid_block_TS2=grid_block_TS%*%portfolio22
    exceedance_TS$all_grid_blocks=grid_block_TS2/sum(portfolio22)
#     png(filename=paste0(output_path, 'exceedance_TS_', portfolio_file_name, '_data_', climate_data, '_', event_type, '_thr_', threshold, '_window_', window, '_days.png'),
#         width = 700, height = 700, units = "px")
    y.label='Exposure (share of portfolio value)'
    t=exceedance_TS$year
    y=exceedance_TS$all_grid_blocks
    p=mannKen(ts(y, start=min(t), end=max(t)))$p.value
    sl=mannKen(ts(y, start=min(t), end=max(t)))$sen.slope
   # p2<-plot.new()
    
    p2<-plot(t, y, xlab="year", ylab=y.label, cex.main=0.75, main=paste0(portfolio_name, ", ", asset_number,
                                                          " assets \n ", event_name, " - ",
                                                          climate_data, "\n",
                                                          "MK p-value = ", signif(p, digits=2)))
    
    lines(locfit.raw(x=c(min(t):max(t)), y, family="poisson"), col='red')
    legend("topright", legend='locfit', lty=1, lwd=2.5, col="red")
  

    
  }else if (simulation_type=='exceedance'){
    exceedance_TS$all_grid_blocks=rowSums (grid_block_TS, na.rm = FALSE, dims = 1)
#     png(filename=paste0(output_path, 'exceedance_TS_', portfolio_file_name, '_data_', climate_data, '_', event_type, '_thr_', threshold, '_window_', window, '_days.png'),
#         width = 700, height = 700, units = "px")
    y.label='Number of exceedances'
    t=exceedance_TS$year
    y=exceedance_TS$all_grid_blocks
    p=mannKen(ts(y, start=min(t), end=max(t)))$p.value
    sl=mannKen(ts(y, start=min(t), end=max(t)))$sen.slope
    #p2<-plot.new()
    model_pois=locfit.raw(x=c(min(t):max(t)),y, family='poisson')
    model_pois1=locfit.raw(x=c(min(t):max(t)),y, alpha=11/164, family='poisson', deg=1)
    model_pois2=locfit.raw(x=c(min(t):max(t)),y, alpha=21/164, family='poisson', deg=1)
    par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
    p2<-plot(t, y, xlab="year", ylab=y.label, 
             ylim=c(0, max(y, fitted(model_pois,c(min(t):max(t))), fitted(model_pois1,c(min(t):max(t))), fitted(model_pois2,c(min(t):max(t))))),
             cex.main=0.75, main=paste0(portfolio_name, ", ", asset_number,
                                                          " assets \n ", event_name, " - climate data: ",
                                                          climate_data, "\n",
                                                          "MK p-value = ", signif(p, digits=2)))
    
    #lines(locfit.raw(x=c(min(t):max(t)), y, family="poisson"), col='red')
    lines(model_pois, col='black', lwd=1)
    lines(model_pois1, col='red', lwd=1)
    lines(model_pois2, col='blue', lwd=1)
    #legend("topright", legend='locfit', lty=1, lwd=2.5, col="red")
    legend("right",inset=c(-0.35,0),cex=0.75,
       legend=c('Default Poisson\nlocfit\n','Poisson locfit\n11-year window\n', 
                'Poisson locfit\n21-year window\n'), 
       lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("black", "red", 'blue'))


  }
  return(p2)
}
