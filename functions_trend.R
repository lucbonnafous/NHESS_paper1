
library('wq')
library('locfit')

run_MK_base<-function(t, TS, asset_number, portfolio_name, event_name, threshold, window, y.label){
  y=TS
  p=mannKen(ts(y, start=min(t), end=max(t)))$p.value
  sl=mannKen(ts(y, start=min(t), end=max(t)))$sen.slope
  
  p2<-plot(t, y, xlab="year", ylab=y.label, main=paste0(portfolio_name, ", ", asset_number,
                                                                               " assets \n ", event_name, "\n",
                                                                               "MK p-value = ", signif(p, digits=2)))

  lines(locfit.raw(x=c(min(t):max(t)), y, family="poisson"), col='red')
  legend("topright", legend='locfit', lty=1, lwd=2.5, col="red")
  return(p2) 
}
