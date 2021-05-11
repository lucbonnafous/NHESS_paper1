pfunc<-function(a){
  
  x <- seq(-4, 4, length=100)
  hx <- dnorm(x, mean=0, sd=a)
  plot.new()
  plot(x, hx)
}

dev.off()
