

cdf<-function(input){
###############################################################################################################################################
## INPUT PARAMETERS
###############################################################################################################################################

  nsim=nrow(input)
  
  ###############################################################################################################################################
  ## PLOT INDEX DISTRIBUTIONS
  ###############################################################################################################################################
  
  
  plots=list()
  #i=1
  for (i in 1:nsim){
    if(input$threshold.types[i]=='probability'){
      prob=input$thresholds[i]
    } ## else needs something if threshold is in absolute terms
    if(input$climate.data[i]=='NOAA.CIRES.V2c'){
      nyears=164
    } else if (input$climate.data[i]=='ECMWF.ERA.20C'){
      nyears=111
    }
    if(input$simulation.types[i]=='exceedance'){
      exceedance_file=paste0(input$exceedance.TS.path[i], 'exceedance_TS_', input$portfolio.file.names[i],
                             '_data_',input$climate.data[i], '_', input$event.type[i],'_thr_',
                             input$thresholds[i], '_window_', input$windows[i], '_days')
      load(exceedance_file)
      exceedance_TS=exceedance$exceedance_TS
      grid_block_TS=exceedance_TS[,-1]
      nassets=length(grid_block_TS)
      exceedance_TS$all_grid_blocks=rowSums (grid_block_TS, na.rm = FALSE, dims = 1)
  
      #dir.create(input$plot.paths[i], showWarnings = F)
      
      a=max(exceedance_TS$all_grid_blocks)
      x=c(0:a)
      
      theoretical=dpois(x, lambda=(nassets*prob))
      
      tt=table(exceedance_TS$all_grid_blocks)
      ttt=as.data.frame(tt, stringsAsFactors=F)
      colnames(ttt)=c("counts", "frequency")
      ttt$counts=as.numeric(ttt$counts)
      for (k in x){
        if (!is.element(k, ttt$counts)){
          ttt=rbind(ttt, c(k,0))
        }
      }
      
      ttt$empirical=ttt$frequency/nyears
      ttt=arrange(ttt, counts)
      ttt$theoretical=theoretical
      ttt$cum_theo=cumsum(ttt$theoretical)
      ttt$cum_emp=cumsum(ttt$empirical)
      
  #     box_df=data.frame(counts=c(exceedance_TS$all_grid_blocks, rpois(1000, lambda=nassets*prob)),
  #                       distribution='empirical')
  #     box_df$distribution=as.character(box_df$distribution)
  #     box_df$distribution[165:nrow(box_df)]='theoretical'
      
  #     p<-ggplot() +
  #       geom_boxplot(data=box_df, aes(x=distribution, y=counts, fill=distribution)) +
  #       guides(fill=FALSE)+
  #       #scale_fill_manual(name="distribution", labels=c("theoretical", "empirical"), values=c("blue", "red"))+
  #       ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))
  #     print(p)
      
      p<-ggplot() +
        geom_point(data=ttt, aes(x=counts, y=cum_theo, colour='blue'), size=4) +
        geom_point(data=ttt, aes(x=counts, y=cum_emp, colour='red'), size=4) +
        
        ylab(expression("cumulative distribution "~ italic("F(N")["t"] ~ italic("(p))")))+
        scale_colour_manual(name="distribution", labels=c("theoretical", "empirical"), values=c("blue", "red"))+
        ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))

      
  #     df1=data.frame(binomial=rbinom(nyears, nassets, prob), binomial=rep('binomial', nyears))
  #     df2=data.frame(empirical=exceedance_TS$all_grid_blocks, empirical=rep('empirical', nyears))
  #     colnames(df1)=c('events', 'distribution')
  #     colnames(df2)=c('events', 'distribution')
  #     
  #     p<-ggplot() +
  #       geom_density(data=df1, aes(x=events, fill=distribution), adjust=3, alpha=0.3) +
  #       geom_density(data=df2, aes(x=events, fill=distribution), adjust=2, alpha=0.3) +
  #       guides(fill=guide_legend(title="Distribution"))+
  #       scale_fill_manual(values=c("orange","red"))+
  #       ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))
      
      plots[[i]]=p
    } else if(input$simulation.types[i]=='NAV'){
      exceedance_file=paste0(input$exceedance.TS.path[i], 'exceedance_TS_', input$portfolio.file.names[i],
                             '_data_',input$climate.data[i], '_', input$event.type[i],'_thr_',
                             input$thresholds[i], '_window_', input$windows[i], '_days')
      load(exceedance_file)
      exceedance_TS=exceedance$exceedance_TS
      grid_block_TS=exceedance_TS[,-1]
      nassets=length(grid_block_TS)
      portfolio=read.csv(paste0(input$portfolio.file.path[i],input$portfolio.file.name[i], '.csv'),stringsAsFactors=F)
      grid_block_TS$other=rep(0, nyears)
      grid_block_TS=as.matrix(grid_block_TS)
      portfolio22=as.matrix(portfolio$NAV)
      grid_block_TS2=grid_block_TS%*%portfolio22
      exceedance_TS$all_grid_blocks=grid_block_TS2/sum(portfolio22)
      
      
      dir.create(input$plot.paths[i], showWarnings = F)
      
      a=max(exceedance_TS$all_grid_blocks)
      x=c(0:a)
      
      
      
      tt=table(exceedance_TS$all_grid_blocks)
      ttt=as.data.frame(tt, stringsAsFactors=F)
      colnames(ttt)=c("exposure", "frequency")
      ttt$counts=as.numeric(ttt$exposure)
      for (k in x){
        if (!is.element(k, ttt$exposure)){
          ttt=rbind(ttt, c(k,0))
        }
      }
      
      ttt$empirical=ttt$frequency/nyears
      ttt=arrange(ttt, exposure)
      ttt$cum_emp=cumsum(ttt$empirical)
      
      
  #     
  #     box_df=data.frame(counts=c(exceedance_TS$all_grid_blocks),
  #                       distribution='empirical')
  #     box_df$distribution=as.character(box_df$distribution)
  #     
  #     p<-ggplot() +
  #       geom_boxplot(data=box_df, aes(x=distribution, y=counts, fill=distribution)) +
  #       guides(fill=FALSE)+
  #       ylab(expression("exposure (share of total portfolio value)"))+
  #       #scale_fill_manual(name="distribution", labels=c("theoretical", "empirical"), values=c("blue", "red"))+
  #       ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))
  #     print(p)
      ttt$exposure=as.numeric(ttt$exposure)
      p<-ggplot() +
          geom_point(data=ttt, aes(x=exposure, y=cum_emp, colour='red'), size=4) +
          xlab(expression("exposure (share of total portfolio value)"))+
          ylab(expression("cumulative distribution of exposure "~ italic("F(R")["t"] ~ italic("(p))")))+
          scale_colour_manual(name="distribution", labels=c("empirical"), values=c("red"))+
          ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))
      print(p)
    
      
  #     p<-ggplot() +
  #       geom_point(data=ttt, aes(x=exposure, y=empirical), size=4) +
  #       ylab("probability mass")+
  #       ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))
  #     print(p)
      
  #     df2=data.frame(empirical=exceedance_TS$all_grid_blocks, empirical=rep('empirical', nyears))
  #     colnames(df2)=c('exposure', 'distribution')
  #     
  #     p<-ggplot() +
  #       geom_density(data=df2, aes(x=exposure, fill=distribution), adjust=2, alpha=0.3) +
  #       guides(fill=guide_legend(title="Distribution"))+
  #       scale_fill_manual(values=c("orange","red"))+
  #       xlab("exposure (share of the portfolio value)") +
  #       ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))
  #     
  #     print(p)
      plots[[i]]=p
    }else if(input$simulation.types[i]=='production'){
      exceedance_file=paste0(input$exceedance.TS.path[i], 'exceedance_TS_', input$portfolio.file.names[i],
                             '_data_',input$climate.data[i], '_', input$event.type[i],'_thr_',
                             input$thresholds[i], '_window_', input$windows[i], '_days')
      load(exceedance_file)
      exceedance_TS=exceedance$exceedance_TS
      grid_block_TS=exceedance_TS[,-1]
      nassets=length(grid_block_TS)
      portfolio=read.csv(paste0(input$portfolio.file.path[i],input$portfolio.file.name[i], '.csv'),stringsAsFactors=F)
      grid_block_TS=as.matrix(grid_block_TS)
      portfolio22=as.matrix(portfolio$Estimated.Earnings)
      
      grid_block_TS2=grid_block_TS%*%portfolio22
      exceedance_TS$all_grid_blocks=grid_block_TS2/sum(portfolio22)
      
      
      dir.create(input$plot.paths[i], showWarnings = F)
      
      a=max(exceedance_TS$all_grid_blocks)
      x=c(0:a)
      
      
      
      tt=table(exceedance_TS$all_grid_blocks)
      ttt=as.data.frame(tt, stringsAsFactors=F)
      colnames(ttt)=c("exposure", "frequency")
      ttt$counts=as.numeric(ttt$exposure)
      for (k in x){
        if (!is.element(k, ttt$exposure)){
          ttt=rbind(ttt, c(k,0))
        }
      }
      
      ttt$empirical=ttt$frequency/nyears
      ttt=arrange(ttt, exposure)
      ttt$cum_emp=cumsum(ttt$empirical)
      
      
      
      box_df=data.frame(counts=c(exceedance_TS$all_grid_blocks),
                        distribution='empirical')
      box_df$distribution=as.character(box_df$distribution)
      
  #     p<-ggplot() +
  #       geom_boxplot(data=box_df, aes(x=distribution, y=counts, fill=distribution)) +
  #       guides(fill=FALSE)+
  #       ylab(expression("exposure (share of estimated 2015 estimated earnings)"))+
  #       #scale_fill_manual(name="distribution", labels=c("theoretical", "empirical"), values=c("blue", "red"))+
  #       ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))
  #     print(p)
      ttt$exposure=as.numeric(ttt$exposure)
       p<-ggplot() +
          geom_point(data=ttt, aes(x=exposure, y=cum_emp, colour='red'), size=4) +
          xlab(expression("exposure (share of estimated 2015 output value)"))+
          ylab(expression("cumulative distribution of exposure "~ italic("F(R")["t"] ~ italic("(p))")))+
          scale_colour_manual(name="distribution", labels=c("empirical"), values=c("red"))+
          ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))
        print(p)
        
      
      #     p<-ggplot() +
      #       geom_point(data=ttt, aes(x=exposure, y=empirical), size=4) +
      #       ylab("probability mass")+
      #       ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))
      #     print(p)
      
      #     df2=data.frame(empirical=exceedance_TS$all_grid_blocks, empirical=rep('empirical', nyears))
      #     colnames(df2)=c('exposure', 'distribution')
      #     
      #     p<-ggplot() +
      #       geom_density(data=df2, aes(x=exposure, fill=distribution), adjust=2, alpha=0.3) +
      #       guides(fill=guide_legend(title="Distribution"))+
      #       scale_fill_manual(values=c("orange","red"))+
      #       xlab("exposure (share of the portfolio value)") +
      #       ggtitle(paste0(input$portfolio.names[i], '\n', input$event.names[i], '\n', input$climate.data[i]))
      #     
      #     print(p)
      plots[[i]]=p
    }
  }
  return(plots)
} 
  
  
