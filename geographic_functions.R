# ***************************************************************
# 1) Auxiliary functions to use in geographical computations
# ***************************************************************

# -----------------------------------------------------------------------
# convertes degrees to radians
# -----------------------------------------------------------------------
degrees.to.radians<-function(degree){
  #degree must be informed in decimal format
  radians<-degree*pi/180
  return(radians)
}

# -----------------------------------------------------------------------
# transform longitude system
# long: longitude vector
# type.in: type of longitude range in input. 0=[-180,+180] ; 1=[0,360]
# type.out: type of longitude range in output. 0=[-180,+180] ; 1=[0,360]
# -----------------------------------------------------------------------
transform.long.sys<-function(long, type.in, type.out){
  
  if(type.in==type.out){
    warning("Input and output types of long are the same! No conversion was done.",call.=FALSE)
    return(long)
  }
  
  long.out<-long
  
  if(type.in == 0){
    #convert to type 1
    long.out[long.out<0]<-long[long.out<0]+360
  }else{
    #convert to type 0
    long.out[long.out>180]<-long[long.out>180]-360
  }
  
  return(long.out)
}

# -----------------------------------------------------------------------------------------
# transforms vectors lon, lat, Time and 3-D array precep into a data frame (version2)
# -----------------------------------------------------------------------------------------
array2data.frame.2<-function(lon,lat,Time, precep){      
  #auxiliar column arrays for building data.frame
  lon.2<-rep(lon,rep(length(lat)*length(Time),length(lon)))
  lat.2<-rep(rep(lat,rep(length(Time),length(lat))),length(lon))
  Time.2<-rep(Time,length(lat)*length(lon))
  
  df<-data.frame(lon.2,lat.2,Time.2, as.vector(aperm(precep,c(length(dim(precep)):1))))
  names(df)<-c("lon","lat","time", "values")
  return(df)
}

# -----------------------------------------------------------------------
# calculates the distance between point A (valueLon, valueLat) and a set of n other
# points (represented by matrix.LongLat) in earth surface in Km
# valueLon: longitude of point A
# valueLat: latitude of point A
# matrix.LongLat: a nx2 matrix of the form [lon,lat] representing the positions of the set
#                 of n points
# -----------------------------------------------------------------------
distKmLonLat<-function (valueLon, valueLat, matrix.LongLat) {
  
  earthRadius<-6371
  
  latRad = degrees.to.radians(valueLat)
  lonRad = degrees.to.radians(valueLon)
  arrayLatRad = degrees.to.radians(matrix.LongLat[,2])
  arrayLonRad = degrees.to.radians(matrix.LongLat[,1])
  
  dLat<-latRad-arrayLatRad
  dLon<-lonRad-arrayLonRad
  
  a <- sin(dLat / 2)^2 + sin(dLon / 2)^2 * cos(latRad) * cos(arrayLatRad)  
  c <- 2 * atan2(sqrt(a), sqrt(1-a))  
  dist <- earthRadius * c
  
  return(dist) #output distance, in km  
}

# -----------------------------------------------------------------------------------------
# compute center (long,lat) of a region (or of set of regions)
# map: a map data frame (must have columns named lat and long)
# region: string (or vector of strings) with name of desired region (or subregion) in the map data frame
# type: string with name of the column in the map data frame where we want to find "region"
# -----------------------------------------------------------------------------------------
compute.center<-function(map, region, type){  
  if(!(type %in% names(map))){    
    warning(paste0("Column ",type, " not found in map! Check arguments."))
    stop
  }
  suppressMessages(require(geosphere))
  
  cg.matrix<-matrix(data=NA, nrow= length(region),ncol=2)
  
  for (i in 1:length(region)){
    rows.region<-map[type]==region[i]
    lat.region<-map$lat[rows.region]
    long.region<-map$long[rows.region]
    mat<-matrix(data=cbind(long.region,lat.region),ncol=2)
    cg.matrix[i,]<-centroid(mat)      
  }
  
  cg<-as.data.frame(cg.matrix)
  names(cg)<-c("lon","lat")
  return(cg) #return data frame with (long,lat) of centroids
}

# -----------------------------------------------------------------------
# finds index in 2D matrix [colLong,colLat] that is closest to (valueLon,valueLat)
# -----------------------------------------------------------------------
findIdxLonLat <- function(valueLon, valueLat, matrix.LongLat) {
  dist<-distKmLonLat(valueLon, valueLat, matrix.LongLat)
  idxmin<-which(dist==min(dist),arr.ind = TRUE)    
  return(idxmin)
}

# -----------------------------------------------------------------------
# finds row and column of cell that is closest to (valueLon,valueLat)
# valueLon: value of longitude being searched
# valueLat: value of latitude being searched
# vector.long: vector with values of longitude 
# vector.lat: vector with values of latitudes
# -----------------------------------------------------------------------
find.Row.Col.LonLat <- function(valueLon, valueLat, vector.long,vector.lat) {

  df<-array2data.frame.2(vector.long,vector.lat,1, array(data=0,dim=c(length(vector.long),length(vector.lat))))
  m<-as.matrix(df[,1:2])
  idx<-findIdxLonLat(valueLon, valueLat, m)
  r<-idx %/% length(vector.lat) + 1
  c<-idx %% length(vector.lat)
  df.out<-as.data.frame(cbind(r,c))
  names(df.out)<-c("row","col")      
  return(df.out)
}

# -----------------------------------------------------------------------
# finds the limits of each grid box
# lat: vector with latitudes of grid boxes (IMPORTANT: vector must be ordered in ascending/descending)
# lon: vector with longitudes of grid boxes (IMPORTANT: vector must be ordered in ascending)
# -----------------------------------------------------------------------
limits.grid<-function(lat,lon){
  
  nlon<-length(lon)
  nlat<-length(lat)
  
  delta<-mean(lon[2:nlon]-lon[1:(nlon-1)])
  
  grid.left<-(lon-c((lon[1]-delta),lon[1:(nlon-1)]))/2
  grid.left<-lon-grid.left
  
  grid.right<-(c(lon[2:nlon],(lon[nlon]+delta))-lon)/2
  grid.right<-lon+grid.right
  
  delta<-mean(lat[2:nlat]-lat[1:(nlat-1)])
  
  if(delta>0){
    grid.up<-(c(lat[2:(nlat)],(lat[nlat]+delta))-lat)/2
    grid.up<-lat+grid.up
    
    grid.down<-(lat-c((lat[1]-delta),lat[1:(nlat-1)]))/2
    grid.down<-lat-grid.down    
  }else{
    grid.up<-(c((lat[1]-delta),lat[1:(nlat-1)])-lat)/2
    grid.up<-lat+grid.up
    
    grid.down<-(lat-c(lat[2:nlat],lat[nlat]+delta))/2
    grid.down<-lat-grid.down    
  }
  
  out<-list()
  
  out$grid.left<-grid.left
  out$grid.right<-grid.right
  out$grid.up<-grid.up
  out$grid.down<-grid.down
  
  return(out)
}

# -----------------------------------------------------------------------
# computes area of each grid box
# lat: vector with latitudes of grid boxes (IMPORTANT: vector must be ordered in ascending/descending)
# lon: vector with longitudes of grid boxes (IMPORTANT: vector must be ordered in ascending)
# -----------------------------------------------------------------------
compute.area.grid<-function(lat,lon){
  lim.grid<-limits.grid(lat,lon)
  
  nlon<-length(lon)
  nlat<-length(lat)  
  
  n.grids<-nlon*nlat
  
  area.grids<-matrix(data=NA,nrow=nlon,ncol=nlat)
  
  for(c in 1:nlat){
    for(r in 1:nlon){
      dist.x<-distKmLatLon(lim.grid$grid.up[c],lim.grid$grid.left[r],matrix(data=c(lim.grid$grid.right[r],lim.grid$grid.up[c]),nrow=1))
      dist.y<-distKmLatLon(lim.grid$grid.up[c],lim.grid$grid.left[r],matrix(data=c(lim.grid$grid.left[r],lim.grid$grid.down[c]),nrow=1))
      
      area.grids[r,c]<-dist.x*1e3*dist.y*1e3
    }
  }
  
  return(area.grids)
}

# -----------------------------------------------------------------------
# finds which grid each element in df belongs to
# df.data: data frame of elements with a lat and lon columns
# lat: vector with latitudes of grid boxes (IMPORTANT: vector must be ordered in ascending/descending)
# lon: vector with longitudes of grid boxes (IMPORTANT: vector must be ordered in ascending)
# -----------------------------------------------------------------------
match.element.grid<-function(df.data, lat, lon){
  
  n.rows<-nrow(df.data)
  df.data$row<-rep(NA,n.rows)
  df.data$col<-rep(NA,n.rows)
  
  limits<-limits.grid(lat,lon)
  
  for(i in 1:n.rows){
    df.data$row[i]<-which(df.data$lon[i]>=limits$grid.left & df.data$lon[i]<limits$grid.right)
    df.data$col[i]<-which(df.data$lat[i]>=limits$grid.down & df.data$lat[i]<limits$grid.up)
  }
  
  return(df.data)
}

# -----------------------------------------------------------------------
# returns list with position of grids that have elements in it
# df.data: data frame of elements with a lat and lon columns
# lat: vector with latitudes of grid boxes
# lon: vector with longitudes of grid boxes
# -----------------------------------------------------------------------
get.grids<-function(df.data, lat, lon){
  
  n.rows<-nrow(df.data)
  n<-0
  
  limits<-limits.grid(lat,lon)
  
  l<-list()
  l$row<-c()
  l$col<-c()
  
  for(i in 1:length(lon)){
    for(j in 1:length(lat)){
      s<-sum(df.data$lon>=limits$grid.left[i] & df.data$lon<limits$grid.right[i] &
               df.data$lat>=limits$grid.down[j] & df.data$lat<limits$grid.up[j])
      if(s>0){
        l$row<-c(l$row,i)
        l$col<-c(l$col,j)
      }
    }
  }
  
  return(l)
}


# -----------------------------------------------------------------------
# For a set of geographically partioned files, finds file what contains (vlon,vlat)
# vlat: latitude
# vlon: longitude
# path: path where file is located (string)
# file.config: name of txt file with the configuration parameters of partitions (name and range of latitudes and longitudes)
# -----------------------------------------------------------------------
find.part.file<-function(vlon,vlat, path,file.config){
  
  df.part<-read.csv(file=paste0(path,file.config))
  
  print(c(vlon,vlat))
  
  idx<-((df.part$lon.min<=vlon) & (df.part$lon.max>=vlon) & (df.part$lat.min<=vlat) & (df.part$lat.max>=vlat))
  
  if(sum(!is.na(idx))==0){
    out<-NA
  }else{
    if(sum(idx)==0){
      out<-NA
    }else{
      out<-df.part$file.name[idx]
    }  
  }
  return(out)
}