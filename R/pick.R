library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(caTools)
library(icesTAF)


### calculate urban from Landscan 
cy0.data.path <- "../python/lsfinal1999.csv"
cy0.data <- read.csv(cy0.data.path, check.names=FALSE)
start.year <-1999
end.year<-2017
for (year in c(start.year:end.year)){
  sleuth.data.path<-sprintf("../LandScan/lspop%s.tif",year)
  mapp <- raster(sleuth.data.path)
  column.blank1 <- rep.int(0, 143)
  for (c in c(1:114)){
#     mapRange <- 0.0435*(189-c)**0.5
    mapRange <- cy0.data$radius[c]
    cy <- cy0.data["city"][[1]][c]
    coy <- cy0.data["corY"][[1]][c]
    cox <- cy0.data["corX"][[1]][c]
    
    urban.map.ext <-extent(cox-mapRange,cox+mapRange,coy-mapRange,coy+mapRange)
    mapp.cp <- crop(mapp, urban.map.ext)
    
    values(mapp.cp)[values(mapp.cp)<=1000]=0
    values(mapp.cp)[values(mapp.cp)>1000]=1

    alal=sum(na.omit(values(mapp.cp)))
    column.blank1[c] <- alal
  }
  clm.name1 <- sprintf("%s_ls",year)
  cy0.data[, clm.name1] <- column.blank1
}

write.csv(cy0.data,"../python/ls_1999_2.csv")

### calculate future urban from GUSPS
cy0.data <- read.csv(cy0.data.path, check.names=FALSE)
start.year <-2016
end.year<-2050
for (year in c(start.year:end.year)){
  sleuth.data.path<-sprintf("../Zhou_GUSPS_2020_2050/global_final_%s.tif",year)
  mapp <- raster(sleuth.data.path)
  column.blank1 <- rep.int(0, 143)
  for (c in c(1:114)){
    mapRange <- cy0.data$radius[c]
    cy <- cy0.data["city"][[1]][c]
    coy <- cy0.data["corY"][[1]][c]
    cox <- cy0.data["corX"][[1]][c]
    
    urban.map.ext <-extent(cox-mapRange,cox+mapRange,coy-mapRange,coy+mapRange)
    mapp.cp <- crop(mapp, urban.map.ext)
    plot(mapp.cp)
    # writeRaster(mapp.cp,"beijing2050.tif")
    values(mapp.cp)[values(mapp.cp)<50]=NaN
    values(mapp.cp)[values(mapp.cp)>=50]=100
    sum(na.omit(values(mapp.cp)))
    alal=sum(na.omit(values(mapp.cp)))
    column.blank1[c] <- alal
  }
  clm.name1 <- sprintf("sl_%s",year)
  cy0.data[, clm.name1] <- column.blank1
}

write.csv(cy0.data,"../python/sl_16_50.csv")

##############################
##############################
#   validation in China or single city
##############################
##############################
cy0.data <- read.csv(cy0.data.path, check.names=FALSE)
cnct<-c(0, 8, 9, 11, 15, 17, 18, 19, 22, 24, 26, 28, 29, 31, 33, 35)+1
cnct=c(2)
year=2050
sleuth.data.path<-sprintf("../Zhou_GUSPS_2020_2050/global_final_%s.tif",year)
mapp.world <- raster(sleuth.data.path)
mapp.world.test <- raster(sleuth.data.path)
cn.map.ext <-extent(106.7,130.3,19.3,49)
mapp.cn<-crop(mapp.world, cn.map.ext)
plot(mapp.cn)
writeRaster(mapp.cn,"mappcn_2050.tif")

for (current.ct in cnct){
    mapRange <- cy0.data$radius[current.ct]
    cy <- cy0.data["city"][[1]][current.ct]
    coy <- cy0.data["corY"][[1]][current.ct]
    cox <- cy0.data["corX"][[1]][current.ct]
    urban.map.ext <-extent(cox-mapRange,cox+mapRange,coy-mapRange,coy+mapRange)
    
    mapp.cp <- crop(mapp.world, urban.map.ext)
    values(mapp.cp)[values(mapp.cp)<50]=NaN
    values(mapp.cp)[values(mapp.cp)>=50]=1
    sl2050=sum(na.omit(values(mapp.cp)))
    
    mapp.cp <- crop(mapp.world, urban.map.ext)
    values(mapp.cp)[is.na(values(mapp.cp))]=0
    values(mapp.cp)[values(mapp.cp)<=100]=NaN
    d0<-distance(mapp.cp)
        mapp.cp <- crop(mapp.world, urban.map.ext)
        for (sspn in c(1:5)){
        ct.area<-ua2050.data[[sspn]][current.ct]
        if (ct.area<=sl2050){
          print("shinking")
          new.urban<-shrinking(ct.area,sl2050,mapp.cp,d0)
        }else{
          print("sprawling")
          new.urban<-sprawling(ct.area,sl2050,mapp.cp,d0)
        }
        writeRaster(new.urban,paste(sspn,current.ct,".tif",sep = "_"),overwrite=TRUE)
  }
}

for (sspn in c(1:5)){
  nan.map.ext <-extent(125,125.1,19.3,19.4)
  urbans<-crop(mapp.world, nan.map.ext)
    for (current.ct in cnct){
      m.raster<-raster(paste(sspn,current.ct,".tif",sep = "_"))
      urbans<-mosaic(urbans, m.raster,fun=mean)
    }
  writeRaster(urbans,paste(sspn,"TOTAL",".tif",sep = "_"),overwrite=TRUE)
}
  

##########functions###########################
sprawling=function(ct.area,sl2030,mapp.cp0,d0){
  mapp.cp<-mapp.cp0
  totalg=length(values(d0))
  org=sum(na.omit(values(mapp.cp)>=111))
    limit.ua<-ct.area
    values(mapp.cp)[is.na(values(mapp.cp))]=-100000
    for (rank0 in c(org:(3*org))){
      current.v<-values(mapp.cp)[order(values(d0))[rank0]]
      if (current.v >= 50){next()}
      current.d<-values(d0)[order(values(d0))[rank0]]
      values(mapp.cp)[order(values(d0))[org:rank0]] <- values(mapp.cp)[order(values(d0))[org:rank0]]+0.1
      lowcount<-totalg-rank0
      low.d<-order(values(d0))[rank0:(rank0+lowcount%/%2)]
      values(mapp.cp)[low.d] <- values(mapp.cp)[low.d]+0.1*current.d/values(d0)[low.d]
      ua<-sum(na.omit(values(mapp.cp)>=50))
      if (ua >= limit.ua){
        print("---")
        print(ua)
        print(org)
        print(rank0)
        break()
      }
    }
    values(mapp.cp)[values(mapp.cp)>100]=100
    values(mapp.cp)[values(mapp.cp0)==111]=111
    values(mapp.cp)[is.na(values(mapp.cp0))]=NaN
  return(mapp.cp)
}

shrinking=function(ct.area,sl2030,mapp.cp0,d0){
    limit.ua<-ct.area
    mapp.cp <- mapp.cp0
    values(mapp.cp)[is.na(values(mapp.cp))]=0
    for (rank in c(0:8000)){
      current.v<-values(mapp.cp)[order(values(d0))[length(values(d0))-rank]]
      current.d<-values(d0)[order(values(d0))[length(values(d0))-rank]]
      if (current.v <= 0){next()}
      values(mapp.cp) <- values(mapp.cp)-current.v*values(d0)/current.d
      ua<-sum(na.omit(values(mapp.cp)>=50))
      if (ua <= limit.ua){break()}
    }
    print(paste(ua,limit.ua))
    values(mapp.cp)[values(mapp.cp0)==111]=111
    values(mapp.cp)[is.na(values(mapp.cp0))]=NaN
    values(mapp.cp)[values(mapp.cp)<0]=0
    return(mapp.cp)
}




##########Frame of study areas###########################
mapp <- raster(sleuth.data.path)
urban.out0<-urban.out
liss <- list()
for(c in c(1:114)){
  print(c)
  mapRange <- cy0.data$radius[c]
  cy <- cy0.data["city"][[1]][c]
  coy <- cy0.data["corY"][[1]][c]
  cox <- cy0.data["corX"][[1]][c]
  eg<-20
  mapRange <- mapRange +  0.008333333*eg
  urban.map.ext<-extent(cox-mapRange,cox+mapRange,coy-mapRange,coy+mapRange)
  urban.out<-crop(mapp, urban.map.ext)
  urban.out[1:(ncol(urban.out)*eg)]=120
  urban.out[(ncell(urban.out)-(ncol(urban.out)*eg)):ncell(urban.out)]=120
  cb<-c(1:3)
  for(i in c(1:nrow(urban.out))){
    temp<-i*ncol(urban.out)-eg
    cb<-append(cb,c(temp:(temp+eg*2)))
  }
  urban.out[cb]=120
  values(urban.out)[values(urban.out)<120]=NaN
  liss<-append(liss,urban.out)
  # urban.out<-urban.out0
}
urban.out0<-do.call(merge,liss)
writeRaster(urban.out0,"edge.tif",overwrite=TRUE)

