library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(caTools)
library(icesTAF)

install.packages("rgdal")
install.packages("rgeos")
install.packages("caTools")
install.packages("icesTAF")
install.packages("")


cy0.data.path <- "cors.csv"
cy0.data <- read.csv(cy0.data.path, check.names=FALSE)


start.year <-1999
end.year<-2017

year=2012
c=6
plot(mapp.cp)
for (year in c(start.year:end.year)){
  sleuth.data.path<-sprintf("/Users/kaso/Downloads/R/LandScan/lspop%s.tif",year)
  mapp <- raster(sleuth.data.path)
  column.blank1 <- rep.int(0, 143)
  for (c in c(1:143)){
    mapRange <- 0.0435*(189-c)**0.5
    cy <- cy0.data["city"][[1]][c]
    coy <- cy0.data["corY"][[1]][c]
    cox <- cy0.data["corX"][[1]][c]
    
    urban.map.ext <-extent(cox-mapRange,cox+mapRange,coy-mapRange,coy+mapRange)
    mapp.cp <- crop(mapp, urban.map.ext)
    
    values(mapp.cp)[values(mapp.cp)<1000]=0
    values(mapp.cp)[values(mapp.cp)>=1000]=1

    alal=sum(na.omit(values(mapp.cp)))
    column.blank1[c] <- alal
  }
  clm.name1 <- sprintf("%s_ls",year)
  cy0.data[, clm.name1] <- column.blank1
}

write.csv(cy0.data,"ls_hist.csv")
