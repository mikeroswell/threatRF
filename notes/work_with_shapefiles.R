# install.packages("fasterize")
library(rgdal)
library(raster)
library(tictoc)
tryshape<-readOGR("data/LULC_2010/")
# create a raster
myrast<-raster()
extent(myrast)<-extent(tryshape)
res(myrast)<-1 #set to 1 m resolution

tic()
lulc2010<-rasterize(tryshape, myrast)
toc()


res(myrast)<-10 #set to 10 m resolution
tic()
lulc2010_med<-rasterize(tryshape, myrast)
toc()


res(myrast)<-30 #set to 10 m resolution
tic()
lulc2010_30<-rasterize(tryshape, myrast)
toc()


res(myrast)<-100 #set to 10 m resolution
tic()
lulc2010_100<-rasterize(tryshape, myrast)
toc()
