# gis notes

#at least for just mapping points, cutting raster size makes a big difference, but different packages do the same.

tic()
k<-terra::extract(focused,localities)
toc()
#1.6 sec

tic()
l<-terra::extract(bc,localities)
toc()
# 46.6 sec

tic()
m<-raster::extract(focused,localities)
toc()
# 1.6 sec

tic()
n<- raster::extract(bc,localities)
toc()
# 46.5 sec
