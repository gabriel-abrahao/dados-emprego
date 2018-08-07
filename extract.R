library(maptools)
library(raster)
library(ncdf4)

reporoot = "/home/gabriel/larissa/dados-emprego/"

shpfname = paste0(reporoot,"munis2010/munis2010.shp")

poly <- readShapePoly(shpfname)

rast = stack(paste0(reporoot,"prec.year.clim.nc"))
plot(rast)

ex = extract(rast,poly,fun=mean,na.rm=TRUE,df=TRUE) 

exsum = extract(rast,poly,fun=sum,na.rm=TRUE,df=TRUE)
excnt = extract(rast,poly,fun=num,na.rm=TRUE,df=TRUE) 
