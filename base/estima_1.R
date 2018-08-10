library(spdep)
library(maptools)
library(sphet)


shpfname = "/home/gabriel/larissa/dados-emprego/munis2010/clima_munis2010.shp"
basfname = "/home/gabriel/larissa/dados-emprego/base/merge1008sem_missing.csv"

outshpfname = "/home/gabriel/larissa/dados-emprego/munis2010/clima_munis2010_limpo.shp"
outbasfname = "/home/gabriel/larissa/dados-emprego/base/merge1008sem_missing_limpo.csv"


base = read.csv(basfname)
#base = subset(base,munic!=697)
base$pi <- NULL
base$monocul[is.na(base$monocul)] <- 0
base$areaplantada[is.na(base$areaplantada)] <- 0
base$princultu[is.na(base$princultu)] <- 0
#base = na.omit(base)

inshape = readShapePoly(shpfname)
shape = subset(inshape,codigo_ibg %in% base$munic)
#shape$data <-

#Saida
writeSpatialShape(shape,outshpfname)
write.csv(base,outbasfname,row.names = F)

attach(base)

mqo = lm(txerna ~ mediaanosest + txenergia + monocul + poplog + arealog + precclimme + tempclimme + anomaprec + anomatemp)
summary(mqo)


queen1 = poly2nb(shape,queen=T,snap=0.02)
summary(queen1)
plot(queen1,coordinates(shape))

w<-nb2listw(queen1, style="W", zero.policy=F)
lm.morantest(mqo,w)
#moran.test(resid(mqo),w)

tests = lm.LMtests(mqo, w, test='all')
summary(tests)

#mlag = lagsarlm(txerna ~ mediaanosest + txenergia + monocul + poplog + arealog + precclimme + tempclimme + anomaprec + anomatemp,base,w)
mlag = spreg(txerna ~ mediaanosest + txenergia + monocul + poplog + arealog + precclimme + tempclimme + anomaprec + anomatemp,base,w,model='lag')
summary(mlag)


detach(base)
