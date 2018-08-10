library(spdep)
library(maptools)
library(sphet)
library(lmtest)
library(rgdal) #Se estiver dando problema, trocar os comandos readOGR e writeOGR por readShapePoly e writeShapePoly

#Nomes do shape e da base
shpfname = "/home/gabriel/larissa/dados-emprego/munis2010/clima_munis2010.shp"
basfname = "/home/gabriel/larissa/dados-emprego/base/merge1008sem_missing.csv"

#Shape e base de saida
outshpfname = "/home/gabriel/larissa/dados-emprego/munis2010/clima_munis2010_limpo.shp"
outbasfname = "/home/gabriel/larissa/dados-emprego/base/merge1008sem_missing_limpo.csv"

#Lendo a base
base = read.csv(basfname)
#Removendo a variavel pi e substituindo missings por zero
base$pi <- NULL
base$monocul[is.na(base$monocul)] <- 0
base$areaplantada[is.na(base$areaplantada)] <- 0
base$princultu[is.na(base$princultu)] <- 0

#Lendo o shape
#inshape = readShapePoly(shpfname)
inshape = readOGR(shpfname) #Trocar pelo de cima
#Removendo os poligonos de municipios que nao existem na base
shape = subset(inshape,codigo_ibg %in% base$munic)
#shape$data <-

#Salva o shape e a base, desnecessario
#writeSpatialShape(shape,outshpfname)
writeOGR(shape,outshpfname,layer=outshpfname,driver="ESRI Shapefile") #Trocar pelo de cima
write.csv(base,outbasfname,row.names = F)

#Faz as variaveis da base ficarem acessiveis. CUIDADO: Use o detach depois se for mexer na base antes de estiamar de novo. Ou reinicie a sessao.
attach(base)

#Estima o mqo
mqo = lm(txerna ~ mediaanosest + txenergia + monocul + poplog + arealog + precclimme + tempclimme + anomaprec + anomatemp)
summary(mqo)

#Matriz de vizinhanca queen ordem 1
queen1 = poly2nb(shape,queen=T,snap=0.02)
summary(queen1)
#Plot as vizinhancas
plot(queen1,coordinates(shape))

#Transforma a matriz de vizinhos em pesos
w<-nb2listw(queen1, style="W", zero.policy=F)
#Teste de moran nos residuos do modelo
lm.morantest(mqo,w)
#moran.test(resid(mqo),w) #Da igual

#Faz os testes de erro e lag
tests = lm.LMtests(mqo, w, test='all')
summary(tests)

#Estima com maxima verossimilhanca, usando o metodo de decomposical LU (LENTO DEMAIS NO PADRAO eigen)
mlagmle = lagsarlm(txerna ~ mediaanosest + txenergia + monocul + poplog + arealog + precclimme + tempclimme + anomaprec + anomatemp,base,w,quiet=F,method="LU")
summary(mlagmle)
#Estima com GMM
mlag = spreg(txerna ~ mediaanosest + txenergia + monocul + poplog + arealog + precclimme + tempclimme + anomaprec + anomatemp,base,w,model='lag')
summary(mlag)

#Teste de moran nos residuos da saida
moran.test(resid(mlag),w)

detach(base)
