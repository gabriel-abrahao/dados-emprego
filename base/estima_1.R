library(spdep)
library(maptools)
library(sphet)
library(lmtest)
library(plyr)
library(rgdal) #Se estiver dando problema, trocar os comandos readOGR e writeOGR por readShapePoly e writeShapePoly

#Localizacao da pasta do repositorio, com uma barra no final
#repofolder = "/home/gabriel/larissa/dados-emprego/"
repofolder = "C:/Users/laris/Desktop/Artigo/dados-emprego/"

#Nomes do shape e da base
shpfname = paste0(repofolder,"/munis2010/clima_munis2010.shp")
basfname = paste0(repofolder,"/base/merge1008sem_missing.csv")

#Shape e base de saida
outshpfname = paste0(repofolder,"/munis2010/clima_munis2010_limpo.shp")
outbasfname = paste0(repofolder,"/base/merge1008sem_missing_limpo.csv")

#Lendo a base
base = read.csv(basfname)
#Removendo a variavel pi e substituindo missings por zero
colnames(base)[colnames(base) == "pi"] = "pia"
base$monocul[is.na(base$monocul)] <- 0
base$areaplantada[is.na(base$areaplantada)] <- 0
base$princultu[is.na(base$princultu)] <- 0

#Lendo o shape
#inshape = readShapePoly(shpfname)
inshape = readOGR(shpfname) #Trocar pelo de cima

#Removendo os poligonos de municipios que nao existem na base
shape = subset(inshape,codigo_ibg %in% base$munic)

#Salva o shape e a base, desnecessario, descomentar se precisar
#writeSpatialShape(shape,outshpfname)
writeOGR(shape,outshpfname,layer=outshpfname,driver="ESRI Shapefile") #Trocar pelo de cima
write.csv(base,outbasfname,row.names = F)

#Faz as variaveis da base ficarem acessiveis. CUIDADO: Use o detach depois se for mexer na base antes de estiamar de novo. Ou reinicie a sessao.
attach(base)

#Formula do modelo, a ser usada em todos as estimacoes
#formula = as.formula("txerna ~ mediaanosest + txenergia + monocul + poplog + arealog + precclimme + tempclimme + anomaprec + anomatemp")
formula = as.formula("txerna ~ mediaanosest + txenergia + monocul + poplog + arealog + precclimme + tempclimme + anomaprec + anomatemp")


#Estima o mqo
mqo = lm(formula)
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
moran.test(base$txerna,w)

#Faz os testes de erro e lag
tests = lm.LMtests(mqo, w, test='all')
summary(tests)

#Estima com maxima verossimilhanca, usando o metodo de decomposical LU (LENTO DEMAIS NO PADRAO eigen)
mlagmle = lagsarlm(formula,base,w,quiet=F,method="LU")
summary(mlagmle)
#Estima com GMM
mlaggmm = spreg(formula,base,w,model='lag')
summary(mlaggmm)

#Teste de moran nos residuos da saida
moran.test(resid(mlaggmm),w)

detach(base)
