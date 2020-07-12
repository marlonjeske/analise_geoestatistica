library(tidyverse)
library(sp)
library(fBasics)
library(caret)
library(sf)
library(geobr)
library(geosphere)
library(rgdal)
library(ggspatial)
library(gstat)
library(scales)
library(gridExtra)



###################################################################################################################
#####################   DADOS PRECIPITAÇÃO DE DEZEMBRO A MARÇO DE 2017/2018       #################################
###################################################################################################################

##DADOS PRECIPITAÇÃO MENSAL (MEDIA DIÁRIA) DE DEZEMBRO A MARÇO DE 2017/2018
dados_mensal_dez <- read_delim("dados_mensal_dez_UTM.csv", delim = ",")
dados_mensal_jan <- read_delim("dados_mensal_jan_UTM.csv", delim = ",")
dados_mensal_fev <- read_delim("dados_mensal_fev_UTM.csv", delim = ",")
dados_mensal_mar <- read_delim("dados_mensal_mar_UTM.csv", delim = ",")

head(dados_mensal_dez)
dim(dados_mensal_dez)


###################################################################################################################
###################################   DADOS PARA ANÁLISE GEOESTATÍSTICA   #########################################
###################################################################################################################


#dados para análise geoestatística

dados_geo_dez = dados_mensal_dez[, c(3,4,2)]
colnames(dados_geo_dez) = c("x","y","z")
head(dados_geo_dez)

dados_geo_jan = dados_mensal_jan[, c(3,4,2)]
colnames(dados_geo_jan) = c("x","y","z")
head(dados_geo_jan)

dados_geo_fev = dados_mensal_fev[, c(3,4,2)]
colnames(dados_geo_fev) = c("x","y","z")
head(dados_geo_fev)

dados_geo_mar = dados_mensal_mar[, c(3,4,2)]
colnames(dados_geo_mar) = c("x","y","z")
head(dados_geo_mar)

#salva como dataframe
dados_geo_df_dez = dados_geo_dez
dados_geo_df_jan = dados_geo_jan
dados_geo_df_fev = dados_geo_fev
dados_geo_df_mar = dados_geo_mar

#cria sistema de coordenadas
sp::coordinates(dados_geo_dez) = ~x+y
sp::coordinates(dados_geo_jan) = ~x+y
sp::coordinates(dados_geo_fev) = ~x+y
sp::coordinates(dados_geo_mar) = ~x+y

#plot gráfico de bolhas: coordenadas x medição
sp::bubble(dados_geo_dez, "z", main="Precipitação (mm) - Dezembro")
sp::bubble(dados_geo_jan, "z", main="Precipitação (mm) - Janeiro")
sp::bubble(dados_geo_fev, "z", main="Precipitação (mm) - Fevereiro")
sp::bubble(dados_geo_mar, "z", main="Precipitação (mm) - Março")


###################################################################################################################
##############################   CRIAÇÃO DO GRID PARA INTERPOLAÇÃO       ##########################################
###################################################################################################################

##Limite territorial da cidade de São Paulo
limite_SP <- read_municipality(code_muni = 3550308)
plot(limite_SP$geom)

#coordenadas do contorno de SP em data frame
limite_SP_coord = as.data.frame(st_coordinates(limite_SP$geom))
head(limite_SP_coord)

#coordenadas em Spatial Points
coord_sp <- SpatialPoints(cbind(limite_SP_coord$X, limite_SP_coord$Y), proj4string = CRS("+proj=longlat"))
coord_sp

##coordenadas em UTM
coordUTM_sp <- spTransform(coord_sp, CRS("+init=epsg:32723"))
coordUTM_sp



## GRID

#Contorno de SP em Spatial Polygon
poligono <- as(limite_SP, "Spatial")
proj4string(poligono) <- CRS(proj4string(coord_sp))
poligono

#cria grid com formato de SP em coordenada UTM. Cellsize do grid em metros
poligono <- spTransform(poligono, CRS("+init=epsg:32723 +units=m"))
grid_sp <- sp::spsample(poligono, cellsize=2000, type="regular")
plot(poligono)
points(grid_sp)

proj4string(grid_sp) <- proj4string(dados_geo_dez)
gridded(grid_sp)     <- TRUE  # Create SpatialPixel object
#fullgrid(grid_sp)    <- TRUE  # Create SpatialGrid object
plot(grid_sp, axes = T, main="Grid - Cidade de São Paulo", ylab="latitude", xlab="longitude")



#Plot do contorno de SP

limite_SP_utm = limite_SP %>% st_transform(32723)

ggplot() + geom_sf(data = limite_SP_utm) + aes(fill = 1) +
  theme(panel.grid.major = element_line(color = "white")) +
  scale_fill_gradientn(colors = sf.colors(20))




###################################################################################################################
######################################   ANÁLISE GEOESTATÍSTICA     ###############################################
###################################################################################################################


#escolha o mês para a análise geoestatística:
#dados_geo = dados_geo_dez
#dados_geo = dados_geo_jan
#dados_geo = dados_geo_fev
dados_geo = dados_geo_mar


##Max e Min distância dos pluviômetros
dist = dist(dados_geo@coords)
hist(dist, main = "Histograma - Distância entre pluviômetros", ylab = "Ocorrências", xlab="Distância entre pluviômetros em metros", xlim=c(0, 60000), ylim=c(0,350))
boxplot(dist, horizontal = T)
summary(dist)

max(dist) ## max dist = 51999 (limita distância do variograma)
min(dist) ## min dist = 1022 (auxilia na definição do lag)


##Análise variográfica
g <- gstat(id="mm", formula = z ~ 1, data = dados_geo)
g


##Semivariograma de nuvem: todos os valores de semivariância entre os pares de pontos
var_cloud <- gstat::variogram(g, cloud=T)
plot(var_cloud, xlab="Distância (m)", ylab="Semivariância", main="Semivariograma de nuvem - Março")


##Verifica anisotropia: Mapa variografico (tendencias e anisotropias)
var_map <- gstat::variogram(g, cutoff = 52000, width = 5000, map  = T)
plot(var_map, xlab="", ylab="", main="Mapa variográfico - Março")


##Variograma experimental: cutoff = distância máxima #width = tamanho do lag
var_exp <- gstat::variogram(g, cutoff = 52000, width = 5000)
plot(var_exp, xlab="Distância (m)", ylab="Semivariância", main="Variograma experimental - Março")

var_exp
summary(var_exp)


#####################

#Modelos teóricos

#Calibra o variograma teórico testando modelos
fit_var <- fit.variogram(var_exp, vgm(c("Sph","Exp","Gau","Mat","Ste","Wav")), fit.kappa = TRUE)
fit_var

#Modelo teórico com valores obtidos pelo fit_var
#fit_model <- fit.variogram(var_exp, vgm(5.519918, "Mat", 29057.17, nugget = 2.062187, kappa = 0.8))
#fit_model <- fit.variogram(var_exp, vgm(6.471956, "Mat", 16510.72, nugget = 3.946518, kappa = 1.1))
#fit_model <- fit.variogram(var_exp, vgm(0.7773687, "Sph", 19797.24, nugget = 0.2513898))
fit_model <- fit.variogram(var_exp, vgm(6.726619, "Ste", 13898.82, nugget = 1.580426, kappa = 0.3))

plot(var_exp, fit_model, xlab="Distância (m)", ylab="Semivariância", main="Semivariograma - Março")
fit_model


##Validação do modelo

set.seed(2321)

#configuração para validação leave-one-out (nfold = nrows)
qtd_rows <- dim(dados_geo)[1]

xval = krige.cv(z ~ 1, locations = dados_geo, model = fit_model, nfold = qtd_rows)

head(xval)
dim(xval)

summary(xval$residual)

#soma dos resíduos
sum(abs(xval$residual)) 
# mean error, ideally 0:
mean(xval$residual)
# MSPE, ideally small
mean(xval$residual^2)
# Mean square normalized error, ideally close to 1
mean(xval$zscore^2)




## Krigagem ordinária

ko = krige(z ~ 1, dados_geo, newdata = grid_sp, model = fit_model)
ko
dim(ko)

#preparar dados da krigagem para plot
krig.output=as.data.frame(ko)
names(krig.output)[1:4]<-c("longitude","latitude","predicao","variancia")
head(krig.output)

#write_csv(krig.output, "resultados_krigagem_dez.csv")


#plot da predição e variância da krigagem
cores <- rev(sf.colors(20))

plot_utm_predict = ggplot(data = krig.output, aes(x = longitude, y = latitude)) + 
  geom_tile(data = krig.output, aes(fill = predicao) ) + 
  geom_point(data = as.data.frame(dados_geo), aes(x=x, y=y), pch=16, col = "black") +
  scale_fill_gradientn(colors = cores) + coord_equal() + ggtitle("Predição - Março")


plot_utm_var = ggplot(data = krig.output, aes(x = longitude, y = latitude)) + 
  geom_tile(data = krig.output, aes(fill = variancia)) + 
  geom_point(data = as.data.frame(dados_geo), aes(x=x, y=y), pch=16, col = "black") +
  scale_fill_gradientn(colors = cores) + coord_equal() + ggtitle("Variância - Março")


grid.arrange(plot_utm_predict, plot_utm_var, ncol=2)


