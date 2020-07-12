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
#####################   DADOS DOS PLUVI�METROS SELECIONADOS PELA OTIMIZA��O       #################################
###################################################################################################################

dados = read_csv("resultado_otimizacao_59pluviometros_rankeados.csv")

head(dados)
dim(dados)



summary(dados)

hist(dados$valorMedidaDez)
hist(dados$valorMedidaJan)
hist(dados$valorMedidaFev)
hist(dados$valorMedidaMar)


###################################################################################################################
###################################   DADOS PARA AN�LISE GEOESTAT�STICA   #########################################
###################################################################################################################

#dados para an�lise geoestat�stica

dados_geo_dez = dados[, c(6,7,2) ]
colnames(dados_geo_dez) = c("x","y","z")
head(dados_geo_dez)

dados_geo_jan = dados[, c(6,7,3) ]
colnames(dados_geo_jan) = c("x","y","z")
head(dados_geo_jan)

dados_geo_fev = dados[, c(6,7,4) ]
colnames(dados_geo_fev) = c("x","y","z")
head(dados_geo_fev)

dados_geo_mar = dados[, c(6,7,5) ]
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

#plot gr�fico de bolhas: coordenadas x medi��o
sp::bubble(dados_geo_dez, "z")
sp::bubble(dados_geo_jan, "z")
sp::bubble(dados_geo_fev, "z")
sp::bubble(dados_geo_mar, "z")


###################################################################################################################
##############################   CRIA��O DO GRID PARA INTERPOLA��O       ##########################################
###################################################################################################################

##Limite territorial da cidade de S�o Paulo
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
plot(grid_sp, axes = T, main="Grid - Cidade de S�o Paulo", ylab="latitude", xlab="longitude")



#Plot do contorno de SP

limite_SP_utm = limite_SP %>% st_transform(32723)

ggplot() + geom_sf(data = limite_SP_utm) + aes(fill = 1) +
  theme(panel.grid.major = element_line(color = "white")) +
  scale_fill_gradientn(colors = sf.colors(20))




###################################################################################################################
######################################   AN�LISE GEOESTAT�STICA     ###############################################
###################################################################################################################


#escolha o m�s para a an�lise geoestat�stica:
#dados_geo = dados_geo_dez
#dados_geo = dados_geo_jan
#dados_geo = dados_geo_fev
dados_geo = dados_geo_mar


##Max e Min dist�ncia dos pluvi�metros
dist = dist(dados_geo@coords)
hist(dist, main = "Histograma - Dist�ncia entre pluvi�metros", ylab = "Ocorr�ncias", xlab="Dist�ncia entre pluvi�metros em metros", xlim=c(0, 80000), ylim=c(0,300))
boxplot(dist, horizontal = T)
summary(dist)

max(dist) ## max dist = 70710 (limita dist�ncia do variograma)
min(dist) ## min dist = 1022 (auxilia na defini��o do lag)


##An�lise variogr�fica
g <- gstat(id="mm", formula = z ~ 1, data = dados_geo)
g


##Semivariograma de nuvem: todos os valores de semivari�ncia entre os pares de pontos
var_cloud <- gstat::variogram(g, cloud=T)
plot(var_cloud, xlab="Dist�ncia (m)", ylab="Semivari�ncia", main="Semivariograma de nuvem - Dezembro")


##Verifica anisotropia: Mapa variografico (tendencias e anisotropias)
var_map <- gstat::variogram(g, cutoff = 71000, width = 5000, map  = T)
plot(var_map, xlab="", ylab="", main="Mapa variogr�fico - Dezembro")


##Variograma experimental: cutoff = dist�ncia m�xima #width = tamanho do lag
var_exp <- gstat::variogram(g, cutoff = 71000, width = 5000)
plot(var_exp, xlab="Dist�ncia (m)", ylab="Semivari�ncia", main="Variograma experimental - Dezembro")

var_exp
summary(var_exp)


#####################

#Modelos te�ricos

#Calibra o variograma te�rico testando modelos
fit_var <- fit.variogram(var_exp, vgm(c("Sph","Exp","Gau","Mat","Ste","Wav")), fit.kappa = TRUE, fit.sills = T, fit.ranges = T)
fit_var

#Modelo te�rico com valores obtidos pelo fit_var
#fit_model <- fit.variogram(var_exp, vgm(20.8048673, "Sph", 332130.3, nugget = 0.8474923))
#fit_model <- fit.variogram(var_exp, vgm(5.625517, "Sph", 40552.81, nugget = 1.897947))
#fit_model <- fit.variogram(var_exp, vgm(0.6135851, "Ste", 7667.023, nugget = 0.3162204, kappa = 4.3))
fit_model <- fit.variogram(var_exp, vgm(2.549182, "Ste", 13135.77, nugget = 4.720129, kappa = 5))
plot(var_exp, fit_model, xlab="Dist�ncia (m)", ylab="Semivari�ncia", main="Semivariograma - Dezembro")
fit_model


##Valida��o do modelo

set.seed(2321)

#configura��o para valida��o leave-one-out (nfold = nrows)
qtd_rows <- dim(dados_geo)[1]

xval = krige.cv(z ~ 1, locations = dados_geo, model = fit_model, nfold = qtd_rows)

head(xval)
dim(xval)

summary(xval$residual)

#soma dos res�duos
sum(abs(xval$residual)) 
# mean error, ideally 0:
mean(xval$residual)
# MSPE, ideally small
mean(xval$residual^2)
# Mean square normalized error, ideally close to 1
mean(xval$zscore^2)




## Krigagem ordin�ria

ko = krige(z ~ 1, dados_geo, newdata = grid_sp, model = fit_model)
ko
dim(ko)

#preparar dados da krigagem para plot
krig.output=as.data.frame(ko)
names(krig.output)[1:4]<-c("longitude","latitude","predicao","variancia")
head(krig.output)

write_csv(krig.output, "resultados_krigagem_otimizados_mar.csv")


#plot da predi��o e vari�ncia da krigagem
cores <- rev(sf.colors(20))

plot_utm_predict = ggplot(data = krig.output, aes(x = longitude, y = latitude)) + 
  geom_tile(data = krig.output, aes(fill = predicao) ) + 
  geom_point(data = as.data.frame(dados_geo), aes(x=x, y=y), pch=16, col = "black") +
  scale_fill_gradientn(colors = cores) + coord_equal() + ggtitle("Predi��o - Mar�o", subtitle = "59 Esta��es redistribu�das")


plot_utm_var = ggplot(data = krig.output, aes(x = longitude, y = latitude)) + 
  geom_tile(data = krig.output, aes(fill = variancia)) + 
  geom_point(data = as.data.frame(dados_geo), aes(x=x, y=y), pch=16, col = "black") +
  scale_fill_gradientn(colors = cores) + coord_equal() + ggtitle("Vari�ncia - Mar�o", subtitle = "59 Esta��es redistribu�das")


grid.arrange(plot_utm_predict, plot_utm_var, ncol=2)
