library(tidyverse)
library(fBasics)
library(sf)
library(geobr)
library(gstat)
library(fields)
library(ggspatial)


###################################################################################################################
#####################   DADOS PRECIPITAÇÃO DE DEZEMBRO A MARÇO DE 2017/2018       #################################
###################################################################################################################

##DADOS PRECIPITAÇÃO MENSAL (MEDIA DIÁRIA) DE DEZEMBRO A MARÇO DE 2017/2018
dados_mensal_dez <- read_delim("media_mensal_pluvi_dez.csv", delim = ",")
dados_mensal_jan <- read_delim("media_mensal_pluvi_jan.csv", delim = ",")
dados_mensal_fev <- read_delim("media_mensal_pluvi_fev.csv", delim = ",")
dados_mensal_mar <- read_delim("media_mensal_pluvi_mar.csv", delim = ",")

head(dados_mensal_dez)
dim(dados_mensal_dez)

head(dados_mensal_jan)
dim(dados_mensal_jan)

head(dados_mensal_fev)
dim(dados_mensal_fev)

head(dados_mensal_mar)
dim(dados_mensal_mar)


###################################################################################################################
#############################   ESTATÍSTICAS DEZEMBRO A MARÇO DE 2017/2018    #####################################
###################################################################################################################

##Estatísticas mensal dezembro
basicStats(dados_mensal_dez$valorMedida)

##Estatísticas mensal janeiro
basicStats(dados_mensal_jan$valorMedida)

##Estatísticas mensal fevereiro
basicStats(dados_mensal_fev$valorMedida)

##Estatísticas mensal março
basicStats(dados_mensal_mar$valorMedida)


##Histogramas
hist(dados_mensal_dez$valorMedida, main = "Medições em Dezembro", xlab = "precipitação (mm)", ylab = "Ocorrências", col = "lightblue")
hist(dados_mensal_jan$valorMedida, main = "Medições em Janeiro", xlab = "precipitação (mm)", ylab = "Ocorrências", col = "lightblue", xlim = c(0,10), ylim = c(0,20))
hist(dados_mensal_fev$valorMedida, main = "Medições em Fevereiro", xlab = "precipitação (mm)", ylab = "Ocorrências", col = "lightblue")
hist(dados_mensal_mar$valorMedida, main = "Medições em Março", xlab = "precipitação (mm)", ylab = "Ocorrências", col = "lightblue", ylim = c(0,25))


##Medições por estação
barplot(dados_mensal_dez$valorMedida, main = "Medições em Dezembro por Estação", xlab = "Estações", ylab = "precipitação (mm)", ylim = c(0,12))
barplot(dados_mensal_jan$valorMedida, main = "Medições em Janeiro por Estação", xlab = "Estações", ylab = "precipitação (mm)", ylim = c(0,12))
barplot(dados_mensal_fev$valorMedida, main = "Medições em Fevereiro por Estação", xlab = "Estações", ylab = "precipitação (mm)", ylim = c(0,12))
barplot(dados_mensal_mar$valorMedida, main = "Medições em Março por Estação", xlab = "Estações", ylab = "precipitação (mm)", ylim = c(0,12))


##Boxplot
boxplot(dados_mensal_dez$valorMedida, main = "Medições em Dezembro", xlab = "precipitação (mm)", horizontal = T, col = "lightblue")
boxplot(dados_mensal_jan$valorMedida, main = "Medições em Janeiro", xlab = "precipitação (mm)", horizontal = T, col = "lightblue")
boxplot(dados_mensal_fev$valorMedida, main = "Medições em Fevereiro", xlab = "precipitação (mm)", horizontal = T, col = "lightblue")
boxplot(dados_mensal_mar$valorMedida, main = "Medições em Março", xlab = "precipitação (mm)", horizontal = T, col = "lightblue")



###################################################################################################################
########################   MAPA DAS ESTAÇÕES PLUVIOMÉTRICAS NA CIDADE DE SÃO PAULO   ##############################
###################################################################################################################
##Limite territorial da cidade de São Paulo
limite_SP <- read_municipality(code_muni = 3550308)


##Plota o limite da cidade de São Paulo
plot(limite_SP$geom) 


##Tranforma os dados em sf
dados_mensal_dez_sf <- st_as_sf(dados_mensal_dez, coords = c("longitude", "latitude"), crs = 4674)



##Mapa com os pontos atuais (pluviometros)
ggplot(dados_mensal_dez_sf) + geom_sf()+ 
  geom_sf(data = limite_SP, fill = "transparent") + theme_bw()+
  annotation_scale(location = "br") + annotation_north_arrow(location = "tr", style = north_arrow_nautical,
                                                             height = unit(1, "cm"), width = unit(1,"cm"))+
  labs(title = "Estações Pluviométricas - SP", color = "mm")


##Mapa com os pontos atuais e suas respectivas medidas em mm (pluviometros)
ggplot(dados_mensal_dez_sf) + geom_sf(aes(color = valorMedida), cex = 3) + 
  geom_sf(data = limite_SP, fill = "transparent") + theme_bw()+ 
  scale_color_gradientn(colors = tim.colors(20),limits = c(0,7))+
  annotation_scale(location = "br") + annotation_north_arrow(location = "tr", style = north_arrow_nautical,
                                                             height = unit(1, "cm"), width = unit(1,"cm"))+
  labs(title = "Estações Pluviométricas - SP", color = "mm")




###################################################################################################################
########################   TRANSFORMAÇÃO PARA COORDENADAS PLANARES     ############################################
###################################################################################################################

##Transformação de dados geo em coordenadas UTM
coordenadas <- SpatialPoints(cbind(dados_mensal_dez$longitude, dados_mensal_dez$latitude), proj4string = CRS("+proj=longlat"))
coordenadas

coordenadasUTM <- spTransform(coordenadas, CRS("+init=epsg:32723"))
coordenadasUTM

##Plota coords geo e UTM para verificar se a transformação foi realizada de maneira correta
par(mfrow = c(1,2))
plot(coordenadas, axes = T, cex.axis = 0.95)
plot(coordenadasUTM, axes = T, cex.axis = 0.95)

##Tranforma em matriz
coordenadasUTM <- as.data.frame(coordenadasUTM)
colnames(coordenadasUTM) <- c("x", "y")
coordenadasUTM

##Atualiza a matriz de dados com coord UTM
dados_mensal_dez_utm <- dados_mensal_dez
dados_mensal_dez_utm$longitude <- coordenadasUTM$x
dados_mensal_dez_utm$latitude  <- coordenadasUTM$y
dados_mensal_dez_utm

dados_mensal_jan_utm <- dados_mensal_jan
dados_mensal_jan_utm$longitude <- coordenadasUTM$x
dados_mensal_jan_utm$latitude  <- coordenadasUTM$y
dados_mensal_jan_utm

dados_mensal_fev_utm <- dados_mensal_fev
dados_mensal_fev_utm$longitude <- coordenadasUTM$x
dados_mensal_fev_utm$latitude  <- coordenadasUTM$y
dados_mensal_fev_utm

dados_mensal_mar_utm <- dados_mensal_mar
dados_mensal_mar_utm$longitude <- coordenadasUTM$x
dados_mensal_mar_utm$latitude  <- coordenadasUTM$y
dados_mensal_mar_utm

#unifica os dados de todos os meses em uma tabela
dados_mensais_utm = data.frame(id = dados_mensal_dez_utm$codEstacao, 
                           valorMedidaDez = dados_mensal_dez_utm$valorMedida, 
                           valorMedidaJan = dados_mensal_jan_utm$valorMedida, 
                           valorMedidaFev = dados_mensal_fev_utm$valorMedida, 
                           valorMedidaMar = dados_mensal_mar_utm$valorMedida,
                           longitude = dados_mensal_dez_utm$longitude,
                           latitude = dados_mensal_dez_utm$latitude)


##Salva em CSV
write_csv(dados_mensal_dez_utm, "dados_mensal_dez_UTM.csv")
write_csv(dados_mensal_jan_utm, "dados_mensal_jan_UTM.csv")
write_csv(dados_mensal_fev_utm, "dados_mensal_fev_UTM.csv")
write_csv(dados_mensal_mar_utm, "dados_mensal_mar_UTM.csv")

write_csv(dados_mensais_utm, "dados_mensais_todos_meses_utm.csv")


################################################################################
