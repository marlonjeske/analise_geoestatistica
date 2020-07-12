library(tidyverse)
library(entropy)
library(fBasics)
library(gridExtra)
library(geosphere)
library(geoR)
library(plot.matrix)
library(ggplot2)
library(fields)
library(sf)
library(rgdal)
library(geobr)
library(ggspatial)



###############################   ENTRADA DE DADOS  


#Dados das estações pluviométricas
dados_mensais_dez = read_csv("dados_mensal_dez_UTM.csv")
dados_mensais_jan = read_csv("dados_mensal_jan_UTM.csv")
dados_mensais_fev = read_csv("dados_mensal_fev_UTM.csv")
dados_mensais_mar = read_csv("dados_mensal_mar_UTM.csv")
head(dados_mensais_dez)  
dim(dados_mensais_dez)

#Resultado da predição (krigagem) - Dezembro
dados_dez = read_csv("resultados_krigagem_dez.csv")
head(dados_dez)  
dim(dados_dez)

#Resultado da predição (krigagem) - Janeiro
dados_jan = read_csv("resultados_krigagem_jan.csv")
head(dados_jan)  
dim(dados_jan)

#Resultado da predição (krigagem) - Fevereiro
dados_fev = read_csv("resultados_krigagem_fev.csv")
head(dados_fev)  
dim(dados_fev)

#Resultado da predição (krigagem) - Março
dados_mar = read_csv("resultados_krigagem_mar.csv")
head(dados_mar)  
dim(dados_mar)




############################################### COMPACTAÇÃO DE DADOS

# COMPACTAÇÃO DE DADOS MENSAIS
dados_mensais = data.frame(id = dados_mensais_dez$codEstacao,
                         valorMedidaDez = dados_mensais_dez$valorMedida,
                         valorMedidaJan = dados_mensais_jan$valorMedida,
                         valorMedidaFev = dados_mensais_fev$valorMedida,
                         valorMedidaMar = dados_mensais_mar$valorMedida,
                         longitude = dados_mensais_dez$longitude,
                         latitude = dados_mensais_dez$latitude)

head(dados_mensais)
dim(dados_mensais)


# COMPACTAÇÃO DE DADOS DA KRIGAGEM
dados_krig <- data.frame(longitude = dados_dez$longitude,
                        latitude = dados_dez$latitude,
                        predicaoDez = dados_dez$predicao,
                        predicaoJan = dados_jan$predicao,
                        predicaoFev = dados_fev$predicao,
                        predicaoMar = dados_mar$predicao,
                        varianciaDez = dados_dez$variancia,
                        varianciaJan = dados_jan$variancia,
                        varianciaFev = dados_fev$variancia,
                        varianciaMar = dados_mar$variancia)

head(dados_krig)
dim(dados_krig)

nrows_krig = dim(dados_krig)[1]

############################################### CALCULAR VARIÂNCIA TOTAL

#Calcular variância total (soma das variâncias)
dados_krig$varianciaTotal <- NA
for(i in 1:nrows_krig){
  dados_krig[i, 11] <- dados_krig[i,7] + dados_krig[i,8] + dados_krig[i,9] + dados_krig[i,10]
}

head(dados_krig)

summary(dados_krig$varianciaTotal)


#ordenar por variância descrescente
dados_krig_order = dados_krig[order(-dados_krig$varianciaTotal), ]
head(dados_krig_order, 15)

write_csv(dados_krig_order, "variancia_total_krigagem_ordenado.csv")




######################################## MAPA DO CONTORNO DA CIDADE SÃO PAULO

limite_SP <- read_municipality(code_muni = 3550308)

#coordenadas do contorno de SP em data frame
limite_SP_coord = as.data.frame(st_coordinates(limite_SP$geom))

#coordenadas em Spatial Points
coord_sp <- SpatialPoints(cbind(limite_SP_coord$X, limite_SP_coord$Y), proj4string = CRS("+proj=longlat"))
coord_sp

##coordenadas em UTM
coordUTM_sp <- spTransform(coord_sp, CRS("+init=epsg:32723"))
coordUTM_sp



########################################### GRAFICOS DE VARIANCIAS

##Tranforma os dados em sf
dados_krig_sf <- st_as_sf(dados_krig, coords = c("longitude", "latitude"), crs = 32723)


##Mapa da variância da krigagem com as estações pluviométricas atuais (gradiente)
cores = rev(sf.colors(20))
ggplot(data = dados_krig, aes(x = longitude, y = latitude)) +
  geom_tile(data = dados_krig, aes(fill = varianciaTotal)) + 
  geom_point(data = dados_mensais, aes(x = longitude, y = latitude), pch=16, col = "black") +
  scale_fill_gradientn(colors = cores) +
  annotation_scale(location = "br") + 
  annotation_north_arrow(location = "tr", style = north_arrow_nautical, height = unit(1, "cm"), width = unit(1,"cm")) +
  labs(title = "Variância total da krigagem", subtitle = "Estações atuais", color = "mm") + coord_sf() + theme_bw()



############################################# LOCALIZACAO DE NOVOS PLUVIOMETROS

dados_krig$n = 1:nrows_krig

#378
p1 = dados_krig %>% dplyr::filter(latitude >= 7410000 & longitude >= 338000)
p1

#360
p2 = dados_krig %>% dplyr::filter(latitude >= 7408000 & longitude < 320000)
p2

#186
p3 = dados_krig %>% dplyr::filter(latitude >= 7388000 & latitude < 7390000 & longitude < 320000)
p3

#93
p4 = dados_krig %>% dplyr::filter(latitude >= 7368000 & latitude < 7370000 & longitude < 320000)
p4

#83
p5 = dados_krig %>% dplyr::filter(latitude >= 7365000 & latitude < 7367000 & longitude >= 330000)
p5

#54
p6 = dados_krig %>% dplyr::filter(latitude >= 7358000 & latitude < 7360000 & longitude < 322000)
p6

#61
p7 = dados_krig %>% dplyr::filter(latitude >= 7358000 & latitude < 7360000 & longitude >= 330000)
p7

#9
p8 = dados_krig %>% dplyr::filter(latitude >= 7348000 & latitude < 7350000 & longitude < 322000)
p8

#16
p9 = dados_krig %>% dplyr::filter(latitude >= 7348000 & latitude < 7350000 & longitude >= 334000)
p9

#41
p10 = dados_krig %>% dplyr::filter(latitude >= 7354000 & latitude < 7356000 & longitude >= 326000 & longitude < 328000)
p10

#2
p11 = dados_krig %>% dplyr::filter(latitude <= 7347000)
p11


#Seleção de novos pluviometros em áreas com variância total alta

id_novos_pluvs = c(378, 360, 186, 93, 83, 54, 61, 9, 16, 41, 2)

np = dados_krig %>% dplyr::filter(dados_krig$n %in% id_novos_pluvs)
np

novos_pluvs = data.frame(id = np$n,
                          valorMedidaDez = np$predicaoDez,
                          valorMedidaJan = np$predicaoJan,
                          valorMedidaFev = np$predicaoFev,
                          valorMedidaMar = np$predicaoMar,
                          longitude = np$longitude,
                          latitude = np$latitude)


head(novos_pluvs)


#Unificar dados de pluviômetros atuais e novos
dados_unif = rbind(dados_mensais, novos_pluvs)
head(dados_unif)
tail(dados_unif)
dim(dados_unif)

write_csv(dados_unif, "dados_mensal_com_novos_pluviometros.csv")



##Mapa da variância da krigagem com as estações pluviométricas atuais e Novos (gradiente)
ggplot(data = dados_krig, aes(x = longitude, y = latitude)) +
  geom_tile(data = dados_krig, aes(fill = varianciaTotal)) + 
  geom_point(data = dados_mensais, aes(x = longitude, y = latitude), pch=16, col = "black") +
  geom_point(data = novos_pluvs, aes(x = longitude, y = latitude), pch=2, col = "black") +
  scale_fill_gradientn(colors = rev(sf.colors(20))) +
  annotation_scale(location = "br") + 
  annotation_north_arrow(location = "tr", style = north_arrow_nautical, height = unit(1, "cm"), width = unit(1,"cm")) +
  labs(title = "Variância total da krigagem", subtitle = "59 Estações atuais e 11 novas", color = "mm") + coord_sf() + theme_bw()




############################################# DISCRETIZAÇÃO DAS MEDIÇÕES

n_amostra = dim(dados_unif)[1]
n_amostra

bins = 1 + log2(n_amostra)
bins = round(bins)
bins

head(dados_unif)

for(i in 1:n_amostra){
  for(j in 2:5){
    dados_unif[i,j] = floor(bins * ( ((2*dados_unif[i,j]) + bins) / (2 * bins)))
  }
}
head(dados_unif)

write_csv(dados_unif, "dados_mensal_com_novos_pluviometros_discretizados.csv")





######################### OTIMIZACAO ###################################

# calcular a entropia marginal dos pluviômetros

dados_unif$entropia = 0
head(dados_unif)

for(i in 1:70){
  y = dados_unif[i, c(2:5)]
  y = c(y$valorMedidaDez, y$valorMedidaJan, y$valorMedidaFev, y$valorMedidaMar)
  dados_unif[i,8] = entropy.empirical(y)
}

head(dados_unif)


# encontrar o pluviometro com max(entropia) Px1

#355030838A
px_max = which.max(dados_unif$entropia)
px_max

#define Px1
px = c(px_max)


# calcular a mutual information T(X,Y) a partir de Px1 em relação a amostra-1

for(i in 2:n_amostra){
  
  mutual_info = c()
  
  for(j in 1:n_amostra){
    
    #verifica se j já foi selecionado em Px
    j_selecionado = which(j %in% px)
    
    if(length(j_selecionado) > 0) {
      mutual_info[j] = 1000
    }
    else
    {
      px1 = px[i-1]
      x = dados_unif[px1, c(2:5)]
      x = c(x$valorMedidaDez, x$valorMedidaJan, x$valorMedidaFev, x$valorMedidaMar)
      
      y = dados_unif[j, c(2:5)]
      y = c(y$valorMedidaDez, y$valorMedidaJan, y$valorMedidaFev, y$valorMedidaMar)
      
      xy = rbind(x,y)
      
      mutual_info[j] = mi.empirical(xy)  
    }
    
    
  }
  
  # encontre o mínimo T(X,Y) calculado e selecione este pluviometro como Px2
  
  px2 = which.min(mutual_info)
  
  px = c(px, px2)
  
}

# o resultado é um ranking de pluviometros
px


#seleciona pluviometros na ordem de px
dados_pluvs_rank = dados_unif[px,]

#atribui ranking
dados_pluvs_rank$rank = 1:n_amostra

#atribui se é um pluviometro Novo ou não
dados_pluvs_rank$novo = 'Original'
dados_pluvs_rank = dados_pluvs_rank %>% dplyr::mutate(novo = ifelse(!is.na(as.numeric(dados_pluvs_rank$id)), 'Novo', 'Original'))

head(dados_pluvs_rank)

#seleciona apenas os 59 primeiros pluviometros
dados_pluvs_rank59 = dados_pluvs_rank[-c(60:70),]
dados_pluvs_rank59

head(dados_pluvs_rank59)
summary(dados_pluvs_rank59)


#salva em csv
write_csv(dados_pluvs_rank, "resultado_70pluviometros_rankeados.csv")
write_csv(dados_pluvs_rank59, "resultado_59pluviometros_rankeados.csv")


####################### PLOT

##Tranforma os dados em sf
dados_pluvs_rank_sf <- st_as_sf(dados_pluvs_rank, coords = c("longitude", "latitude"), crs = 32723)
dados_pluvs_rank59_sf <- st_as_sf(dados_pluvs_rank59, coords = c("longitude", "latitude"), crs = 32723)
dados_mensais_sf <- st_as_sf(dados_mensais, coords = c("longitude", "latitude"), crs = 32723)



##Mapa com os pontos atuais e suas respectivas medidas em mm (70 pluviometros)
mapa70 = ggplot(dados_pluvs_rank_sf) + geom_sf(aes(color = rank), cex = 3) + 
  geom_sf(data = limite_SP, fill = "transparent") + theme_bw()+ 
  scale_color_gradientn(colors = tim.colors(20),limits = c(1,70))+
  annotation_scale(location = "br") + annotation_north_arrow(location = "tr", style = north_arrow_nautical,
                                                             height = unit(1, "cm"), width = unit(1,"cm"))+
  labs(title = "Ranking das Estações Pluviométricas", subtitle = "59 Estações atuais e 11 novas", color = "Rank")

mapa70


##Mapa com os pontos atuais e suas respectivas medidas em mm (59 pluviometros)
mapa59_novo = ggplot(dados_pluvs_rank59_sf) + geom_sf(aes(color = novo), cex = 3) + 
  geom_sf(data = limite_SP, fill = "transparent") + theme_bw() + 
  #scale_color_gradientn(colors = tim.colors(20),limits = c(1,70))+
  annotation_scale(location = "br") + annotation_north_arrow(location = "tr", style = north_arrow_nautical,
                                                             height = unit(1, "cm"), width = unit(1,"cm"))+
  labs(title = "Novas Estações Pluviométricas", subtitle = "59 estações com melhor ranking", color = "Status")

mapa59_novo

grid.arrange(mapa70, mapa59_novo, ncol=2)




####################################### TRATAMENTO DE DADOS DOS NOVOS PLUVIOMETROS

#preenche o valorMedida dos meses com os valores contínuos originais
for(i in 1:59){
  
  if(dados_pluvs_rank59[i,10] == 'Novo')
  {
    dados_pluvs_rank59[i,c(2:5)] = novos_pluvs %>% dplyr::filter(id == dados_pluvs_rank59[i,1]) %>% select(c(2:5)) 
  }
  else
  {
    dados_pluvs_rank59[i,c(2:5)] = dados_mensais %>% dplyr::filter(id == dados_pluvs_rank59[i,1]) %>% select(c(2:5)) 
  }
  
}

head(dados_pluvs_rank59)
tail(dados_pluvs_rank59)

#remove colunas rank, entropia e novo
dados_pluvs_rank59 <- dados_pluvs_rank59[,-c(8:10)]

summary(dados_pluvs_rank59)

#salva em csv
write_csv(dados_pluvs_rank59, "resultado_otimizacao_59pluviometros_rankeados.csv")

