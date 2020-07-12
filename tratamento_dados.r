library(tidyverse)


###################################################################################################################
#####################   DADOS PRECIPITA��O DE DEZEMBRO A MAR�O DE 2017/2018       #################################
###################################################################################################################
dados_dez <- read_delim("2692_SP_2017_12_oficial.csv", delim = ";")
dados_jan <- read_delim("2692_SP_2018_1_oficial.csv", delim = ";")
dados_fev <- read_delim("2692_SP_2018_2_oficial.csv", delim = ";")
dados_mar <- read_delim("2692_SP_2018_3_oficial.csv", delim = ";")


###################################################################################################################
###############################   INFORMA��ES E CARACTER�STICAS DOS DADOS       ###################################
###################################################################################################################
dim(dados_dez)
head(dados_dez)
str(dados_dez)

dim(dados_jan)
head(dados_jan)
str(dados_dez)

dim(dados_fev)
head(dados_fev)
str(dados_dez)

dim(dados_mar)
head(dados_mar)
str(dados_dez)



###################################################################################################################
###############################   VERIFICA��O DO N�MERO DE ESTA��ES EM CADA M�S   #################################
###################################################################################################################

##Verifica n�mero de esta��es em cada m�s
table_dez <- table(dados_dez$codEstacao)
table_jan <- table(dados_jan$codEstacao)
table_fev <- table(dados_fev$codEstacao)
table_mar <- table(dados_mar$codEstacao)

length(table_dez)
length(table_jan)
length(table_fev)
length(table_mar)


##Garante que em todos os meses analisados, os dados ser�o provenientes do mesmo grupo de esta��es
estacoes_dez <- dados_dez$codEstacao
estacoes_jan <- dados_jan$codEstacao
estacoes_fev <- dados_fev$codEstacao
estacoes_mar <- dados_mar$codEstacao

inter_meses1 <- intersect(estacoes_dez,estacoes_jan)
inter_meses2 <- intersect(estacoes_fev,estacoes_mar)
inter_meses_total <- intersect(inter_meses1,inter_meses2)
length(inter_meses_total)


##Define total de esta��es (mesmas) que ser�o utilizadas
inter_meses_total


##Dados de precipita��o com os mesmos pluvi�metros       
dados_dez <- dados_dez %>% dplyr::filter(codEstacao %in% inter_meses_total)
dados_jan <- dados_jan %>% dplyr::filter(codEstacao %in% inter_meses_total)
dados_fev <- dados_fev %>% dplyr::filter(codEstacao %in% inter_meses_total)
dados_mar <- dados_mar %>% dplyr::filter(codEstacao %in% inter_meses_total)

dim(dados_dez)
head(dados_dez)
str(dados_dez)

dim(dados_jan)
head(dados_jan)
str(dados_jan)

dim(dados_fev)
head(dados_fev)
str(dados_fev)

dim(dados_mar)
head(dados_mar)
str(dados_mar)


###################################################################################################################
##################################     LOCALIZA��ES DOS PLUVI�METROS       ########################################
###################################################################################################################

##Pluvi�metro com sua respectiva coordenada (lat, long)
localizacao_lat <- aggregate(dados_fev$latitude, by = list(dados_fev$codEstacao), first)
localizacao_lat

localizacao_long <- aggregate(dados_fev$longitude, by = list(dados_fev$codEstacao), first)
localizacao_long

head(localizacao_lat)
head(localizacao_long)

localizacao <- cbind(localizacao_lat, localizacao_long$x)
localizacao


##Dados originais do CEMADEN est�o com os nomes trocados - Coordenadas est�o no oceano
colnames(localizacao) <- c("codEstacao", "longitude", "latitude")
localizacao

head(localizacao)

##salva em csv
#write_csv(localizacao, "pluviometros.csv")


###################################################################################################################
############################  ORGANIZA��O DOS DADOS POR DIA POR PLUVI�METRO DE CADA M�S  ##########################
###################################################################################################################

##Valores de precipita��o di�rios no m�s de dezembro
dados_dez$data <- as.Date(dados_dez$datahora)
dados_estacao_dez <- aggregate(dados_dez$valorMedida, by = list(dados_dez$codEstacao, dados_dez$data), sum)
dados_estacao_dez

colnames(dados_estacao_dez) <- c("codEstacao", "data", "valorMedida")
dados_estacao_dez

##Unifica matriz de precipta��o com suas coordenadas geogr�ficas
dados_estacao_dez <- merge(dados_estacao_dez, localizacao, by = "codEstacao")

##Ordenada por data
dados_estacao_dez <- dados_estacao_dez[order(as.Date(dados_estacao_dez$data, format="%Y-%m-%d")),]
dados_estacao_dez

dim(dados_estacao_dez)
head(dados_estacao_dez)



##Valores de precipita��o di�rios no m�s de janeiro
dados_jan$data <- as.Date(dados_jan$datahora)
dados_estacao_jan <- aggregate(dados_jan$valorMedida, by = list(dados_jan$codEstacao, dados_jan$data), sum)
dados_estacao_jan

colnames(dados_estacao_jan) <- c("codEstacao", "data", "valorMedida")
dados_estacao_jan

##Unifica matriz de precipta��o com suas coordenadas geogr�ficas
dados_estacao_jan <- merge(dados_estacao_jan, localizacao, by = "codEstacao")
dados_estacao_jan

##Ordenada por data
dados_estacao_jan <- dados_estacao_jan[order(as.Date(dados_estacao_jan$data, format="%Y-%m-%d")),]
dados_estacao_jan

dim(dados_estacao_jan)
head(dados_estacao_jan)



##Valores de precipita��o di�rios no m�s de fevereiro
dados_fev$data <- as.Date(dados_fev$datahora)
dados_estacao_fev <- aggregate(dados_fev$valorMedida, by = list(dados_fev$codEstacao, dados_fev$data), sum)
dados_estacao_fev

colnames(dados_estacao_fev) <- c("codEstacao", "data", "valorMedida")
dados_estacao_fev

##Unifica matriz de precipta��o com suas coordenadas geogr�ficas
dados_estacao_fev <- merge(dados_estacao_fev, localizacao, by = "codEstacao")
dados_estacao_fev

##Ordenada por data
dados_estacao_fev <- dados_estacao_fev[order(as.Date(dados_estacao_fev$data, format="%Y-%m-%d")),]
dados_estacao_fev

dim(dados_estacao_fev)
head(dados_estacao_fev)



##Valores de precipita��o di�rios no m�s de mar�o
dados_mar$data <- as.Date(dados_mar$datahora)
dados_estacao_mar <- aggregate(dados_mar$valorMedida, by = list(dados_mar$codEstacao, dados_mar$data), sum)
dados_estacao_mar

colnames(dados_estacao_mar) <- c("codEstacao", "data", "valorMedida")
dados_estacao_mar

##Unifica matriz de precipta��o com suas coordenadas geogr�ficas
dados_estacao_mar <- merge(dados_estacao_mar, localizacao, by = "codEstacao")
dados_estacao_mar

##Ordenada por data
dados_estacao_mar <- dados_estacao_mar[order(as.Date(dados_estacao_mar$data, format="%Y-%m-%d")),]
dados_estacao_mar

dim(dados_estacao_mar)
head(dados_estacao_mar)



###################################################################################################################
######################################     S�RIE TEMPORAL DI�RIA       ############################################
###################################################################################################################
## Unifica todos os meses criando uma serie temporal diaria (dia 1 at� 120)
serie_temporal <- rbind(dados_estacao_dez, dados_estacao_jan, dados_estacao_fev, dados_estacao_mar)
serie_temporal

dim(serie_temporal)
head(serie_temporal)
str(serie_temporal)

##salva em csv
#write_csv(serie_temporal, "serie_temporal_diaria.csv")



###################################################################################################################
######################################     M�DIA MENSAL POR PLUVI�METRO     #######################################
###################################################################################################################
##Matriz com a media mensal de cada pluvi�metro 
#DEZEMBRO
dados_dez_mensal <- aggregate(dados_estacao_dez$valorMedida, by = list(dados_estacao_dez$codEstacao), mean)
dados_dez_mensal

colnames(dados_dez_mensal) <- c("codEstacao", "valorMedida")
dados_dez_mensal

dados_dez_mensal <- merge(dados_dez_mensal, localizacao, by = "codEstacao")
dados_dez_mensal

head(dados_dez_mensal)
dim(dados_dez_mensal)


#JANEIRO
dados_jan_mensal <- aggregate(dados_estacao_jan$valorMedida, by = list(dados_estacao_jan$codEstacao), mean)
dados_jan_mensal

colnames(dados_jan_mensal) <- c("codEstacao", "valorMedida")
dados_jan_mensal

dados_jan_mensal <- merge(dados_jan_mensal, localizacao, by = "codEstacao")
dados_jan_mensal

head(dados_jan_mensal)
dim(dados_jan_mensal)



#FEVEREIRO
dados_fev_mensal <- aggregate(dados_estacao_fev$valorMedida, by = list(dados_estacao_fev$codEstacao), mean)
dados_fev_mensal

colnames(dados_fev_mensal) <- c("codEstacao", "valorMedida")
dados_fev_mensal

dados_fev_mensal <- merge(dados_fev_mensal, localizacao, by = "codEstacao")
dados_fev_mensal

head(dados_fev_mensal)
dim(dados_fev_mensal)



#MAR�O
dados_mar_mensal <- aggregate(dados_estacao_mar$valorMedida, by = list(dados_estacao_mar$codEstacao), mean)
dados_mar_mensal

colnames(dados_mar_mensal) <- c("codEstacao", "valorMedida")
dados_mar_mensal

dados_mar_mensal <- merge(dados_mar_mensal, localizacao, by = "codEstacao")
dados_mar_mensal

head(dados_mar_mensal)
dim(dados_mar_mensal)




###################################################################################################################
##################################     MEDI��ES DOS PLUVI�METROS      #############################################
###################################################################################################################

##Verifica se todos os pluviom�tros tem medi��es m�dias maiores que zero
summary(dados_dez_mensal$valorMedida)
summary(dados_jan_mensal$valorMedida)
summary(dados_fev_mensal$valorMedida)
summary(dados_mar_mensal$valorMedida)


##Verifica quais s�o os pluviom�tros sem medi��es por m�s
pluvs_sem_medicao_dez <- dados_dez_mensal %>% dplyr::filter(valorMedida == 0)
pluvs_sem_medicao_jan <- dados_jan_mensal %>% dplyr::filter(valorMedida == 0)
pluvs_sem_medicao_fev <- dados_fev_mensal %>% dplyr::filter(valorMedida == 0)
pluvs_sem_medicao_mar <- dados_mar_mensal %>% dplyr::filter(valorMedida == 0)

pluvs_sem_medicao_dez
pluvs_sem_medicao_jan
pluvs_sem_medicao_fev
pluvs_sem_medicao_mar

#verifica quais pluvi�metros n�o tem medi��o em todos os meses
pluvs_sem_medicao1 <- intersect(pluvs_sem_medicao_dez, pluvs_sem_medicao_jan)
pluvs_sem_medicao2 <- intersect(pluvs_sem_medicao1, pluvs_sem_medicao_fev)
pluvs_sem_medicao_total <- intersect(pluvs_sem_medicao2, pluvs_sem_medicao_mar)

pluvs_sem_medicao_total

#remove o pluvi�metro sem medi��es em todos os meses (355030801B)
dados_dez_mensal <- dados_dez_mensal %>% dplyr::filter(codEstacao != '355030801B')
dados_jan_mensal <- dados_jan_mensal %>% dplyr::filter(codEstacao != '355030801B')
dados_fev_mensal <- dados_fev_mensal %>% dplyr::filter(codEstacao != '355030801B')
dados_mar_mensal <- dados_mar_mensal %>% dplyr::filter(codEstacao != '355030801B')


#total de 59 pluvi�metros
dim(dados_dez_mensal)
dim(dados_jan_mensal)
dim(dados_fev_mensal)
dim(dados_mar_mensal)


##Verifica quais s�o os pluviom�tros sem medi��es em algum dos meses

#6 pluviometros com alguma medi��o mensal igual a zero
#"355030856A" "355030838A" "355030870A" "355030808B" "355030844A" "355030847A"

#unifica os dados de todos os meses em uma tabela
dados_mensais = data.frame(id = dados_dez_mensal$codEstacao, 
                           valorMedidaDez = dados_dez_mensal$valorMedida, 
                           valorMedidaJan = dados_jan_mensal$valorMedida, 
                           valorMedidaFev = dados_fev_mensal$valorMedida, 
                           valorMedidaMar = dados_mar_mensal$valorMedida,
                           longitude = dados_dez_mensal$longitude,
                           latitude = dados_dez_mensal$latitude)


media_p6 = (2.58000000    +  0.1209677) / 2; #valorMedidaFev #valorMedidaMar  
media_p28 = (0.01290323  + 0.1114286) /2; # valorMedidaJan valorMedidaFev
media_p31 = (1.42322581 +    0.1935484     +  0.4387097) /3; #valorMedidaFev 
media_p33 = (0.45806452   +   0.8661290)/2; #valorMedidaFev valorMedidaMar
media_p39 = (6.4479310    + 2.64392857  +    7.2174194)/3; # valorMedidaDez
media_p46 = (1.04903226  +    0.02142857    +  6.7154839)/3;# valorMedidaJan

dados_mensais[6,c(4,5)] = media_p6
dados_mensais[28,c(3,4)] = media_p28
dados_mensais[31,4] = media_p31
dados_mensais[33,c(4,5)] = media_p33
dados_mensais[39,2] = media_p39
dados_mensais[46,3] = media_p46


dados_dez_mensal2 = data.frame(codEstacao = dados_mensais$id, 
                              valorMedida = dados_mensais$valorMedidaDez,
                              longitude = dados_mensais$longitude,
                              latitude = dados_mensais$latitude)

dados_jan_mensal2 = data.frame(codEstacao = dados_mensais$id, 
                               valorMedida = dados_mensais$valorMedidaJan,
                               longitude = dados_mensais$longitude,
                               latitude = dados_mensais$latitude)

dados_fev_mensal2 = data.frame(codEstacao = dados_mensais$id, 
                               valorMedida = dados_mensais$valorMedidaFev,
                               longitude = dados_mensais$longitude,
                               latitude = dados_mensais$latitude)

dados_mar_mensal2 = data.frame(codEstacao = dados_mensais$id, 
                               valorMedida = dados_mensais$valorMedidaMar,
                               longitude = dados_mensais$longitude,
                               latitude = dados_mensais$latitude)



################################################################################

#salva em csv
write_csv(dados_dez_mensal2, "media_mensal_pluvi_dez.csv")
write_csv(dados_jan_mensal2, "media_mensal_pluvi_jan.csv")
write_csv(dados_fev_mensal2, "media_mensal_pluvi_fev.csv")
write_csv(dados_mar_mensal2, "media_mensal_pluvi_mar.csv")
write_csv(dados_mensais, "dados_mensais_todos_meses.csv")

################################################################################
