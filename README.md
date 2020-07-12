#Análise geoestatística da rede pluviométrica da cidade de Sâo Paulo, SP, Brasil.

Autor: Marlon Jeske
Instituições: Universidade Federal de São Paulo (UNIFESP) e Instituto Tecnológico de Aeronáutica (ITA)

Com este trabalho é possível realizar a análise de uma rede pluviométrica por meio de geoestatística e observar as predições de precipitação na cidade de São Paulo por meio da interpolação. Com esses resultados, é possível identificar novos locais para instalação de pluviômetros onde há maior variância (erro na estimativa de chuva). 

Este trabalho ainda otimiza a rede pluviométrica ordenando as estações por sua importância, calculada por meio do ganho de informação que cada pluviômetro fornece para a rede (medidas de entropia). Com este ranking de importância, é possível verificar quais pluviômetros são redundantes e que podem ser realocados para locais de maior importância.



Os dados para análise da precipitação estão disponíveis no site do Cemaden: http://www.cemaden.gov.br/mapainterativo/


##1. Tratamento de dados
Arquivo: tratamento_dados.r
- Tratamento de dados brutos para sumarizar os dados em medições diárias e mensais por estação. 
- Além disso, é necessário remover os pluviômetros que não estão presentes em todos os meses da análise e os que estão com todas as - medições mensais iguais a zero.
- Para os pluviômetros com medição mensal igual a zera em alguns dos meses observados, utiliza-se a média dos demais meses disponíveis.

##2. Análise estatística e Mapas
Arquivo: analise_estatistica_mapas.r
- Estatísticas do conjunto de dados por medições mensais.
- Obtenção do mapa dos limites da cidade de São Paulo.
- Gráficos da localização e medições das estações pluviométricas.
- Conversão das coordenadas longitudinais para formato UTM (planares) de todas estações pluviométricas

##3. Análise Geoestatística
Arquivo: analise_estatistica_mapas.r
- Análise geoestatística das medições mensais
- Criação de semivariogramas de nuvem, mapa variográfico e semivariogramas experimental e teórico
- Interpolação dos dados por meio de Krigagem Ordinária
- Análise dos erros de predição por validação cruzada leave-one-out
- Gráficos do mapa de São Paulo com os valores estimados pela krigagem e a variância das predições

##4. Seleção de novas localizações para pluviômetros / Otimização utilizando entropia
Arquivo: otimizacao_entropia.r
- Observando o mapa de variâncias da krigagem é possível selecionar novas localizações para pluviômetros em locais de alta variância
- Seleção das coordenadas para os novos pluviômetros
- Adição das novas posições na rede pluviométrica atual
- Rankeamento da importância de cada pluviômetro por medidas de informação (entropia)
- Otimização da rede pluviométrica selecionando os N melhores pluviômetros pelo ranking
- Obtenção de um novo conjunto de dados com a nova rede pluviométrica
- Gráficos do rankeamento das estações e o mapa da nova rede pluviométrica

##5. Análise geoestatística da nova rede pluviométrica
Arquivo: analise_geoestatistica_otimizados.r
- Análise geoestatística do novo conjuntos de pluviômetros
- Realização de uma nova krigagem ordinária para comparar a variância da rede com a nova configuração
- Gráficos comparativos da nova krigagem e variância da rede
