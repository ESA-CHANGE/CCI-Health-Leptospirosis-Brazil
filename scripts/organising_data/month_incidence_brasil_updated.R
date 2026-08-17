
###Correlação entre volume de chuva acumulada e incidência de leptospirose


rm(list=ls())
#https://rstudio-pubs-static.s3.amazonaws.com/576046_d6a9b07690204143a7db7be74f79217f.html#114_Plot_LISA_clusters
library(foreign)
#library(rgdal)
library(ggplot2)
library(tidyverse)
library(reshape)

##Getting the shapefile of municipalities IBGE
path.shape <- "./BR_Municipios_2022/BR_Municipios_2022.shp"

path_db <- "./Arquivos_leptospirose_BR_sinan"
files_lep_year<- list.files(path_db, pattern = "*.dbf", full.names = T)

df_pop<- read.csv("./População_Municipio_por_ano/pop_2007_2024.csv")
#Até a data de hj 04/09/2024 não havia disponivel a estimativa da populacao para o ano de 2023 no site do ibge
#https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html
#por isso estou usando a populacao estimada de 2024
df_pop$X2023 <- df_pop$X2024
#Populacao Brasil
df_pop_br<- data.frame(year=2007:2024 , pop= colSums(df_pop[, 4:21], na.rm = T))





#Moran_global_results_ebi <- data.frame("Ano"= numeric(), "Moran" = numeric(), "p_valor"=numeric())
i <- files_lep_year[1]
#files_lep_year <- files_lep_year[8:length(files_lep_year)]

df_mes_ano_br <- data.frame("mes"=numeric(), ano=numeric(), casos_confirmados=numeric(), populacao=numeric(), incidencia=numeric() )
#df_data_br <- data.frame(data=numeric(), casos_confirmados=numeric(), populacao=numeric(), incidencia=numeric() )

for(i in files_lep_year){
  
  db<- read.dbf(i)
  db$data <- as.Date(db$DT_NOTIFIC, format="%Y-%m-%d")
  db$mes <-  format(db$data, format="%m")
  db$ano <- format(db$data, format="%Y")
  ano <- unique(db$ano)
  
  
  # ###Casos confirmados por mes e ano
  db_casos_confirmados <- db %>% filter(db$CLASSI_FIN == "1") %>%
    group_by(mes, ano)  %>% summarize(n())
  names(db_casos_confirmados)[ncol(db_casos_confirmados)]<- "casos_confirmados"

  ###Casos confirmados por dia
  # db_casos_confirmados <- db %>% filter(db$CLASSI_FIN == "1") %>%
  #   group_by(data)  %>% summarize(n())
  # names(db_casos_confirmados)[ncol(db_casos_confirmados)]<- "casos_confirmados"
  
  ###População
  db_casos_confirmados$populacao<- df_pop_br[df_pop_br$year==ano,"pop"]
  #incidência por 100,000 habitantes
  db_casos_confirmados<- db_casos_confirmados %>% mutate(incidencia = (casos_confirmados/populacao)*100000)
  df_mes_ano_br<- rbind(df_mes_ano_br, db_casos_confirmados)
  #df_data_br<- rbind(df_data_br, db_casos_confirmados)
  db <- db_casos_confirmados <- NA
}
#salvando mes ano 
write.csv(df_mes_ano_br, file="./Leptospirose_no_br/incidencia_no_brasil_mes_ano_2024.csv", row.names = F)  
##salvando por data
#write.csv(df_data_br, file="./Leptospirose_no_br/incidencia_no_brasil_DATA_2024.csv", row.names = F)  
