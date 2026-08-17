#############################################################################
###Organising tables and setting month incidence aggregated by region    ####
#############################################################################

rm(list=ls())

library(foreign)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape)
library(lubridate)
library(read.dbc)
Sys.setlocale("LC_TIME", "English")


###Loading files downloaded from SINAN 
# Source : https://datasus.saude.gov.br/transferencia-de-arquivos/#
path_db <- "./data/Leptospirosis_files_SINAN"
files_lep_year<- list.files(path_db, pattern = "*.dbc", full.names = T) #dbc is a compacted format used by SINAN to share datasets


### Loading annual inhabitats table from 2007 to 2024 (IBGE)###
df_inhab <- read.csv("./data/Inhabitants_2007_2024.csv") #previously organized

### Brazil inhabitants 
df_inhab_br<- data.frame(year=2007:2024 , inhab= colSums(df_inhab[, 4:21], na.rm = T))
### State inhabitants
df_inhab_uf <- df_inhab %>% mutate(UF = str_sub(code, start=1, end=2)) 
df_inhab_uf2<- df_inhab_uf[!is.na(df_inhab_uf$Municipio),]
#df_inhab[,4:46]<- apply(df_inhab[,4:46], MARGIN = 2, FUN="as.numeric")

### Setting a table with the Brazilian States
states <- c(
  "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal",
  "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul",
  "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro",
  "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima", "Santa Catarina",
  "São Paulo", "Sergipe", "Tocantins"
)

### Setting a table with the respective regions
regions <- c(
  "North", "Northeast", "North", "North", "Northeast", 
  "Northeast", "Central-West", "Southeast", "Central-West",
  "Northeast", "Central-West", "Central-West", "Southeast", "North",
  "Northeast", "South", "Northeast", "Northeast", "Southeast", "Northeast", 
  "South", "North", "North", "South", "Southeast", "Northeast", "North"
)

### Setting a dataframe with the states and its regions
states <- data.frame(UF = states, region = regions)

### Inserting a column with the states acronym and code
states$UF <- c(
  "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA",
  "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"
)
states$CODE <- c(
  12, 27,  16, 13, 29, 23, 53, 32, 52, 21, 51, 50, 31, 15, 25, 41, 
  26, 22, 33, 24, 43, 11, 14, 42, 35, 28, 17
)

i<- files_lep_year[1]

df_data_UF <- data.frame(SG_UF=character(), UF=character() , month_year=numeric(),  confirmed_cases=numeric(), inhab=numeric(), incidence=numeric() )

for(i in files_lep_year){
  
  db<- read.dbc(i)
  db <- merge(db, states, by.x= "SG_UF", by.y= "CODE" )
  db$date <- as.Date(db$DT_NOTIFIC, format="%Y-%m-%d")
  db<- db %>% mutate(year_month = format(date, format="%Y-%m"), year = year(date))
  year <- unique(db$year)
  

  # ###Confirmed cases aggregated by month , year and State
  db_mun_confirmed_cases <- db %>% filter(db$CLASSI_FIN == "1") %>% 
    group_by(SG_UF, year_month)  %>% summarize(n())
  
  names(db_mun_confirmed_cases)[3]<- "confirmed_cases"
  
 
  ###Inhabitants
  df_inhab<- df_inhab %>% mutate(SG_UF= str_sub(code, start=1, end=2)) 
  df_inhab_year <- df_inhab[, c("SG_UF","code", paste0("X", year))]
  names(df_inhab_year)[3]<- "inhab"
  df_inhab_year_UF<- merge(df_inhab_year, states, by.x="SG_UF", by.y="CODE")
  df_inhab_year_UF_sum <- df_inhab_year_UF %>% group_by(SG_UF, UF) %>% summarise(inhab=sum(inhab, na.rm=T))
  
  db_UF_inhab <- merge(db_mun_confirmed_cases, df_inhab_year_UF_sum,  by="SG_UF", all.x=T)

  ### Incidence per 100,000 inhabitants
  db_UF_inhab <- db_UF_inhab %>% mutate(incidence=(confirmed_cases/inhab)*10^5)
  db_UF_inhab<- db_UF_inhab[,c("SG_UF","UF", "year_month", "confirmed_cases", "inhab", "incidence")]
  df_data_UF<- rbind(df_data_UF, db_UF_inhab)
  db <- db_confirmed_cases <- db_UF_inhab<- NA
}


#Saving and exporting
write.csv(df_data_UF, file="./data/month_year_incidence_UF.csv", row.names = F)  

