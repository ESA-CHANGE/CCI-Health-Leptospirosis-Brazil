#######################################################################
### This code saves organised tables with confirmed                 ###
### cases,  inhabitants, and incidence aggregated by Brazilian      ###
### regions                                                         ###
#######################################################################


rm(list=ls())
library(foreign)
#library(rgdal)
library(ggplot2)
library(tidyverse)
library(read.dbc)
library(sf)
library(reshape)
Sys.setlocale("LC_TIME", "English")

##Getting the shapefile of municipalities IBGE
path.shape <- "./data/BR_Municipios_2024/BR_Municipios_2024.shp"
shp <- read_sf(dsn =path.shape , stringsAsFactors = F)
### Standardizing Municipality code ###
shp$CD_MUN_6 <- str_sub(shp$CD_MUN, 1, 6)
shp$CD_UF <- str_sub(shp$CD_MUN, 1, 2)
CODIGO_UF<- cbind(unique(shp$CD_UF), unique(shp$SIGLA_UF))

### Removing Lagoons polygons: Lagoa dos patos and Lagoa mirim ###
shp<- shp[which(shp$CD_MUN_6!="430000"),]


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

#states %>% filter(region=="Central-West") %>% arrange(CODE)

#i <- files_lep_year[1]

### Setting dataframe structure to receive the region data
df_data_region <- data.frame(region=character() , year=numeric(),  confirmed_cases=numeric(), inhab=numeric(), incidence=numeric() )

### Looping for reading the annual files and getting the required data combining them in one dataframe ###

for(i in files_lep_year){
  
  db<- read.dbc(i)
  db <- merge(db, states, by.x= "SG_UF", by.y= "CODE" )
  db$data <- as.Date(db$DT_NOTIFIC, format="%Y-%m-%d")
  db<- db %>% mutate(month_year = as.Date(data, format="%Y-%m"), year = year(data))
  year <- unique(db$year)
  

  db_mun_confirmed_cases <- db %>% filter(db$CLASSI_FIN == "1") %>% 
    group_by(ID_MN_RESI)  %>% summarize(n())
  
  names(db_mun_confirmed_cases)[2]<- "confirmed_cases"
  
  ### Spatial data
  shp_db <- merge(shp, db_mun_confirmed_cases, by.x = "CD_MUN_6", by.y="ID_MN_RESI", all.x=T)
  shp_db <- merge(shp_db, states , by.x="CD_UF", by.y="CODE", all.x=T)
  shp_db[which(is.na(shp_db$incidence)),"incidence"]<- 0
  shp_db[which(is.na(shp_db$confirmed_cases)),"confirmed_cases"]<- 0
  shp_db[which(is.na(shp_db$inhabitants)),"inhabitants"]<- 0
  db_mun <- st_drop_geometry(shp_db)
  ### Inhabitants
  df_inhab_year <- df_inhab[, c("code", paste0("X", year))]
  names(df_inhab_year)[2]<- "inhab"
  db_mun_inhab <- merge(db_mun, df_inhab_year, by.x="CD_MUN_6" , by.y = "code", all.x=T)

  #Incidence per 100,000 inhabitants
  db_region_inhab <- db_mun_inhab %>% group_by(region) %>% summarise(inhab=sum(inhab, na.rm = T), 
                                                                     confirmed_cases=sum(confirmed_cases))
  db_region_inhab <-db_region_inhab %>% mutate(incidence = (confirmed_cases/inhab)*10^5)
  db_region_inhab$year<- year
  db_region_inhab<- db_region_inhab[,c("region", "year", "confirmed_cases", "inhab", "incidence")]
  df_data_region<- rbind(df_data_region, db_region_inhab)
  db <- db_confirmed_cases <- db_region_inhab<- shp_db<- NA
}

#Saving and exporting
write.csv(df_data_region, file="./data/annual_incidence_region_2007_2024.csv", row.names = F)  

