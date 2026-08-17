##############################################################
### Organising tables and setting the incidence statistics ###
###                                                        ###  
##############################################################


rm(list=ls())
#Loading required packages
library(tidyverse)
library(sf)
library(foreign)
library(read.dbc)

### Calculating the incidence per year in Brazil, federative units and municipality ###
## Loading shapefile of municipalities IBGE ##
path.shape <- "./data/BR_Municipios_2024/BR_Municipios_2024.shp"
shp <- read_sf(dsn =path.shape , stringsAsFactors = F)

### Standardising Municipality code ###
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

### Setting table to join data based on spatial unities ###
## Brazil annual incidence ##
br_annual_incidence<- data.frame("year"=numeric(), "incidence"=numeric(), "total_cases"= numeric(), "total_inhab"= numeric())
## States annual incidence ##
uf_annual_incidence <- data.frame("uf"= unique(shp$SIGLA_UF))
## Municipalities annual incidence ##
mun_annual_incidence <- data.frame("mun_code"= df_inhab$code, "municipio"=df_inhab$Municipio)
uf_annual_confirmed_cases <- data.frame("uf"= unique(shp$SIGLA_UF))

##Looping for reading the annual files and getting the required data combining them in one dataframe###

for(i in files_lep_year){
  print(i)
  db<- read.dbc(i) #read dbc files 
  db$month_year <-  format(as.Date(db$DT_NOTIFIC, format="%Y-%m-%d"), format="%Y-%m") #Notification date
  year<- format(as.Date(db$DT_NOTIFIC[1], format="%Y-%m-%d"), format="%Y") #Get year of notification date
  
  ### Confimed cases ###
  db_mun_confirmed_cases <- db %>% filter(db$CLASSI_FIN == "1") %>%
    group_by(ID_MN_RESI)  %>% summarize(n()) #filter confirmed cases and summarizes based on the residence municipality of the patient
  names(db_mun_confirmed_cases)[2]<- "confirmed_cases"
  
  ### Inhabitats ###
  #Merging confirmed cases dataset with the inhabitants dataset#
  db_mun_confirmed_cases_inhab <- merge(df_inhab[, c("code", paste0("X", year))], db_mun_confirmed_cases,by.x="code", by.y="ID_MN_RESI", all.x = T)
  names(db_mun_confirmed_cases_inhab)[2]<- "inhabitants"
  ##Calculating incidence cases/inhabitants##
  db_mun_confirmed_cases_inhab$incidence <- (db_mun_confirmed_cases_inhab$confirmed_cases/ db_mun_confirmed_cases_inhab$inhabitants)
  # Spatial dataset ###
  shp_db <- merge(shp, db_mun_confirmed_cases_inhab, by.x = "CD_MUN_6", by.y="code", all.x=T)
  shp_db[which(is.na(shp_db$incidence)),"incidence"]<- 0
  shp_db[which(is.na(shp_db$confirmed_cases)),"confirmed_cases"]<- 0
  shp_db[which(is.na(shp_db$inhabitants)),"inhabitants"]<- 0
  
  ### Br annual incidence
  cases_total<- sum(shp_db$confirmed_cases, na.rm=T)
  inhab_total <-sum(shp_db$inhabitants, na.rm=T)
  b <- (cases_total/inhab_total)*10^5 #per 100 thousand inhabitats
  br_aux<- data.frame("year"=year, "incidence"=b, "total_cases"=cases_total, "total_inhab"= inhab_total)
  br_annual_incidence <- rbind(br_annual_incidence, br_aux)
  
  ### Relative risk Mun
  shp_db$incidence <- (shp_db$confirmed_cases/shp_db$inhabitants)*10^5 #per 100 thousand inhabitats
  df_mun<- st_drop_geometry(shp_db)
  mun_annual_incidence <- merge(mun_annual_incidence, df_mun[, c("CD_MUN_6", "incidence")], by.x="mun_code", by.y="CD_MUN_6")
  names(mun_annual_incidence)[ncol(mun_annual_incidence)]<-paste( "incidence", year) 
  
  ### Relative risk State
  df_state<- df_mun %>% group_by(SIGLA_UF)  %>% summarise(total_inhab = sum(inhabitants),
                                                          total_cases = sum(confirmed_cases))
  df_state$incidence<- (df_state$total_cases/df_state$total_inhab)*10^5 #per 100 thousand inhabitats
  
  uf_annual_incidence<- merge(uf_annual_incidence, df_state[,c("SIGLA_UF", "incidence")], by.x="uf", by.y="SIGLA_UF")
  names(uf_annual_incidence)[ncol(uf_annual_incidence)]<- paste("incidence", year)
  
  uf_annual_confirmed_cases <- merge(uf_annual_confirmed_cases, df_state[,c("SIGLA_UF", "total_inhab", "total_cases")], by.x="uf", by.y="SIGLA_UF")
  names(uf_annual_confirmed_cases)[(ncol(uf_annual_confirmed_cases)-1):ncol(uf_annual_confirmed_cases)]<- c(paste0("Inhab_", year), paste0("Cases_", year))

}

uf_annual_incidence$mean<- apply(uf_annual_incidence[,2:19 ], MARGIN =1, FUN=mean)

uf_annual_confirmed_cases_2 <- uf_annual_confirmed_cases %>% rowwise() %>% mutate(Total_cases = sum(c_across(starts_with("cases_")))) %>%
  ungroup()

uf_annual_confirmed_cases_2<- uf_annual_confirmed_cases_2 %>% mutate(incidence_2007_2024 = (Total_cases/Inhab_2024)*10^5) 


### Saving and exporting 
write.csv(br_annual_incidence, file="./data/annual_incidence_lep_br.csv")
write.csv(uf_annual_incidence , file="./data/annual_incidence_lep_uf.csv ")
write.csv(mun_annual_incidence, file="./data/annual_incidence_lep_mun.csv")
write.csv(uf_annual_confirmed_cases_2 , file="./data/incidence_lep_uf_2007_2024.csv")


