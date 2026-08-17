#########################################################################################################
### This code uses the annual incidence of leptospirosis in the Brazilian municipalities to calculate ###
### the Empirical Baysian Index and then estimate the local spatial autocorrelation (LISA)            ###
### also known as local MORAN      								                                                    ###
#########################################################################################################

rm(list=ls())
#https://rstudio-pubs-static.s3.amazonaws.com/576046_d6a9b07690204143a7db7be74f79217f.html#114_Plot_LISA_clusters
library(foreign)
library(rgeoda) #to run moran
library(ggplot2)
library(sf)
library(tidyverse)
library(broom)
library(sp)
library(spdep) #to run moran
library(spgwr)
library(grid)
library(gridExtra)
library(ggthemes)
library(ggpubr)

### Moran Test for Leptospirose per year ###
### Getting the shapefile of municipalities IBGE ###
path.shape <- "./data/BR_Municipios_2024/BR_Municipios_2024.shp"
shp <- read_sf(dsn =path.shape , stringsAsFactors = F)

###Municipalities Codes compatibility (removing one digit from the municipality code to be compatible with SINAN table)
shp$CD_MUN_6 <- str_sub(shp$CD_MUN, 1, 6)
shp$CD_UF <- str_sub(shp$CD_MUN, 1, 2)
CODIGO_UF<- cbind(unique(shp$CD_UF), unique(shp$SIGLA_UF))
#Removing polygons that are actually lakes: Lagoa dos patos e Lagoa mirim
shp<- shp[which(shp$CD_MUN_6!="430000"),]

#Getting the centroids of the polygons
shp_centroid <- st_centroid(shp)

#Annual incidence
annual_incidence <- read.csv("./data/annual_incidence_lep_mun.csv")
annual_incidence$mean <- apply(annual_incidence[,4:21], MARGIN= 1, FUN=mean)
annual_inhabitants <- read.csv("./data/Inhabitants_2007_2024.csv")

# Merging datasets of incidence and annual inhabitants #
df_num <-  merge(annual_incidence, annual_inhabitants, by.x="mun_code", by.y="code")
number_of_cases <- (df_num[,4:21]*df_num[,25:42])/10^5
names(number_of_cases)<- paste0( "cases_", 2007:2024)
df_cases <- cbind(df_num[, c(1,3)], number_of_cases)
df_cases$number_of_cases_mean <- apply(df_cases[,3:20], MARGIN = 1, FUN=mean)
df_n_cases_mean <- df_cases[,c("mun_code","municipio" , "number_of_cases_mean")]
df_mean_incidence <- annual_incidence[, c("mun_code","municipio", "mean")]
df_inhab <-  annual_inhabitants[, c(2, 3, 21)] #getting the 2024 population as reference

#Merging the mean datasets 
df_merge1 <- merge(df_mean_incidence, df_n_cases_mean[, c("mun_code", "number_of_cases_mean")], by= "mun_code")
df_merge <- merge(df_merge1, df_inhab[,c(1,3)], by.x = "mun_code", by.y = "code")
names(df_merge)[c(3, 5)]<- c("mean_incidence", "inhabitats_ref")

### Spatial data ###
shp_db <- merge(shp, df_merge, by.x = "CD_MUN_6", by.y="mun_code", all.x=T)

#Setting NA values as zero ###
shp_db[which(is.na(shp_db$mean_incidence)),"mean_incidence"]<- 0
shp_db[which(is.na(shp_db$inhabitants_ref)),"inhabitats_ref"]<- 0

### Empirical Bayes Index (EBI) ###
cases_total<- sum(round(shp_db$number_of_cases_mean), na.rm=T)
inhabitats_total <-sum(shp_db$inhabitats_ref, na.rm=T)
pav<- inhabitats_total/5570
b <- (cases_total/inhabitats_total)
shp_db$alfa <- (sum(shp_db$inhabitats_ref*(shp_db$mean_incidence-b)^2)/inhabitats_total)-(b/pav)

##In case alpha is negative it should be considered zero ##
shp_db[which(shp_db$alfa<0), "alfa"]<- 0
shp_db$variance <- shp_db$alfa +(b/shp_db$inhabitats_ref) 
shp_db$ebi <- (shp_db$mean_incidence - b)/ (shp_db$variance)^(1/2)

## Neighbours setting
##Warning !!! this step takes around ~11 min !!!
neighbours_sf <- poly2nb(shp_db)

### Defining weighted matrix
### OBS !!! this step takes time !!!
listw <- nb2listw(neighbours_sf , zero.policy = T)
### Applying  The Global Moran test
globalMoran <- moran.test(shp_db$ebi, listw, na.action= na.omit, zero.policy = T)

  
##Applying Local Moran with the Empirical Bayesian Index (EBI) 
shp_db$POLY_ID <-  rownames(shp_db)
queen_w <- queen_weights(shp_db)

lisa <- local_moran(w= queen_w, df=shp_db[c("ebi")])

shp_db$cluster <- as.factor(lisa$GetClusterIndicators())

levels(shp_db$cluster) <- lisa$GetLabels()[as.numeric(levels(shp_db$cluster))+1]
 
col_val<- lisa$GetColors()[as.numeric(levels(shp_db$cluster))+1]
db_lisa <-  st_drop_geometry(shp_db)

## Saving the results
write.csv(db_lisa, file=paste0("./data/db_lisa_ebi_2007_2024_mean.csv"), row.names = F)
  # # 
  # # ### Plotting the results
m<- ggplot(data = shp_db) +
  geom_sf(aes(fill = cluster), color = NA)+
  scale_fill_manual(values=c("Not significant"="#eeeeee", "High-High"="#FF0000", "Low-Low"= "#0000FF",
                             "Low-High"="#a7adf9", "High-Low" ="#f4ada8"))+
  labs(fill=paste("LISA 2007-2024"))+
  theme_map()

##Saving the map
ggsave(filename = paste0("./data/Figures_results/lisa_ebi_2007_2024_mean.png"),m)

  