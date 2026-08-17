#############################################
### Incidence of Brazil Region 2007 - 2024 ##
### Map plotting 		                       ##
#############################################
rm(list=ls())

library(cartography)
library(ggplot2)
library(sf)
library(sp)
library(tidyverse)
library(ggthemes)
library(ggpubr)


### Loading dataset ###
path_region<- "./data/annual_incidence_region_2007_2024.csv"
region_inc<- read.csv(path_region)

df_mean_region <-region_inc %>% group_by(region) %>% summarise(mean(incidence), confirmed_cases_total =sum(confirmed_cases), inhab=max(inhab))
df_mean_region <- df_mean_region%>% mutate(incidence_mean = (confirmed_cases_total/inhab)*10^5)

##Plotting Region Map - accumulated incidence 2007-2024

path_UF<- "./data/BrasilUFPoligono/BrasilUFPolygon.shp"
shp<- read_sf(path_UF)
shp <- shp %>%
  mutate(region = case_when(
    UF %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "North",
    UF %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Northeast",
    UF %in% c("DF", "GO", "MT", "MS") ~ "Central-West",
    UF %in% c("ES", "MG", "RJ", "SP") ~ "Southeast",
    UF %in% c("PR", "RS", "SC") ~ "South",
    TRUE ~ NA_character_
  ))

shp_merge <- merge(shp, df_mean_region, by="region")

### Calculating centroids for positioning legend
state <- shp_merge %>%
  mutate(geometry = st_make_valid(geometry))
state_rotulo <- state %>%
  st_point_on_surface() %>% 
  cbind(st_coordinates(.))  


accumulated_n <- ggplot(shp_merge) +
  geom_sf(aes(fill = incidence_mean), color = "white") +
  scale_fill_viridis_c(option = "plasma", name="Incidence 2007-2024\n per 100,000 inhab.", na.value = "lightgrey")+
  geom_text(data=state_rotulo,
            aes(X, Y, label = UF),
            size = 3.5, fontface = "bold", color = "white") +
  theme_map()+
  theme(legend.title = element_text(size=12, lineheight = 1.2), legend.text = element_text(size=11) )

### Saving and exporting map ###
ggsave(accumulated_n, filename= "./data/Figures_results/Incidence_2007_2024_region.png", width= 6.32, height=4.62 )
