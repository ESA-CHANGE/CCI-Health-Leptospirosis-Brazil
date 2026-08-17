######################################
### Relative risk map per State   ####
######################################

rm(list=ls())

require(sf)
require(tidyverse)
require(ggplot2)
require(ggthemes)

### Getting datasets ###
### Accumulated Incidence per Federative unit 2007-June 2024 ###

##Loading Shapefile of the Federative Units (UF)
path_UF<- "./data/BrasilUFPoligono/BrasilUFPolygon.shp"
shp<- read_sf(path_UF)

##Loading 
df<-read.csv("./data/incidence_lep_uf_2007_2024.csv")

##Juntando dados de shapefile e incidencia para a plotagem do mapa
shp_inc <- merge(shp, df, by.x="UF", by.y="uf")

#Calcular centróides para posicionar os rótulos
estados <- shp_inc %>%
  mutate(geometry = st_make_valid(geometry))
estados_rotulo <- shp_inc[, c("UF", "incidence_2007_2024")] %>%
  st_point_on_surface() %>% 
  cbind(st_coordinates(.))  


##Plotando MAPA incidencia UF
accumulated_n <- ggplot(shp_inc) +
  geom_sf(aes(fill = incidence_2007_2024), color = "white") +
  scale_fill_viridis_c(option = "plasma", name="Incidence 2007-2024\n per 100,000 inhab.", na.value = "lightgrey")+
  geom_text(data=estados_rotulo,
            aes(X, Y, label = UF),
            size = 5, fontface = "bold", color = "white") +
  theme_map()+
  theme(legend.title = element_text(size=12, lineheight = 1.2), 
        legend.text = element_text(size=11), 
        legend.position = "right")

accumulated_n
ggsave(accumulated_n, filename= "./Leptospirose_no_br/RS_estudo_de_caso/Manuscrito_RS_Lep/Figuras_Geo_health/Incidence_2007_2024_SOUTH.png",
       width= 5, height=5 )


