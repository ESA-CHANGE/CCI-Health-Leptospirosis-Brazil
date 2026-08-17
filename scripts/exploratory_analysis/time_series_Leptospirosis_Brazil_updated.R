#############################################
### Plotting timeseries of Brazil regions ###
#############################################

rm(list=ls())
require(tidyverse)
require(fpp3)
require(seasonal)
require(reshape2)
require(ggplot2)

### Loading dataset ###
path_inc <- "./data/month_year_incidence_brasil.csv"
df_inc <- read.csv(path_inc)


### Dataset arrange to transform in a timeseries object ###
df_inc<- df_inc %>% mutate(month_year= yearmonth(paste0(year,"_",month)), 
                           incidence=(confirmed_cases/inhabitants)*10^5)

### Converting dataset in tsibble ###
df_inc2 <- df_inc |> as_tsibble(index=month_year) 

### Correcting gaps in the time series ###
df_inc2 <- df_inc2 |> fill_gaps()

### Timeseries plotting ###
p_ts_inc <- df_inc2 |> autoplot(incidence) + 
  labs(y = "Incidence per 100,000 inhabitants", x = "")+
  scale_x_yearmonth(
    date_breaks = "3 years",       
    date_labels = "%b %Y" )+
  theme_bw()+
  theme (legend.position = "none", 
        axis.text = element_text(size = 12),   
        axis.title = element_text(size = 13),   
        strip.text = element_text(size = 13 , hjust = 0), 
        strip.background = element_blank())
ggsave(p_ts_inc, filename = "./data/Figures_results/Brazil_time_serie.png", 
       height = 3, width = 7)

#### Multiple seasonal decomposition of leptospirosis incidence in Brazil
dcmp <- df_inc2 |>
  model(stl = STL(incidence))
STL_components <- components(dcmp) |> autoplot()+
  labs(subtitle = "Incidence = trend + season_year + remainder") +
  theme_bw()+
  theme(
    plot.title    = element_text(size = 20, face = "bold"), 
    plot.subtitle = element_text(size = 13),                 
    strip.text    = element_text(size = 13, face = "bold"),  
    axis.title    = element_text(size = 12),                 
    axis.text     = element_text(size = 11)                  
  )

STL_components

### Saving timeseries plot ###
ggsave(STL_components, filename = "./data/Figures_results/Brazil_time_serie_decomp.png", 
       height = 10, width = 10)
