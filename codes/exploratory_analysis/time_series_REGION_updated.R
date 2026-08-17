######################################
### Regional time series plotting ###
#####################################

rm(list=ls())
require(tidyverse)
require(fpp3)
require(seasonal)
require(reshape2)
require(ggplot2)

path_inc <- "./data/month_year_incidence_UF.csv"
df_inc <- read.csv(path_inc)


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


###Juntando para constar a região 
df_inc2 <- merge(df_inc, states, by= "UF")

###Aggregate by region
df_inc3 <- df_inc2 |> mutate(Date= yearmonth(year_month)) |> group_by(region, Date) |>
  summarise(confirmed_cases=sum(confirmed_cases), Inhabitants=sum(inhab)) |> 
  mutate(Incidence_per_100000 = (confirmed_cases/Inhabitants)*10^5)

##transformando em tsibble
df_inc3 <- df_inc3 |> as_tsibble(key = c(region), index=Date) 

#Corrigindo os gaps na serie temporal
df_inc3 <- df_inc3 |> fill_gaps()

#Colocando na ordem desejada das regiões
df_inc3 <- df_inc3 |>
  mutate(region = factor(region, levels = c("North", "Northeast", "Central-West", "Southeast", "South")))

# Vetor com as regiões na ordem desejada
region_levels <- c("North", "Northeast", "Central-West", "Southeast", "South")

region_labels <- paste0("(", letters[1:5], ") ", region_levels)

# Criar uma named vector para fazer a substituição
region_map <- setNames(region_labels, region_levels)

# Substituir e reordenar
df_inc3 <- df_inc3 |> 
  mutate(region = fct_relevel(region, region_levels)) |> 
  mutate(region = fct_recode(region,
                             "(a) North"        = "North",
                             "(b) Northeast"    = "Northeast",
                             "(c) Central-West" = "Central-West",
                             "(d) Southeast"    = "Southeast",
                             "(e) South"        = "South"
  ))


#Plotagem das séries temporais
p_ts<- df_inc3 |> autoplot(Incidence_per_100000) + 
  facet_wrap(~ region, ncol = 1) +
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
ggsave(filename = "./data/Figures_results/Regions_time_series.png", 
       height = 7, width = 7)

top3_max <- df_inc3 |>
  group_by(region) |>
  slice_max(order_by = Incidence_per_100000, n = 3, with_ties = FALSE) |>
  arrange(region, desc(Incidence_per_100000)) |>
  ungroup()

acima_05 <- df_inc3 |> filter(Incidence_per_100000 > 0.5)
