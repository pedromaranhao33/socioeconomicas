# Quantidade de municípios com área inferior a 100 km² em cada UF do Brasil a ser 
# exibida em um mapa

# Desativar a notação científica

options(scipen = 999)

# Bibliotecas utilizadas

library(sidrar)
library(tidyverse)
library(geobr) 
library(sf) 
library(tmaptools) 
library(ggspatial)
library(RColorBrewer)
library(classInt)

# Extrair os dados de população dos municípios brasileiros do Censo 2022
# no Sidra IBGE por meio de API

area <- get_sidra(api = "/t/4714/n6/all/v/6318/p/all/d/v6318%203")

# código das ufs para transformar o valor em 0 se alguma delas não possuirem cidades

ufs_completas <- as.character(c(12, 27, 13, 16, 29, 23, 53, 32, 52, 21, 
                   31, 50, 51, 15, 25, 26, 22, 41, 33, 24, 
                   11, 14, 43, 42, 28, 35, 17))

# Transformações no dataframe

area <- area %>% 
  filter(Valor < 100) %>%  # filtrar para municípios com menos de 100 km²
  mutate(code_state = str_sub(`Município (Código)`, end = 2)) %>% 
  # criar coluna com código das ufs utilizando os primeiros números do código dos municípios
  group_by(code_state) %>% # agrupar pelas ufs
  summarise(qtd_mun = n()) %>% # realizar a contagem
  complete(code_state = ufs_completas, fill = list(qtd_mun = 0)) # completar com 0 as UFs 
  # que não possuem municípios com menos de 100 kms²

### vamos utilizar a função read_state para extrair os dados necessários
# para localizar os estados no mapa do Brasil e transformar a coluna code_state  
# para o tipo character

mapa_estados <- geobr::read_state() %>% 
  mutate(code_state = as.character(code_state))

# vamos realizar a junção dos nossos dados do IBGE com os dados de localização 

mapa_uf <- left_join(area, mapa_estados) 

# transformar o mapa_uf em geometry

mapa_uf <- st_as_sf(mapa_uf)

# criar intervalos 

intervalos <- classIntervals(mapa_uf$qtd_mun, n = 5, style = "jenks")

intervalos$brks = round(intervalos$brks, digits = 2)

### plotar mapa cloroplético do Brasil com a quantidade de municípios com 
# essas características

ggplot() +
  geom_sf(data = mapa_uf, aes(fill = qtd_mun), color = 'lightgray', size = .15) +
  labs(title = 'Quantidade de municípios com área inferior a 100 km² em cada UF do Brasil', size = 10,
       caption = "Fonte: IBGE, Censo Demográfico 2022 \n Autor: Pedro Maranhão",
       color = 'black') +
  geom_sf_text(data = mapa_uf, aes(label = qtd_mun), size = 3, nudge_y = 0.2, color = "black") +
  scale_fill_distiller(palette = "Reds", 
                       direction = 1, 
                       name = "Taxa de Variação",
                       breaks = intervalos$brks) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(4, "cm"),
                                    height = unit(4, "cm")) +
  
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
        plot.title.position = "plot", 
        plot.caption = element_text(hjust = 0.5))



  