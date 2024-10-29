# Taxa de alfabetização das pessoas de 15 anos ou mais por UFs e por cidades 
# do Nordeste do Brasil no ano de 2022 ambos categorizados por raça e cor em mapas

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

# Extrair os dados de alfabetização das UFs do Brasil do Censo 2022
# no Sidra IBGE por meio de API

alfa_br <- get_sidra(api = "/t/9543/n3/all/v/all/p/all/c2/6794/c86/all/c287/100362/d/v2513%202")

# realizar transformações no dataframe alfa_br

alfa_br <- alfa_br %>% 
  select(code_state = `Unidade da Federação (Código)`, `Unidade da Federação`,
         taxalfa = Valor, cor_raca = `Cor ou raça`)

### vamos utilizar a função read_state para extrair os dados necessários
# para localizar os estados no mapa do Brasil e transformar a coluna code_state  
# para o tipo character

mapa_estados <- geobr::read_state() %>% 
  mutate(code_state = as.character(code_state))

# vamos realizar a junção dos nossos dados do IBGE com os dados de localização 

mapa_uf <- left_join(alfa_br, mapa_estados) 

# transformar o mapa_uf em geometry

mapa_uf <- st_as_sf(mapa_uf)

# criar intervalos 

intervalos <- classIntervals(mapa_uf$taxalfa, n = 5, style = "jenks")

intervalos$brks = round(intervalos$brks, digits = 2)

### plotar mapa cloroplético do Brasil com a taxa de de alfabetização por UF

ggplot() +
  geom_sf(data = mapa_uf, aes(fill = taxalfa), color = 'lightgray', size = .15) +
  labs(title = 'Taxa de alfabetização das pessoas de 15 anos ou mais por UFs', size = 10,
       caption = "Fonte: IBGE, Censo Demográfico 2022 \n Autor: Pedro Maranhão",
       color = 'black') +
  facet_wrap(~cor_raca) +
  geom_sf_text(data = mapa_uf, aes(label = paste0(taxalfa, "%")), size = 2, nudge_y = 0.2, color = "black") +
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = 1, 
                       name = "Taxa de alfabetização",
                       breaks = intervalos$brks)   +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(2, "cm"),
                                    height = unit(2, "cm")) +
  
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
        plot.title.position = "plot", 
        plot.caption = element_text(hjust = 0.5))

# agora vamos extrair os dados dos municípios do Nordeste

alfa_ne <- get_sidra(api = '/t/9543/n6/all/v/all/p/all/c2/6794/c86/all/c287/100362/d/v2513%202')

# Transformações no dataframe

alfa_ne <- alfa_ne %>% 
  select(code_muni = `Município (Código)`, taxalfa_ne = Valor, cor_raca = `Cor ou raça`) %>% 
  filter(str_sub(code_muni, 1, 2) %in% c("21", "22", "23", "24", "25", "26", "27", "28", "29"))

### vamos utilizar a função read_region para extrair os dados necessários
# para localizar as regiões no mapa do Brasil e transformar a coluna code_region  
# para o tipo character

mapa_ne <- read_municipality(year = 2020) %>% 
  mutate(code_muni = as.character(code_muni))

# vamos realizar a junção dos nossos dados do IBGE com os dados de localização 

mapa_ne <- left_join(alfa_ne, mapa_ne) 

# transformar o mapa_uf em geometry

mapa_ne <- st_as_sf(mapa_ne)

# criar intervalos 

intervalos_ne <- classIntervals(mapa_ne$taxalfa_ne, n = 5, style = "jenks")

intervalos_ne$brks = round(intervalos_ne$brks, digits = 2)

# criar um outro dataframe para marcar a divisa entre os estados do Nordeste utilizando o mapa_uf
# filtrando o código dos estados entre 21 ao 29

mapauf_ne <- mapa_uf %>% 
  filter(str_sub(code_state, 1, 2) %in% c("21", "22", "23", "24", "25", "26", "27", "28", "29"))


### plotar mapa cloroplético do Brasil com a taxa de de alfabetização dos municípios do Nordeste

ggplot() +
  # Camada dos municípios do Nordeste
  geom_sf(data = mapa_ne, aes(fill = taxalfa_ne), color = 'lightgray', size = .05) +  # Linhas dos municípios transparentes
  
  # Limites dos estados do Nordeste (por cima dos municípios)
  geom_sf(data = mapauf_ne, fill = NA, color = 'black', size = .6) +  # Limites mais espessos para os estados
  
  # Mapas por cor e reça
  facet_wrap(~cor_raca) +
  
  # Legendas e informações adicionais
  labs(title = 'Taxa de alfabetização das pessoas de 15 anos ou mais dos Municípios do NE', size = 10,
       caption = "Fonte: IBGE, Censo Demográfico 2022 \n Autor: Pedro Maranhão",
       color = 'black') +
  
  # Escala de cores para a taxa de alfabetização
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = 1, 
                       name = "Taxa de alfabetização",
                       breaks = intervalos_ne$brks) +
  
  # Adiciona escala gráfica e seta de orientação
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(1.5, "cm"),
                                    height = unit(1.5, "cm")) +
  
  # Tema visual
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
        plot.title.position = "plot", 
        plot.caption = element_text(hjust = 0.5))
