# Taxa de variação Populacional de 2010 a 2022 (censo) por UF do Brasil com mapa

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

# Extrair os dados de população dos estados brasileiros no Censo 2010 e de 2022
# no Sidra IBGE por meio de API

# 2010

pop2010 <- get_sidra(api = "/t/1378/n3/all/v/allxp/p/all/c1/0/c2/0/c287/0/c455/0")
  
# 2022

pop2022 <- get_sidra(api = "/t/9514/n3/all/v/allxp/p/all/c2/6794/c287/100362/c286/113635")

### Preparar dataframe geral

# retirar colunas desnecessárias e renomeá-las

pop2010 <- pop2010 %>% 
  select(code_state = `Unidade da Federação (Código)`, uf = `Unidade da Federação`,
         pop2010 = Valor)

pop2022 <- pop2022 %>% 
  select(code_state = `Unidade da Federação (Código)`, uf = `Unidade da Federação`,
         pop2022 = Valor)

# realizar a junção das bases por left join

txvariacao <- left_join(pop2010, pop2022)

# realizar a divisão para obter a taxa de variação populacional

txvariacao <- txvariacao %>% 
  mutate(tx = pop2022 / pop2010) %>% 
  mutate(tx = round((tx - 1) * 100, 2))


### agora vamos utilizar a função read_state para extrair os dados necessários
# para localizar os estados no mapa do Brasil e transformar a coluna code_state  
# para o tipo character

mapa_estados <- geobr::read_state() %>% 
  mutate(code_state = as.character(code_state))

# vamos realizar a junção dos nossos dados do IBGE com os dados de localização e retirar 
# as colunas desnecessárias

mapa_uf <- left_join(txvariacao, mapa_estados) %>% 
  select(-name_state, -code_region, -name_region, -pop2010, -pop2022)

# transformar o mapa_uf em geometry

mapa_uf <- st_as_sf(mapa_uf)

# criar intervalos 

intervalos <- classIntervals(mapa_uf$tx, n = 6, style = "equal")

intervalos$brks = round(intervalos$brks, digits = 2)

# intervalos

### plotar mapa cloroplético do Brasil com a taxa de variação por UF

ggplot() +
  geom_sf(data = mapa_uf, aes(fill = tx), color = 'lightgray', size = .15) +
  labs(title = 'Taxa de Variação Populacional de 2010 a 2022 das UFs brasileiras', size = 10,
       caption = "Fonte: IBGE, Censo Demográfico 2022 \n Autor: Pedro Maranhão",
       color = 'black') +
  geom_sf_text(data = mapa_uf, aes(label = abbrev_state), size = 2, nudge_y = 0.6, color = "black") +
  geom_sf_text(data = mapa_uf, aes(label = tx), size = 1.7, nudge_y = 0, color = "black") +
  scale_fill_distiller(palette = "Blues", 
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



