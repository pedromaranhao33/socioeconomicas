# Estudo sobre a Taxa de variação Populacional de 2010 a 2022 (censo) 
# nos municípios do Brasil com gráficos e um mapa para Pernambuco

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

# Extrair os dados de população dos municípios brasileiros e dos municípios 
# no Censo 2010 e de 2022 no Sidra IBGE por meio de API

# 2010

pop2010 <- get_sidra(api = "/t/1378/n6/all/v/allxp/p/all/c1/0/c2/0/c287/0/c455/0")

# 2022

pop2022 <- get_sidra(api = "/t/9514/n6/all/v/allxp/p/all/c2/6794/c287/100362/c286/113635")

### Preparar dataframe geral

# retirar colunas desnecessárias, renomeá-las e criar a coluna uf para facilitar
# análise posterior

pop2010 <- pop2010 %>% 
  select(code_muni = `Município (Código)`, mun = Município, 
         pop2010 = Valor) %>% 
  mutate(uf = str_sub(code_muni, end = 2))

pop2022 <- pop2022 %>% 
  select(code_muni = `Município (Código)`, mun = Município, 
         pop2022 = Valor) %>% 
  mutate(uf = str_sub(code_muni, end = 2))

# a diferença de 5 municípios se deve a criação de mais 5 municípios no Brasi
# em 2013, para mais informações recomendo matéria da BBC Brasil:
# https://www.bbc.com/portuguese/brasil-44932862

# realizar a junção das bases por left join

txvariacao <- left_join(pop2010, pop2022)

# realizar a divisão para obter a taxa de variação populacional

txvariacao <- txvariacao %>% 
  mutate(tx = pop2022 / pop2010) %>% 
  mutate(tx = round((tx - 1) * 100, 2))

# Agora vamos realizar as transformações nos dados para criar gráficos de barras
# horizontais com os 10 municípios que mais crescerem demograficamente e os 10 que menos

# criação do dataframe com os 10 maiores 

maiores <- txvariacao %>%
  arrange(desc(tx)) %>%  
  slice_head(n = 10)  %>% 
  as.data.frame()

# Plotar gráfico de barras

ggplot(maiores, aes(x = reorder(mun, tx), y = tx)) +  
  geom_col(fill = "#3A66A0") +  
  theme_classic() +
  labs(
    title = "Os 10 municípios com maior crescimento demográfico no Brasil entre 2010 e 2022",
    x = NULL,
    y = "Taxa de crescimento demográfico de 2010 a 2022",
    caption = "Fonte: Censo IBGE, Outubro de 2024 \n Autor: Pedro Maranhão") +
  geom_text(
    aes(label = paste0(format(round(tx, digits = 2), nsmall = 1, decimal.mark = ","), "%")),
    color = "white",  
    hjust = 1.2,  
    size = 3.5) +
  theme(
    plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
    plot.title.position = "plot", 
    axis.title.y = element_text(size = 10, family = "sans"),
    axis.text = element_text(size = 10, family = "sans"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  expand_limits(y = c(0, max(maiores$tx) * 1.15)) + 
  coord_flip()

# Criação de dataframe com os 10 menores

menores <- txvariacao %>%
  arrange(tx) %>%        
  slice_head(n = 10)      

# Plotar gráfico de barras

ggplot(menores, aes(x = reorder(mun, -tx), y = -tx)) +  
  geom_col(fill = "#c30010") +  
  theme_classic() +
  labs(
    title = "Os 10 municípios com maior redução demográfica no Brasil entre 2010 e 2022",
    x = NULL,
    y = "Taxa de redução demográfica de 2010 a 2022",
    caption = "Fonte: Censo IBGE, Outubro de 2024 \n Autor: Pedro Maranhão") +
  geom_text(
    aes(label = paste0(format(round(tx, digits = 2), nsmall = 1, decimal.mark = ","), "%")),
    color = "white",  
    hjust = 1.1, 
    size = 3.5) +
  theme(
    plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
    plot.title.position = "plot",
    axis.title.y = element_text(size = 10, family = "sans"),
    axis.text = element_text(size = 10, family = "sans"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_flip() 

# Vamos plotar mapas das variações de taxas demográficas para Pernambuco 

# extrair os geoms para os municípios de PE

pe_mun <- read_municipality(code_muni = "PE",
                            year = 2020) %>% 
  mutate(code_muni = as.character(code_muni))

# left join

mapa_pe <- left_join(pe_mun, txvariacao) 

# Criar intervalos automáticos dos dados para as cores do mapa

intervalos_pe <- classIntervals(mapa_pe$tx, n = 6, style = "equal")

intervalos_pe$brks <- round(intervalos_pe$brks, 2)

# transformar o mapa_uf em geometry

mapa_pe <- st_as_sf(mapa_pe)

# as 10 cidades que mais cresceram para adicionarmos os nomes no gráfico

# Para aprimorar a visualização vamos retirar o município de Fernando de Noronha 
# por ser uma ilha distante do continente e distorcer o mapa. 

mapa_pe <- mapa_pe %>% 
  filter(mun != 'Fernando de Noronha - PE')

# Criar um outro dataframe com os nome dos 10 municípios com maior crescimento
# demográfico de Pernambuco para adicionarmos ao mapa

maiores_pe <- mapa_pe %>%
  arrange(desc(tx)) %>%  
  slice_head(n = 10)  

# Plotar o mapa cloroplético dos municípios de Pernambuco

ggplot() +
  geom_sf(data = mapa_pe, aes(fill = tx), color = 'lightgray', size = .15) +
  labs(title = 'Taxa de variação demográfica dos municípios de PE entre 2010 a 2022', size = 10,
       caption = "Fonte: Censo IBGE, Outubro 2024 \n Autor: Pedro Maranhão",
       subtitle = "Com o nome dos 10 municípios que mais cresceram demograficamente no período",
       color = 'black') + 
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = 1, 
                       name = "Taxa de variação demográfica",
                       breaks = intervalos_pe$brks) +
  geom_sf_text(data = maiores_pe, aes(label = name_muni), size = 3.5, nudge_y = 0, color = "black") + 
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "tl",
                                    style = north_arrow_nautical,
                                    width = unit(2, "cm"),
                                    height = unit(2, "cm")) +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
        plot.title.position = "plot",  
        plot.subtitle = element_text(family = 'sans', hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) 


