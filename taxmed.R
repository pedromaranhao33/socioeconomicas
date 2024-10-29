# Estados e municípios com maior e menor médicos por mil habitantes com mapas

# bibliotecas a serem utilizadas

library(tidyverse)
library(microdatasus)
library(sidrar)
library(geobr)
library(RColorBrewer)
library(sf)
library(ggspatial)
library(tmaptools) 
library(classInt)


setwd('C:/Users/55819/Desktop/scripts_r')

# 1) extrair a população do sidra do IBGE para estados e municípios do Censo 2022
# aproveitaremos para realizar slices, renomear colunas e transformar o código municipal
# de 7 dígitos para 6 dígitos para facilitar nosso processo de merge 
# com a base do datasus

pop_est <- get_sidra(api = "/t/9514/n3/all/v/allxp/p/all/c2/6794/c287/100362/c286/113635")

pop_est <- pop_est %>% 
  select(UF = `Unidade da Federação (Código)`, pop = Valor, `Unidade da Federação`)

pop_mun <- get_sidra(api = "/t/9514/n6/all/v/allxp/p/all/c2/6794/c287/100362/c286/113635")

pop_mun <- pop_mun %>% 
  select(codigo_mun = `Município (Código)`, pop = Valor, Município) %>% 
  mutate(codigo_mun = str_sub(codigo_mun, end = 6))
  
# 2) quantidade de médicos por estado e município no datasus para a data mais recente
# realizando 

med <- fetch_datasus(year_start = 2024, month_start = 09,
                         year_end = 2024, month_end = 09,
                         uf = "all", information_system = "CNES-PF", 
                     vars = c("CODUFMUN","CBO", "CPFUNICO")) %>% 
                     na.omit(CPFUNICO)
        
med <- process_cnes(med, information_system = "CNES-PF")

# testar para sp

med_sp <- med %>% 
  filter(CODUFMUN == 355030) %>% 
  filter(str_detect(CBO, "^225|^2231")) %>% 
  group_by(nome) %>% 
  summarise(total_medicos = length(CBO))
 
# no tabdatasus não é exibido as quantidades de Medico Radiologista Intervencionista 
# CBO 225355 e Médico Hemoterapeuta CBO 225340, apesar do último poder ser escolhido 
# nas seleções disponíveis de Médicos, por isso o resultado para algumas poucas cidades
# ficará ligeramente diferente dos dados verificados no  
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?cnes/cnv/prid02br.def
# utilizei o município de são paulo - sp como referência dessa pesquisa por ser 
# a cidade brasileira com o maior número de médicos.

# calcular a diferença 
  

# vamos filtrar o CBO para começando com 225 - PROFISSIONAIS DA MEDICINA e
# 2231 - Médicos


# 2.1) por Estado - criaremos uma coluna chamada UF extraindo os dois primeirs dígitos
# do CODUFMUN que representam a UF para futuro merge com a população dos estados 

med_est <- med %>% 
  filter(str_detect(CBO, "^225|^2231")) %>% 
  mutate(UF = str_sub(CODUFMUN, end = 2)) %>% 
  group_by(UF) %>% 
  summarise(total_medicos = length(CBO))

# 2.2) por Município - renomearemos o nome da coluna CODUFMUN para merge com a população
# dos municípios. 

med_mun <- med %>% 
  filter(str_detect(CBO, "^225|^2231")) %>% 
  group_by(CODUFMUN) %>% 
  summarise(total_medicos = length(CBO)) %>% 
  rename(codigo_mun = CODUFMUN)


rm(med)
  
# 3) para nossa pesquisa precisamos adicionar outros municípios que não existem médicos 
# alocados, pois apenas 5450 municípios estão presentes dos 5570 existentes no Brasil.

# vamos utilizar os códigos dos municípios do pop_mun do ibge para fazer isso
# aproveitaremos para extrair o nome em extenso deles também

med_mun <- full_join(pop_mun, med_mun)

# os municípios que adicionamos não tem médicos, no nosso dataframe estão como NA
# temos de substituir o NA por 0 

med_mun[is.na(med_mun)] <- 0

# vamos separar um dataframe com esses municípios para uma futura análise

mun_sem_med <- med_mun %>% 
  filter(total_medicos == 0)
  
# vamos criar a taxa de médicos por mil habitantes e arredondada para dois dígitos

taxa_medicos <- med_mun %>% 
#  filter(total_medicos > 0) %>% 
  mutate(txmed = round((total_medicos / (pop / 1000)),2))

# 4) vamos criar o dataframe para os estados

med_est <- left_join(med_est, pop_est)

taxa_med_est <- med_est %>% 
  mutate(txmed = round((total_medicos / (pop / 1000)),2)) %>% 
  mutate(txmedbr = mean(txmed))

# taxa média do Brasil é 

# 5) criar mapas estaduais

uf = read_state(
  code_state = "all",
  year = 2020, 
  simplified = TRUE,
  showProgress = TRUE) 

# alterar o tipo da coluna e renomear para realizar join

uf <- uf %>% 
  mutate(code_state = as.character(code_state)) %>% 
  rename(UF = code_state)

# realizar o left_join

mapa_uf <- left_join(taxa_med_est, uf)

# Criar intervalos automáticos dos dados para as cores do mapa

intervalos <- classIntervals(mapa_uf$txmed, n = 6, style = "equal")

# transformar o mapa_uf em geometry

mapa_uf <- st_as_sf(mapa_uf)

### plotar mapa

ggplot() +
  geom_sf(data = mapa_uf, aes(fill = txmed), color = 'black', size = .15) +
  labs(title = 'Taxa de Médicos por 1000 habitantes das UFs brasileiras', size = 10,
       caption = "Fonte: DATASUS, Setembro 2024 \n Autor: Pedro Maranhão",
       color = 'black') +
  geom_sf_text(data = mapa_uf, aes(label = abbrev_state), size = 2, nudge_y = 0.6, color = "black") +
  geom_sf_text(data = mapa_uf, aes(label = txmed), size = 1.7, nudge_y = 0, color = "black") +
  scale_fill_distiller(palette = "Blues", 
                       direction = 1, 
                       name = "Taxa de médicos por 1000 habitantes",
                       breaks = intervalos$brks) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(4, "cm"),
                                    height = unit(4, "cm")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

# 6) mapas dos municípios 

# vamos realizar para SP

sp_mun <- read_municipality(code_muni = "SP",
                            year = 2020) %>% 
mutate(code_muni = as.character(code_muni),
       code_muni = str_sub(code_muni, end = 6)) %>% 
  rename(codigo_mun = code_muni) 


# left join

mapa_sp <- left_join(sp_mun, taxa_medicos) 

# Criar intervalos automáticos dos dados para as cores do mapa

intervalos_sp <- classIntervals(mapa_sp$txmed, n = 6, style = "equal")

intervalos_sp$brks <- round(intervalos_sp$brks, 2)

# transformar o mapa_uf em geometry

mapa_sp <- st_as_sf(mapa_sp)

# plotar

ggplot() +
  geom_sf(data = mapa_sp, aes(fill = txmed), color = 'lightgray', size = .15) +
  labs(title = 'Taxa de Médicos por 1000 habitantes dos municípios de SP', size = 10,
       caption = "Fonte: DATASUS, Setembro 2024 \n Autor: Pedro Maranhão",
       color = 'black') + 
  scale_fill_distiller(palette = "Blues", 
                       direction = 1, 
                       name = "Taxa de médicos por 1000 habitantes",
                       breaks = intervalos_sp$brks) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(4, "cm"),
                                    height = unit(4, "cm")) +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
        plot.title.position = "plot",  
        plot.caption = element_text(hjust = 0.5))

# plotar as cidades de SP que tem 0 médicos

# primeiro criar o dataframe

zero_medicos_sp <- mapa_sp %>% 
  filter(txmed == 0)

zero_medicos_sp <- st_as_sf(zero_medicos_sp)

# plotar mapa

ggplot() +
  geom_sf(data = mapa_sp, aes(fill = ifelse(txmed == 0, "Sem Médicos", "Possuem Médicos")), 
          color = 'lightgray', size = .15) +
  labs(title = 'Municípios de SP sem médicos', 
       size = 10,
       caption = "Fonte: DATASUS, Setembro 2024 \n Autor: Pedro Maranhão",
       color = 'black') +
  geom_sf_text(data = zero_medicos_sp, aes(label = name_muni), size = 2.5, nudge_y = 0, color = "black") +
  scale_fill_manual(values = c("Sem Médicos" = "red", "Possuem Médicos" = "white"), 
                    name = "Situação dos Municípios") + 
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(4, "cm"),
                                    height = unit(4, "cm")) +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
  plot.caption = element_text(hjust = 0.5))



# gráfico de barras horizontais com os 10 municípios com maiores taxas e 10 menores

# 10 Maiores

maiores_sp <- mapa_sp %>%
arrange(desc(txmed)) %>%  
slice_head(n = 10)  %>% 
as.data.frame()


ggplot(maiores_sp, aes(x = reorder(name_muni, txmed), y = txmed)) +  
  geom_col(fill = "#3A66A0") +  
  theme_classic() +
  labs(
    title = "Top 10 Maiores taxas de médicos por 1000 habitantes das cidades de SP",
    x = NULL,
    y = "Taxa de médicos por 1000 habitantes",
    caption = "Fonte: DATASUS, Setembro 2024 \n Autor: Pedro Maranhão") +
  geom_text(
    aes(label = paste0(format(round(txmed, digits = 2), nsmall = 1, decimal.mark = ","))),
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
  expand_limits(y = c(0, max(maiores_sp$txmed) * 1.15)) + 
  coord_flip()


# 10 Menores
  
menores_sp <- mapa_sp %>%
filter(txmed > 0) %>%     
arrange(txmed) %>%        
slice_head(n = 10)      

ggplot(menores_sp, aes(x = reorder(name_muni, txmed), y = txmed)) +  
  geom_col(fill = "#c30010") +  
  theme_classic() +
  labs(
    title = "Top 10 menores taxas de médicos por 1000 habitantes das cidades de SP",
    x = NULL,
    y = "Taxa de médicos por 1000 habitantes",
    caption = "Fonte: DATASUS, Setembro 2024 \n Autor: Pedro Maranhão") +
  geom_text(
    aes(label = paste0(format(round(txmed, digits = 2), nsmall = 1, decimal.mark = ","))),
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
  expand_limits(y = c(0, max(menores_sp$txmed) * 1.15)) + 
  coord_flip()



