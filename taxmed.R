# Estudo sobre a taxa de médicos por mil habitantes nas UFs brasileiras 
# e nos municípios de São Paulo.

# Nesse estudo vamos realizar vários tratamentos de dados com a base demográfica do
# Censo 2022 do IBGE, da base CNES-PF (Profissionais) do Datasus de Outubro de 2024
# e algumas visualizações:

# 1) Tabela com os 10 municípios com maiores taxas de médicos por mil habitantes do Brasil
# 2) Mapa com taxa de médicos por mil habitantes por UFs e média do Brasil
# 3) Mapa com taxa de médicos por mil habitantes para municípios de SP
# 4) Mapa apontando a localização das cidades onde não há médicos em SP
# 5) Gráficos de barras dos municípios com maior e menor taxa de médicos por habitantes em SP

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
library(gridExtra)
library(sqldf)
library(knitr)
library(kableExtra)

# Primeiramente vamos extrair a população do sidra do IBGE para estados e municípios 
# do Censo 2022, aproveitaremos para realizar slices, renomear colunas e transformar 
# o código municipal de 7 dígitos para 6 dígitos para facilitar nosso processo de merge 
# com a base do datasus

pop_est <- get_sidra(api = "/t/9514/n3/all/v/allxp/p/all/c2/6794/c287/100362/c286/113635")

pop_est <- pop_est %>% 
  select(UF = `Unidade da Federação (Código)`, pop = Valor, `Unidade da Federação`)

pop_mun <- get_sidra(api = "/t/9514/n6/all/v/allxp/p/all/c2/6794/c287/100362/c286/113635")

pop_mun <- pop_mun %>% 
  select(codigo_mun = `Município (Código)`, pop = Valor, Município) %>% 
  mutate(codigo_mun = str_sub(codigo_mun, end = 6))
  
# Extração da quantidade de médicos por estado e município no datasus para o mês de Outubro
# de 2024. Vamos utilizar de variáveis o código do município (CODUFMUN), filtrar por médicos 
# por meio do CBO e o CPFUNICO para retirarmos as linhas com os médicos duplicados. 

med <- fetch_datasus(year_start = 2024, month_start = 10,
                         year_end = 2024, month_end = 10,
                         uf = "all", information_system = "CNES-PF", 
                     vars = c("CODUFMUN","CBO", "CPFUNICO")) %>% 
                     na.omit(CPFUNICO)

# Para adicionar nomes no lugar de códigos para facilitar a leitura dos CBOs, 
# utiliza-se a função process_cnes 
        
med <- process_cnes(med, information_system = "CNES-PF")

# Antes, realizaremos a testagem dos dados da base para o município de SP para comparar 
# com os dados verificados na página tabnet do datasus para a base Cnes-PF
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?cnes/cnv/prid02br.def

# Vamos filtrar o CBO para os números começando com 225 - PROFISSIONAIS DA MEDICINA e
# 2231 - Médico

med_sp <- med %>% 
  filter(CODUFMUN == 355030) %>% 
  filter(str_detect(CBO, "^225|^2231")) %>% 
  group_by(nome) %>% 
  summarise(total_medicos = length(CBO))
 
# Observei que no tabdatasus, diferente do resultado da API, não é exibida as quantidades de 
# Medico Radiologista Intervencionista CBO 225355 e Médico Hemoterapeuta CBO 225340, 
# apesar do último poder ser escolhidonas seleções disponíveis de Médicos, por isso 
# o resultado para algumas capitais e cidades mais populosas pode estar minimamente 
# distinto dos dados do http://tabnet.datasus.gov.br/cgi/tabcgi.exe?cnes/cnv/prid02br.def

# Utilizei o município de são paulo - sp como referência dessa pesquisa por ser 
# a cidade brasileira com o maior número de médicos.

# Por Estado - criaremos uma coluna chamada UF extraindo os dois primeirs dígitos
# do CODUFMUN que representam a UF para futuro merge com a população dos estados 

med_est <- med %>% 
  filter(str_detect(CBO, "^225|^2231")) %>% 
  mutate(UF = str_sub(CODUFMUN, end = 2)) %>% 
  group_by(UF) %>% 
  summarise(total_medicos = length(CBO))

# Por Município - renomearemos o nome da coluna CODUFMUN para merge com a população
# dos municípios. 

med_mun <- med %>% 
  filter(str_detect(CBO, "^225|^2231")) %>% 
  group_by(CODUFMUN) %>% 
  summarise(total_medicos = length(CBO)) %>% 
  rename(codigo_mun = CODUFMUN)

# Retirar o med e med_sp para economizar espaço 

rm(med, med_sp)
  
# Para nossa pesquisa, precisamos adicionar outros municípios que não existem médicos 
# alocados, pois apenas 5450 municípios estão presentes nessa pesquisa do datasus
# contra 5570 existentes no Brasil.

# Vamos utilizar os códigos dos municípios do pop_mun do ibge para fazer isso
# aproveitaremos para extrair o nome em extenso deles também. Para tanto realizaremos
# um merge full join.

med_mun <- full_join(pop_mun, med_mun)

# Os municípios que adicionamos que não tem médicos no nosso dataframe estão como NA.
# Precisamos ter o cuidado de substituir o NA por 0 para futura análise. 

med_mun[is.na(med_mun)] <- 0
  
# Criamos a taxa de médicos por mil habitantes para municípios 
# e arredondada para dois dígitos

taxa_medicos <- med_mun %>% 
  mutate(txmed = round((total_medicos / (pop / 1000)),2))

# Algumas estatísticas descritivas para municípios

summary(taxa_medicos$pop)
summary(taxa_medicos$total_medicos)
summary(taxa_medicos$txmed)

# Quantidade de municípios com zero médicos

sum(taxa_medicos$total_medicos == 0) # 105 municípios de 5570

# Porcentagem do total de municípios com zero médicos

sum(taxa_medicos$total_medicos == 0) / count(pop_mun) * 100 # 1,89%

# 10 municípios com maiores taxas de médicos por 1000 habitantes

# Transformação de dataframe dos 10 top municípios utilizando  SQL

top_taxa_mun <- sqldf("
  SELECT 
  Município,
  total_medicos AS Médicos,
  txmed AS Taxa
  FROM taxa_medicos
  ORDER BY txmed DESC
  LIMIT 10
")

# 1) Criar tabela com os 10 municípios com maiores taxas de médicos do Brasil

top_taxa_mun %>%
  kable(caption = "Top 10 Maiores Taxas de médicos por mil habitantes 10/2024") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = F,
    font_size = 16
  ) %>%
  column_spec(3, color = "white", background = "#3A66A0") %>%
  add_header_above(c(" " = 3), bold = TRUE) 

# Alguns números de cidades podem parecer estranhamente altos, pprém estão validados 
# no site do datasus: http://tabnet.datasus.gov.br/cgi/tabcgi.exe?cnes/cnv/prid02br.def

# Agora vamos criar o dataframe para os estados e a média da taxa brasileira

med_est <- left_join(med_est, pop_est)

taxa_med_est <- med_est %>% 
  mutate(txmed = round((total_medicos / (pop / 1000)),2)) %>% 
  mutate(txmedbr = mean(txmed))

# taxa média do Brasil é 

txmedbr <- round(taxa_med_est$txmedbr, 2)

# 2) criar mapa do Brasil delimitado pelas UFs

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
  geom_sf(data = mapa_uf, aes(fill = txmed), color = 'lightgrey', size = .15) +
  labs(title = 'Taxa de Médicos por 1000 habitantes das UFs brasileiras',
       subtitle = "Dados de outubro de 2024",
       caption = "Fonte: DATASUS, Dezembro de 2024 \n Autor: Pedro Maranhão") +
  geom_sf_text(data = mapa_uf, aes(label = abbrev_state), size = 2.5, nudge_y = 0.6, 
               color = "black") +
  geom_sf_text(data = mapa_uf, aes(label = txmed), size = 2.5, nudge_y = 0, 
               color = "black") +
  scale_fill_distiller(palette = "Blues", 
                       direction = 1, 
                       name = "Taxa de médicos por 1000 habitantes",
                       breaks = intervalos$brks) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(4, "cm"),
                                    height = unit(4, "cm")) +
  annotate("text", x = Inf, y = Inf, label = paste("Taxa Média Brasileira:", txmedbr),
           hjust = 1.1, vjust = 5, size = 4.5, fontface = "bold", family = 'sans') +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = 'bold', family = 'sans', hjust = 0.5),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

# 3) Plotaremos o mapa do estado de São Paulo dividido pelos seus municípios

# extrair os geoms dos municípios de SP

sp_mun <- read_municipality(code_muni = "SP",
                            year = 2020) %>% 
mutate(code_muni = as.character(code_muni),
       code_muni = str_sub(code_muni, end = 6)) %>% 
  rename(codigo_mun = code_muni) 

# left join com a taxa de médicos dos municípios

mapa_sp <- left_join(sp_mun, taxa_medicos) 

# Criar intervalos automáticos dos dados para as cores do mapa

intervalos_sp <- classIntervals(mapa_sp$txmed, n = 6, style = "equal")

intervalos_sp$brks <- round(intervalos_sp$brks, 2)

# transformar o mapa_uf em geometry

mapa_sp <- st_as_sf(mapa_sp)

# plotar

# Selecionar os 4 municípios com maiores taxas

municipios_destaque <- mapa_sp %>%
  arrange(desc(txmed)) %>%
  slice(1:4)

# Plotar Mapa

ggplot() +
  geom_sf(data = mapa_sp, aes(fill = txmed), color = 'lightgray', size = .15) +
  labs(title = 'Taxa de Médicos por 1000 habitantes dos municípios de SP',
       subtitle = 'Dados de outubro de 2024', 
       caption = "Fonte: DATASUS, Dezembro de 2024 \n Autor: Pedro Maranhão") +
  scale_fill_distiller(palette = "Blues", 
                       direction = 1, 
                       name = "Taxa de médicos por 1000 habitantes",
                       breaks = intervalos_sp$brks) +
  geom_sf_text(data = municipios_destaque, 
               aes(label = paste(name_muni, round(txmed, 2))), 
               size = 3, color = "black", nudge_y = 0.2) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(4, "cm"),
                                    height = unit(4, "cm")) +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
        plot.title.position = "plot",  
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

# 4) plotar as cidades de SP em que não há médicos

# primeiro criar o dataframe

zero_medicos_sp <- mapa_sp %>% 
  filter(txmed == 0)

zero_medicos_sp <- st_as_sf(zero_medicos_sp)

nome_municipios <- data.frame(Municipios = zero_medicos_sp$name_muni)

# Plotar o mapa
ggplot() +
  geom_sf(data = mapa_sp, aes(fill = ifelse(txmed == 0, "Sem Médicos", "Possuem Médicos")), 
          color = 'lightgray', size = .15) + labs(title = 'Municípios de SP sem médicos',
       subtitle = 'Dados de Outubro de 2024',
       caption = "Fonte: DATASUS, Dezembro de 2024 \n Autor: Pedro Maranhão") +
  scale_fill_manual(values = c("Sem Médicos" = "firebrick", "Possuem Médicos" = "white"), 
                    name = "Situação dos Municípios") + 
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(4, "cm"),
                                    height = unit(4, "cm")) +
  annotation_custom(
    grob = tableGrob(nome_municipios, 
                     theme = ttheme_minimal(base_size = 10)),  
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))


# 5) Gráfico de barras horizontais com os 10 municípios com maiores taxas e 10 menores

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
    subtitle = 'Dados de Outubro de 2024',
    caption = "Fonte: DATASUS, Dezembro de 2024 \n Autor: Pedro Maranhão") +
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
    plot.subtitle = element_text(hjust = 0.5),
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
  geom_col(fill = "firebrick") +  
  theme_classic() +
  labs(
    title = "Top 10 menores taxas de médicos por 1000 habitantes das cidades de SP",
    x = NULL,
    y = "Taxa de médicos por 1000 habitantes",
    subtitle = 'Dados de Outubro de 2024',
    caption = "Fonte: DATASUS, Dezembro de 2024 \n Autor: Pedro Maranhão") +
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
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  expand_limits(y = c(0, max(menores_sp$txmed) * 1.15)) + 
  coord_flip()



