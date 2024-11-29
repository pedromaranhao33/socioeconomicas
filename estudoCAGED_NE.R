# Estudo utilizando Caged de Outubro de 2024 para os Municípios da região Nordeste do Brasil

# Nesse estudo vamos realizar algumas visualizações:

# 1) Saldo por Grande Agrupamento de Atividade Econômica do Nordeste
# 2) Tabelas com 10 municípios do Nordeste com maior e piores saldo de emprego
# 3) Mapa sobre Saldo de emprego de cada município do Nordeste para Outubro de 2024
# 4) Mapa sobre saldo de emprego da indústria de transformação 
# 5) 10 empregos com maiores salários presentes a movimentação desse mês para a região

# Bibliotecas

library(tidyverse)
library(rio)
library(curl)
library(archive)
library(geobr) 
library(sf) 
library(tmaptools) 
library(ggspatial)
library(knitr)
library(kableExtra)


# aumentar o timeout do download

options(timeout = 600)

# extração dos dados do CAGED

# mov

urlmov <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/2024/202410/CAGEDMOV202410.7z"
destinomov <- "C:/Users/55819/Desktop/scripts_r/CAGEDMOV202410.7z"

curl_download(urlmov, destfile = destinomov)

# descompactar o arquivo

archive_extract('C:/Users/55819/Desktop/scripts_r/CAGEDMOV202410.7z', dir = "C:/Users/55819/Desktop/scripts_r/")

# importar os dados

df <- import("C:/Users/55819/Desktop/scripts_r/CAGEDMOV202410.txt")


# Algumas subclasses (7 dígitos) da CNAE no dataframe começam com '0' 
# e, por padrão ele é omitido na extração. Com efeito, precisamos adicionar 
# o '0' na frente nos valores que tem 6 dígitos para poder realizar nosso estudo

df <- df %>%
  mutate(subclasse = as.character(subclasse),                    
         subclasse = ifelse(nchar(subclasse) == 6,                 
                            str_pad(subclasse, width = 7, pad = "0"),  
                            subclasse))    

# fazer filtro para a região Nordeste (região == 2)
# e renomear a coluna de municípios para futuro join

df <- df %>% 
  filter(região == 2) %>% 
  rename(code_muni = município)

# ESTUDOS

# 1) Saldo por Grande Agrupamento de Atividade Econômica do Nordeste 

df_g_setores <- df %>%
  mutate(g_setores = case_when(
    seção == "A" ~ "Agricultura",
    seção %in% c("B", "C", "D", "E") ~ "Indústria",
    seção == "F" ~ "Construção",
    seção == "G" ~ "Comércio",
    seção %in% LETTERS[8:20] ~ "Serviços",
    TRUE ~ 'Não Identificado'  
  )) %>%
  group_by(g_setores) %>%
  summarise(
    saldo = sum(saldomovimentação, na.rm = TRUE))

# criar um gráfico de barras com esses resultados

ggplot(df_g_setores, aes(x = reorder(g_setores, saldo), y = saldo)) +  
  geom_col(aes(fill = ifelse(g_setores == "Construção", "Construção", "Outros"))) +  
  scale_fill_manual(values = c("Construção" = "firebrick", "Outros" = "#3A66A0")) +  
  theme_classic() +
  labs(
    title = "Saldo de emprego dos grandes setores econômicos do Nordeste Brasileiro 10/2024",
    x = NULL,
    y = "Saldo de empregos",
    caption = "Fonte: CAGED, Novembro de 2024 \n Autor: Pedro Maranhão") +
  geom_text(
    aes(
      label = format(round(saldo, 0), big.mark = "."),
      hjust = ifelse(saldo < 0, -0.2, 1.2),
      color = ifelse(g_setores == "Construção", "white",
                     ifelse(g_setores == "Não Identificado", "black", "white"))), size = 3.5) +
  scale_color_identity() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
    plot.title.position = "plot", 
    axis.title.y = element_text(size = 10, family = "sans"),
    axis.text = element_text(size = 10, family = "sans"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none") +
  expand_limits(y = range(df_g_setores$saldo) * 1.10) + 
  coord_flip()

# 2) tabelas com 10 municípios do Nordeste com maior e piores saldo de emprego

# transformação da tabela original

dfsaldo <- df %>% 
  group_by(code_muni) %>% 
  summarise(sum(saldomovimentação)) %>% 
  mutate(code_muni = as.character(code_muni)) %>% 
  rename(saldo = `sum(saldomovimentação)`)

# Por enquanto temos apenas os códigos dos municípios, vamos então transformar a base
# utilizando os dados dos geoms extraídos por meio do pacote geobr para trazer os nomes
# das cidades. Utilizaremos os geoms posteriormente no nosso estudo para plotarmos mapas.

# Primeiro vamos extrar os geoms dos estados

mapauf_ne <- geobr::read_state(year = 2020) %>% 
  mutate(code_state = as.character(code_state)) 

### vamos utilizar a função read_region para extrair os dados necessários
# para localizar as regiões no mapa do Brasil e transformar a coluna code_region  
# para o tipo character e filtrar pela região Nordeste

mapa_ne <- read_municipality(year = 2020) %>% 
  mutate(code_muni = as.character(code_muni)) %>% 
  mutate(code_muni = str_sub(code_muni, end = 6)) %>% 
  filter(code_region == 2)

# vamos realizar a junção dos nossos dados do IBGE com os dados de localização
# com atenção para termos todos os municípios da reunião, pois em alguns municípios pequenos
# podem não ter acontecido admissões ou demissões no mês portanto não contabilizados
# pelo caged

mapa_ne <- full_join(dfsaldo, mapa_ne)

# Transformar os dados de NA em 0

mapa_ne$saldo <- replace_na(mapa_ne$saldo, 0)

# transformar o mapa_uf em geometry

mapa_ne <- st_as_sf(mapa_ne)

# criar um outro dataframe para marcar a divisa entre os estados do Nordeste utilizando o mapa_uf
# filtrando o código dos estados entre 21 ao 29

mapauf_ne <- mapauf_ne %>% 
  filter(str_sub(code_state, 1, 2) %in% c("21", "22", "23", "24", "25", "26", "27", "28", "29"))

# criar uma coluna para 

mapa_ne$saldo_cat <- ifelse(mapa_ne$saldo > 0, "positivo", 
                            ifelse(mapa_ne$saldo < 0, "negativo", "zero"))

# Agora vamos fazer uma tabela utilizando a biblioteca knitr e a função kable 

# Os 10 municípios com maiores saldos de emprego

# Filtrando os 10 maiores valores da coluna 'saldo' e selecionando as colunas desejadas

mapa_ne_top10 <- mapa_ne %>%
  as.data.frame() %>% 
  arrange(desc(saldo)) %>%         
  select(Município = name_muni, UF = abbrev_state, Saldo = saldo) %>%  
  head(10)                          


# Usando kableExtra para criar uma tabela mais estilizada

mapa_ne_top10 %>%
  kable(caption = "Top 10 Maiores Saldo de Emprego 10/2024") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = F,
    font_size = 16
  ) %>%
  column_spec(3, color = "white", background = "#3A66A0") %>%
  add_header_above(c(" " = 3), bold = TRUE) 

# aproveitando para criar tabela com os 10 municípios com saldos negativos mais expressivos

mapa_ne_buttom10 <- mapa_ne %>%
  as.data.frame() %>% 
  arrange((saldo)) %>%          # Ordena em ordem crescente
  select(Município = name_muni, UF = abbrev_state, Saldo = saldo) %>%  # Seleciona as 3 colunas
  head(10)                          # extrai os 10 menores

# Usando kableExtra para criar uma tabela mais estilizada

mapa_ne_buttom10 %>%
  kable(caption = "Top 10 Saldo negativos de emprego 10/2024") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = F,
    font_size = 16
  ) %>%
  column_spec(3, color = "white", background = "firebrick") %>%
  add_header_above(c(" " = 3), bold = TRUE) 


# 3) mapa sobre Saldo de cada município para Outubro de 2024

# agora vamos plotar o mapa cloroplético do Nordeste de acordo com o saldo

ggplot() +
  # Camada dos municípios do Nordeste
  geom_sf(data = mapa_ne, aes(fill = saldo_cat), color = 'lightgray', size = .05) +  # Linhas dos municípios
  
  # Limites dos estados do Nordeste (por cima dos municípios)
  geom_sf(data = mapauf_ne, fill = NA, color = 'black', size = .6) +  # Limites mais espessos para os estados
  
  # Legendas e informações adicionais
  labs(title = 'Saldo de Emprego dos municípios do Nordeste brasileiro em 10/2024', size = 10,
       caption = "Fonte: CAGED, Novembro de 2024 \n Autor: Pedro Maranhão") +
  
  # Escala de cores manual para saldo positivo, negativo e zero
  scale_fill_manual(values = c("positivo" = "royalblue", "negativo" = "firebrick", "zero" = "white"), 
                    name = "Saldo de empregos") +
  
  # Adiciona escala gráfica e seta de orientação
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(3, "cm"),
                                    height = unit(3, "cm")) +
  
  # Tema visual
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),
        plot.title.position = "plot", 
        plot.caption = element_text(hjust = 0.5))

# 4) agora vamos fazer o mesmo porém em relação à indústria de transformação (seção C)

df_indtransf <- df %>% 
  filter(seção == 'C') %>%   
  group_by(code_muni) %>% 
  summarise(sum(saldomovimentação)) %>% 
  mutate(code_muni = as.character(code_muni)) %>% 
  rename(saldo_indtransf = `sum(saldomovimentação)`)

# vamos realizar a junção dos nossos dados do IBGE com os dados de localização
# com atenção para termos todos os municípios da reunião, pois em alguns municípios pequenos
# podem não ter acontecido admissões ou demissões no mês portanto não contabilizados
# pelo caged

mapa_ne_indtransf <- full_join(df_indtransf, mapa_ne)

# Transformar os dados de NA em 0

mapa_ne_indtransf$saldo_indtransf <- replace_na(mapa_ne_indtransf$saldo_indtransf, 0)

# transformar o mapa_uf em geometry

mapa_ne_indtransf <- st_as_sf(mapa_ne_indtransf)

# criar uma coluna para 

mapa_ne_indtransf$saldo_indtransf <- ifelse(mapa_ne_indtransf$saldo_indtransf > 0, "positivo", 
                                            ifelse(mapa_ne_indtransf$saldo_indtransf < 0, "negativo", "zero"))

# Plot

ggplot() +
  # Camada dos municípios do Nordeste
  geom_sf(data = mapa_ne_indtransf, aes(fill = saldo_indtransf), color = 'lightgray', size = .05) +  # Linhas dos municípios
  
  # Limites dos estados do Nordeste (por cima dos municípios)
  geom_sf(data = mapauf_ne, fill = NA, color = 'black', size = .6) +  # Limites mais espessos para os estados
  
  # Legendas e informações adicionais
  labs(title = 'Saldo dos empregos da indústria de transformação dos municípios do Nordeste brasileiro em 10/2024', size = 4,
       caption = "Fonte: CAGED, Novembro de 2024 \n Autor: Pedro Maranhão") +
  
  # Escala de cores manual para saldo positivo, negativo e zero
  scale_fill_manual(values = c("positivo" = "royalblue", "negativo" = "firebrick", "zero" = "white"), 
                    name = "Saldo de empregos") +
  
  # Adiciona escala gráfica e seta de orientação
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = north_arrow_nautical,
                                    width = unit(3, "cm"),
                                    height = unit(3, "cm")) +
  
  # Tema visual
  theme_void() +
  theme(plot.title = element_text(size = 14, face = "bold", family = "sans", hjust = 0.5),
        plot.title.position = "plot", 
        plot.caption = element_text(hjust = 0.5))

# 5) 10 empregos com maiores salários da região em outubro de 2024

# importar tabela cnae

cnae <- import('https://cnae.ibge.gov.br/images/concla/documentacao/CNAE_Subclasses_2_3_Estrutura_Detalhada.xlsx',
               skip = 3)

# modificações no dataframe dos cnaes e retirar os símbolos da subclasse

cnae <- cnae %>% 
  select(subclasse = Subclasse, nome = ...6) %>% 
  na.omit() %>% 
  mutate(subclasse = gsub("[-/]", "", subclasse))


# operações com o dataframe df para  

df_salario <- df %>%
  select(subclasse, salário, code_muni) %>%
  mutate(salário = gsub("[^0-9.,]", "", salário),   # Remove todos os caracteres que não sejam números, vírgulas ou pontos
         salário = as.numeric(gsub(",", ".", salário))) %>%   # Substitui vírgulas por pontos e converte para numérico
  group_by(subclasse) %>% 
  summarise(salario = round(mean(salário),2)) %>% 
  left_join(cnae) %>% 
  arrange(desc(salario)) %>%  
  slice_head(n = 10) 

# Plotar gráfico de barras com os 10 empregos de maiores salários presentes 
# na movimentação do CAGED de agosto de 2024


ggplot(df_salario, aes(x = reorder(nome, salario), y = salario)) +  
  geom_col(fill = "#3A66A0") +  
  theme_classic() +
  labs(
    title = "Top 10 Salários presentes na movimentação de emprego do CAGED 10/2024 ",
    x = NULL,
    y = "Reais (R$)",
    caption = "Fonte: CAGED, Novembro de 2024 \n Autor: Pedro Maranhão") +
  geom_text(
    aes(label = paste0(format(round(salario, digits = 2), nsmall = 1, decimal.mark = ","))),
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
  expand_limits(y = c(0, max(df_salario$salario) * 1.15)) + 
  coord_flip()




