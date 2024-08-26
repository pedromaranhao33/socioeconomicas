####################### Autor: Pedro Maranhão

### Comparar salário minímo nominal com salário mínimo real de 2014 a 2022 
### utilizando o IPCA de julho de 2024 

# evitar notação científica

options(scipen = 999)

# bibliotecas

library(tidyverse)
library(ipeadatar)
library(sidrar)

# working directory

setwd('C:/Users/55819/Desktop/scripts_r')

# utilizar a função available_series para encontrar o nome da base 
# em que se encontra os dados do salário mínimo

series <- ipeadatar::available_series()

# extrair dados do salário mínimo

sal_min <- ipeadata('MTE12_SALMIN12')

# extrair dados de 10 anos de salário mínimo (2014 a 2024)

sal_min <- sal_min %>% 
  select(date, value) %>% 
  filter(date >= '2014-01-01')

# extrair IPCA do sidra 6691

ipca <- get_sidra(api = "/t/6691/n1/all/v/2266/p/all/d/v2266%2013")

# fazer slice, modificar nome de colunas e formatar a data

ipca <- ipca %>% 
  select(indice = Valor, 
         date = `Mês (Código)`) %>% 
  mutate(date = ym(date))

# realizar Join entre as bases e retirar os NA's 

sal_real <- left_join(ipca, sal_min) %>% 
  na.omit()

# deflacionar utilizando o último valor do IPCA com a função last 
# para criar o fator e o valor real

sal_real <- sal_real %>% 
  mutate(fator = (last(indice) / indice)) %>% 
  mutate(real = fator*value)

# plotando gráfico da série histórica

ggplot(data = sal_real, aes(x = date)) +
  geom_line(aes(y = value, colour = "Nominal"),size = 1.) +
  geom_line(aes(y = real, colour = "Real"), size = 1.) +
  scale_color_manual(NULL, values = c("#3182BD","#E44CB7")) +
  scale_y_continuous(n.breaks = 16, name = "Reais (R$)") +
  labs(x = "Ano", title = "Salário Mínimo no Brasil", 
       subtitle = "Salário Mínimo Real deflacionado pelo IPCA a preços de Julho de 2024",
       caption = "Fonte: IPEA e IBGE") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))
