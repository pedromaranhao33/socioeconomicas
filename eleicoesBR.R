# Dados eleições BR

# Extração e transformação de dados sobre candidatos na cidade de 
# Santa Maria da Boa Vista em Pernambuco com a plotagem de um gráfico
# para facilitar a visualização das informações. 

# bibliotecas

library(electionsBR)
library(tidyverse)
library(scales)

# API para extrair dados com informações dos candidatos nas eleições
# de 2020 em Pernambuco

df <- elections_tse(year = 2020, type = "candidate", uf = "PE")

# Slice e filtro para o município de Santa Maria da Boa Vista, um município com
# poucos candidatos para facilitar nossa visualização posterior

df2 <- df %>% 
  select(DS_GRAU_INSTRUCAO, DS_COR_RACA, DS_GENERO, NM_UE) %>% 
  filter(NM_UE == "SANTA MARIA DA BOA VISTA")

# Agrupamento e summarização dos dados  

dfagrup <- df2 %>% 
  group_by(DS_GENERO, DS_GRAU_INSTRUCAO, DS_COR_RACA) %>% 
  summarise(quantidade = n())

# Plotar o gráfico

ggplot(data = dfagrup, aes(x = DS_GRAU_INSTRUCAO, y = quantidade, fill = DS_COR_RACA)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ DS_GENERO) +
  theme_bw() +
  scale_fill_manual(values = c("#DB7F67","#4682B4","#D34F73")) +
  theme(legend.title = element_blank(),
        legend.text = element_text(face = "bold", color = "#000000", size = 10),
        legend.key.size = unit(0.8, 'cm'), 
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 15, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 15),
        plot.margin = margin(t = 20, r = 8, b = 7, l = 5),
        legend.position = 'right',
        panel.grid.major = element_line(color = "gray", linewidth = 0.5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 20)) +
  labs(x = '', y = "Quantidade de candidatos",
       title = "Quantidade de candidatos por sexo e raça",
       subtitle = "SANTA MARIA DA BOA VISTA (PE) - Eleições municipais de 2020",
       caption = "Desenvolvido por Pedro Maranhão") +
  coord_flip() 
  
