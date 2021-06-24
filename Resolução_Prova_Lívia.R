#############################################################################################################
################   PROVA DE CIÊNCIA DE DADOS DA 4INTELIGENCE   ##############################################
#############################################################################################################

install.packages("tidymodels")
library(readxl)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(Cairo)

# Abrindo o Dataset
prova <- read_excel("Bases_Final_ADS_Jun2021.xlsx", sheet = 2 )
dicionario <- read_excel("Bases_Final_ADS_Jun2021.xlsx", sheet = 1 )

# Criando a variavel ano
prova$ano <- year(prova$data_tidy)

summary(prova)

############## Análise dp Consumo de Energia  ##################################################

consumo_comercial <- prova %>% select(data_tidy, ano, com_co, com_n, com_ne, com_s, com_se)

consumo_industrial <- prova %>% select(data_tidy, ano, ind_co, ind_n, ind_ne, ind_s, ind_se)

consumo_residencial <- prova %>% select(data_tidy, ano, res_co, res_n, res_ne, res_s, res_se)

consumo_comercial_ano <- consumo_comercial %>%
                         gather(regiao, consumo, -c(data_tidy, ano)) %>%
                         drop_na() %>% 
                         group_by(ano, regiao) %>% 
                         summarise(consumototal= sum(consumo, na.rm =TRUE),
                                   consumomedio = mean(consumo, na.rm =TRUE))
# Grafico Consumo Comercial 
grafico_comercial <- ggplot(consumo_comercial_ano, aes(x=ano, y= consumototal, colour = regiao))+
       geom_line()+
       theme_minimal()+
       labs(x="", y="Consumo Total", title = "Consumo Total de Energia Comercial, por Região. 2004-2021*.",
            caption = "*Os dados de 2021 vão somente até Fevereiro.", colour = "Região: ")+
       scale_x_continuous(breaks = seq(2004, 2021, 2), limits = c(2004, 2021))+
       theme(legend.position = "bottom", title = element_text(size = 8))


consumo_industrial_ano <- consumo_industrial %>%
  gather(regiao, consumo, -c(data_tidy, ano)) %>%
  drop_na() %>% 
  group_by(ano, regiao) %>% 
  summarise(consumototal= sum(consumo, na.rm =TRUE),
            consumomedio = mean(consumo, na.rm =TRUE))

# Grafico Consumo Industrial 
grafico_industrial <- ggplot(consumo_industrial_ano, aes(x=ano, y= consumototal, colour = regiao))+
  geom_line()+
  theme_minimal()+
  labs(x="", y="Consumo Total", title = "Consumo Total de Energia Industrial, por Região. 2004-2021*.",
       caption = "*Os dados de 2021 vão somente até Fevereiro.", colour = "Região: ")+
  scale_x_continuous(breaks = seq(2004, 2021, 2), limits = c(2004, 2021))+
  theme(legend.position = "bottom", title = element_text(size = 8))

consumo_residencial_ano <- consumo_residencial %>%
  gather(regiao, consumo, -c(data_tidy, ano)) %>%
  drop_na() %>% 
  group_by(ano, regiao) %>% 
  summarise(consumototal= sum(consumo, na.rm =TRUE),
            consumomedio = mean(consumo, na.rm =TRUE))

# Grafico Consumo Residencial 
grafico_residencial <- ggplot(consumo_residencial_ano, aes(x=ano, y= consumototal, colour = regiao))+
  geom_line()+
  theme_minimal()+
  labs(x="", y="Consumo Total", title = "Consumo Total de Energia Residencial, por Região. 2004-2021*.",
       caption = "*Os dados de 2021 vão somente até Fevereiro.", colour = "Região: ")+
  scale_x_continuous(breaks = seq(2004, 2021, 2), limits = c(2004, 2021))+
  theme(legend.position = "bottom", title = element_text(size = 8))

grafico_total <- arrangeGrob(grafico_comercial, grafico_industrial, grafico_residencial, ncol= 3)

# Salvando em PNG
ggsave(plot = grafico_total, "grafico_total.png",
       width = 14, height = 6, dpi = 450, units = "in",type = "cairo")



##############   Análise da Produção Industrial      ##################################################

producao_industrial <- prova %>% select(data_tidy, ano, pim_co, pim_n, pim_ne, pim_s, pim_se)

producao_industrial_ano <- producao_industrial %>%
  gather(regiao, producao, -c(data_tidy, ano)) %>%
  drop_na() %>% 
  group_by(ano, regiao) %>% 
  summarise(producaototal= sum(producao),
            producaomedio = mean(producao))

# Grafico Produção Industrial 
grafico_producao <- ggplot(producao_industrial_ano, aes(x=ano, y= producaototal, colour = regiao))+
  geom_line()+
  theme_minimal()+
  labs(x="", y="Produção Insdustrial", title = "Produção Insdustrial, por Região. 2004-2021.",
       caption = "*Os dados de 2021 vão somente até Fevereiro.", colour = "Região: ")+
  scale_x_continuous(breaks = seq(2004, 2021, 1), limits = c(2004, 2021))+
  theme(legend.position = "bottom", title = element_text(size = 10))


consumo_industrial_ano <- consumo_industrial %>%
  gather(regiao, producao, -c(data_tidy, ano)) %>%
  drop_na() %>% 
  group_by(ano, regiao) %>% 
  summarise(consumototal= sum(, na.rm =TRUE),
            consumomedio = mean(consumo, na.rm =TRUE))