#############################################################################################################
################   PROVA DE CIÊNCIA DE DADOS DA 4INTELIGENCE   ##############################################
#############################################################################################################

options(scipen = 999)

library(readxl)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(Cairo)
library(viridis)
library(prophet)

setwd("C:/Users/livia/Desktop/4intelligence")

# Abrindo o Dataset
prova <- read_excel("Bases_Final_ADS_Jun2021.xlsx", sheet = 2 )
dicionario <- read_excel("Bases_Final_ADS_Jun2021.xlsx", sheet = 1 )

# Criando a variavel ano
prova$ano <- year(prova$data_tidy)

summary(prova)


###################################################################################################
###############################     QUESTÃO 01     ------------------------------------------------
###################################################################################################


############## Análise dp Consumo de Energia  ##################################################

# Criando Banco  sel
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

# Grafico Produção Industrial Total
grafico_producao <- ggplot(producao_industrial_ano, aes(x=ano, y= producaototal, colour = regiao))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x="", y="Produção Industrial", title = "Produção Industrial Total, por Região. 2004-2021.",
       caption = "*Os dados de 2021 vão somente até Fevereiro.", colour = "Região: ")+
  scale_x_continuous(breaks = seq(2004, 2021, 1), limits = c(2004, 2021))+
  theme(legend.position = "bottom", title = element_text(size = 10))



# Grafico Produção Industrial Medio
grafico_producao_md <- ggplot(producao_industrial_ano, aes(x=ano, y= producaomedio, colour = regiao))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x="", y="Produção Industrial", title = "Produção Média Industrial, por Região. 2004-2021.",
       caption = "*Os dados de 2021 vão somente até Fevereiro.", colour = "Região: ")+
  scale_x_continuous(breaks = seq(2004, 2021, 1), limits = c(2004, 2021))+
  theme(legend.position = "bottom", title = element_text(size = 10))

producao_total <- arrangeGrob(grafico_producao, grafico_producao_md, ncol= 2)

# Salvando em PNG
ggsave(plot = producao_total, "producao_total.png",
       width = 14, height = 6, dpi = 450, units = "in",type = "cairo")


##############   Análise Pesquisa Mensal do Comércio Ampliada     ##################################################

pmca <- prova %>% select(data_tidy, ano, pmc_a_co, pmc_a_n, pmc_a_ne, pmc_a_s, pmc_a_se)

pmca_ano <- pmca %>%
  gather(regiao, comercio, -c(data_tidy, ano)) %>%
  drop_na() %>% 
  group_by(ano, regiao) %>% 
  summarise(comerciototal= sum(comercio),
            comerciomedio = mean(comercio))

# Grafico Produção Industrial Total
grafico_pmca <- ggplot(pmca_ano, aes(x=ano, y= comerciototal, colour = regiao))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x="", y="Índice", title = "Total da Pesquisa Mensal do Comércio Ampliada, por Região. 2004-2021.",
       caption = "*Os dados de 2021 vão somente até Fevereiro.\nFonte: IBGE", colour = "Região: ")+
  scale_x_continuous(breaks = seq(2004, 2021, 1), limits = c(2004, 2021))+
  theme(legend.position = "bottom", title = element_text(size = 10))+
  scale_color_viridis(discrete = TRUE, option = "C")



# Grafico Produção Industrial Medio
grafico_pmca_md <- ggplot(pmca_ano, aes(x=ano, y= comerciomedio, colour = regiao))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x="", y="Índice", title = "Média da Pesquisa Mensal do Comércio Ampliada, por Região. 2004-2021.",
       caption = "*Os dados de 2021 vão somente até Fevereiro.\nFonte: IBGE", colour = "Região: ")+
  scale_x_continuous(breaks = seq(2004, 2021, 1), limits = c(2004, 2021))+
  theme(legend.position = "bottom", title = element_text(size = 10))+
  scale_color_viridis(discrete = TRUE, option = "C")

pmca_total <- arrangeGrob(grafico_pmca, grafico_pmca_md, ncol= 2)

# Salvando em PNG
ggsave(plot = pmca_total, "pmca_total.png",
       width = 14, height = 6, dpi = 450, units = "in",type = "cairo")



###################################################################################################
###############################     QUESTÃO 02     ------------------------------------------------
###################################################################################################

## Algoritmo de previsão

# Selecionando as variáveis de interesse
consumo_sudeste <- prova %>% select(data_tidy, ind_se) %>% drop_na()

# Trocando os nomes das colunas
colnames(consumo_sudeste) <- c("ds", "y")

# Iniciado o algoritmo de previsão do prophet
v1 <- prophet(consumo_sudeste) 

# Criando dataframe para previsão
pred <- make_future_dataframe(v1, periods = 24, freq = "month")

# Fazendo a previsão
previsao <- predict(v1, pred)

# Estatísticas da serie temporal
prophet_plot_components(v1,previsao)

#Gráfico da previsão

tail(previsao[c("ds","yhat","yhat_lower","yhat_upper")])

plot(v1, previsao, xlab = "ano", ylab ="valor")+add_changepoints_to_plot(v1)

df.cv <- cross_validation(v1, horizon = 365/12, units = "days")

# Acuracia

dataframe_acuracia <- data.frame(data = previsao$ds, modelo = previsao$yhat)

dataframe_acuracia <- dataframe_acuracia[1:206,]

dataframe_acuracia <- cbind(dataframe_acuracia, consumo_sudeste)

testes <- lm(dataframe_acuracia$y ~ dataframe_acuracia$modelo)

summary(testes)
