rm(list = ls())

install.packages("patchwork")
install.packages("summarytools")
install.packages("credentials")

library(credentials)
library(summarytools)
library(patchwork)
library(ggspatial)
library(RColorBrewer)
library(geobr)
library(janitor)
library(sf)
library(ggplot2)
library(tidyverse)
library(tibble)
library(readxl)
library(dplyr)

####################################################################################################################################################################################

setwd("C:/Users/Flávia Cristina/Documents/pos/estatistica/Dados Metereologicos/")

##### Distribuião de Frequência ###############################################################################################################################################################################

#Dados
dados <- read.csv2("Dados Metereologicos.csv") %>% 
         janitor::clean_names() %>%
         as_tibble() %>% 
         select( -c(velocidade_vento, umidade_ar, temp_media_c))
        
#Gráficos Individuais
#Freq. Absoluta
ggplot(dados,aes(x=precipitacao_mm))+
  geom_histogram(bins = 12,
                 col="blue",
                 fill="lightblue")+
  labs(x="Precipitação Média (mm)",y = "Frequência Absoluta",title = "Histograma")+
  theme_minimal()

#Freq. Relativa
ggplot(dados,aes(x = precipitacao_mm, y = (..count../sum(..count..))*100))+
  geom_histogram(bins = 12,
                 col="blue",
                 fill="lightblue")+
  labs(x="Precipitação Média (mm)",y = "Frequência Relativa",title = "Histograma")+
  theme_minimal()

#Freq. Acumulada
ggplot(dados,aes(x=precipitacao_mm,cumsum(..count..)))+
  geom_histogram(bins = 12,
                 col="blue",
                 fill="lightblue")+
  labs(x="Precipitação Média (mm)",y = "Frequência Acumulada",title = "Histograma")+
  theme_minimal()

#Freq. Relativa Acumulada
ggplot(dados,aes(x=precipitacao_mm, y = cumsum((..count../sum(..count..))*100)))+
  geom_histogram(bins = 12,
                 col="blue",
                 fill="lightblue")+
  labs(x="Precipitação Média (mm)",y = "Frequência Relativa Acumulada(%)",title = "Histograma")+
  theme_minimal()

#Gráficos Juntos
#Freq. Absoluta
g1 <- ggplot(dados,aes(x=precipitacao_mm))+
  geom_histogram(bins = 12,
                 col="blue",
                 fill="lightblue")+
  labs(x="Precipitação Média (mm)",y = "Frequência Absoluta")+
  theme_minimal()

#Freq. Relativa
g2 <- ggplot(dados,aes(x = precipitacao_mm, y = (..count../sum(..count..))*100))+
  geom_histogram(bins = 12,
                 col="blue",
                 fill="lightblue")+
  labs(x="Precipitação Média (mm)",y = "Frequência Relativa")+
  theme_minimal()

#Freq. Acumulada
g3 <- ggplot(dados,aes(x=precipitacao_mm,cumsum(..count..)))+
  geom_histogram(bins = 12,
                 col="blue",
                 fill="lightblue")+
  labs(x="Precipitação Média (mm)",y = "Frequência Acumulada")+
  theme_minimal()

#Freq. Relativa Acumulada
g4 <- ggplot(dados,aes(x=precipitacao_mm, y = cumsum((..count../sum(..count..))*100)))+
  geom_histogram(bins = 12,
                 col="blue",
                 fill="lightblue")+
  labs(x="Precipitação Média (mm)",y = "Frequência Relativa Acumulada(%)")+
  theme_minimal()

(g1 + g2)/(g3 + g4) + plot_annotation(title = "Estatistica Descritiva",
                                      subtitle = "Nome: BELO HORIZONTE - CERCADINHO
                                                  Codigo Estacao: F501
                                                  Latitude: -19.97999999
                                                  Longitude: -43.95861111
                                                  Altitude: 1199.55
                                                  Situacao: Operante
                                                  Data Inicial: 2020-01-01
                                                  Data Final: 2020-01-31
                                                  Periodicidade da Medicao: Diaria",
                                      tag_levels = c("A","B","C","D"),
                                      tag_prefix = "Gráfico ")

##### Tabela de Distribuião  de Frequência ###############################################################################################################################################################################

dados.cat <- cut(dados$precipitacao_mm, breaks = c(0,57,114,180),
                 right = F,
                 labels = c("0-57","57-114","114-180")) 

summarytools::freq(dados.cat,report.nas = FALSE,
                   style = "rmarkdown")

##### Média  ###############################################################################################################################################################################

prec_media <- read.csv2("Dados Metereologicos.csv") %>% 
              janitor::clean_names() %>% 
              as_tibble() %>%
              select( -c(velocidade_vento, umidade_ar, temp_media_c)) %>% 
              dplyr::group_by(nome) %>%                 #Agrupou os dados de acordo com o nome da estaão;
              summarise(prec_media = mean(precipitacao_mm, na.rm = T))     #A funão "summarise()" quando aplicada em dados agrupados, retornara a média também agrupada.


##### Mediana  ###############################################################################################################################################################################

prec_mediana <- read.csv2("Dados Metereologicos.csv") %>% 
                janitor::clean_names() %>% 
                as_tibble() %>%
                select( -c(velocidade_vento, umidade_ar, temp_media_c)) %>% 
                dplyr::group_by(nome) %>%                 #Agrupou os dados de acordo com o nome da estaão;
                summarise(prec_mediana = median(prec_mediana$precipitacao_mm, na.rm = T))     #A funão "summarise()" quando aplicada em dados agrupados, retornara a média também agrupada.

##### Medida de dispersão ###############################################################################################################################################################################
##### Variãncia  ###############################################################################################################################################################################

prec_varianca <-  read.csv2("Dados Metereologicos.csv") %>% 
                  janitor::clean_names() %>% 
                  as_tibble() %>% 
                  select( -c(velocidade_vento, umidade_ar, temp_media_c)) %>% 
                  dplyr::group_by(nome) %>%                                       #Agrupou os dados de acordo com o nome da estaão;
                  summarise(prec_variacia = var(precipitacao_mm, na.rm = T))
                                      
##### Desvio Padrão ###############################################################################################################################################################################

prec_desvio <-  read.csv2("Dados Metereologicos.csv") %>% 
                janitor::clean_names() %>% 
                as_tibble() %>% 
                select( -c(velocidade_vento, umidade_ar, temp_media_c)) %>% 
                dplyr::group_by(nome) %>%                                       #Agrupou os dados de acordo com o nome da estaão;
                summarise(prec_desvio = sd(precipitacao_mm, na.rm = T))         #A funão "summarise()" quando aplicada em dados agrupados, retornara a média também agrupada.


##### Rascunhoo ###############################################################################################################################################################################
notas <- c(9,3,7,1,4,4,4,7,7,9,0,NA)

#Amostra
var_amostra <- var(dados$precipitacao_mm, na.rm = T)

#População
n <- length(notas)
var_pop <- var(notas, na.rm = T)*(n-1)/n

var_pop <- function(x){
        #Bloco de comandos da função 
        n <- length(x)
        var.pop <- var(x, na.rm = T)*(n-1)/n
        return(var.pop)
        }
var_pop(notas)


#Desvio Padrão Amostra
sd(notas,na.rm = T)


sqrt(var_pop(notas))






















