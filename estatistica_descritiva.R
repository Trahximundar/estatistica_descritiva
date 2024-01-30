rm(list = ls())

##### Biblbiotecas #################################################################################################################################################################################

install.packages("patchwork")
install.packages("summarytools")
install.packages("credentials")
install.packages("glue")

library(glue)
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

##### Curso Mercel #################################################################################################################################################################################

##### Diretório de Trabalho #################################################################################################################################################################################

setwd("C:/Users/Flávia Cristina/Documents/pos/estatistica/Dados Metereologicos/")

##### Leitura e tratamento dos Dados ###############################################################################################################################################################################

#Dados
 dados <- read.csv("C:/Users/Flávia Cristina/Documents/pos/estatistica/Dados Metereologicos/estatistica_descritiva/dados_pnad_2015.csv") %>% 
          janitor::clean_names() %>%
          as_tibble() 

##### Gráficos Individuais #################################################################################################################################################################################        

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

##### Gráficos Agrupados #################################################################################################################################################################################

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

#Frequencia e porcentagem por sexo
dist_freq_qualitativas <- cbind(freq = table(dados$sexo), percent = prop.table(table(dados$sexo)) * 100)
colnames(dist_freq_qualitativas) <- c('Frequência', 'Porcentagem (%)')
rownames(dist_freq_qualitativas) <- c('Masculino', 'Feminino')
dist_freq_qualitativas

#Renda média por sexo e raça
medias <- tapply(dados$renda, list(dados$sexo, dados$cor), mean)
rownames(medias) <- c('Masculino', 'Feminino')
colnames(medias) <- c('Indígena', 'Branca', 'Preta', 'Amarela', 'Parda')
medias

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

##### Medida de Dispersão ###############################################################################################################################################################################
##### Variãncia  ###############################################################################################################################################################################

prec_varianica <-  read.csv2("Dados Metereologicos.csv") %>% 
                   janitor::clean_names() %>% 
                   as_tibble() %>% 
                   select( -c(velocidade_vento, umidade_ar, temp_media_c)) %>% 
                   dplyr::group_by(nome) %>%                                       #Agrupou os dados de acordo com o nome da estaão;
                   summarise(prec_variancia = var(precipitacao_mm, na.rm = T))
                                      
##### Desvio Padrão ###############################################################################################################################################################################

prec_desvio <-  read.csv2("Dados Metereologicos.csv") %>% 
                janitor::clean_names() %>% 
                as_tibble() %>% 
                select( -c(velocidade_vento, umidade_ar, temp_media_c)) %>% 
                dplyr::group_by(nome) %>%                                       #Agrupou os dados de acordo com o nome da estaão;
                summarise(prec_desvio = sd(precipitacao_mm, na.rm = T))         #A funão "summarise()" quando aplicada em dados agrupados, retornara a média também agrupada.

##### Quantil ###############################################################################################################################################################################

prec_quantil <- read.csv2("Dados Metereologicos.csv") %>% 
                janitor::clean_names() %>% 
                as_tibble() %>% 
                select( -c(velocidade_vento, umidade_ar, temp_media_c)) %>% 
                dplyr::group_by(nome) %>%                                                        #Agrupou os dados de acordo com o nome da estaão;
                summarise(prec_per_25 = quantile(precipitacao_mm,probs = .25, na.rm = T),        #Percentil 25 ou Primeiro Quartil
                          prec_per_50 = quantile(precipitacao_mm,probs = .50, na.rm = T),        #Percentil 50 ou Segundo Quartil
                          prec_per_75 = quantile(precipitacao_mm,probs = .75, na.rm = T))        #Percentil 75 ou Terceiro Quartil

##### Coeficiente de Variação ###############################################################################################################################################################################

prec_coeficinte_variacao <- read.csv2("Dados Metereologicos.csv") %>% 
                            janitor::clean_names() %>% 
                            as_tibble() %>% 
                            select( -c(velocidade_vento, umidade_ar, temp_media_c)) %>% 
                            dplyr::group_by(nome) %>%                                                                                #Agrupou os dados de acordo com o nome da estaão;
                            summarise(prec_coef_var = sd(precipitacao_mm, na.rm = T)/ mean(precipitacao_mm, na.rm = T) * 100,
                                      cv_categoria = cut(prec_coef_var, breaks = c(-Inf,15,30,Inf), 
                                                                          labels = c("Baixa Dispersão","Média Dispersão","Alta Dispersão")))        

# C.V. <= 15%        tem-se baixa dispersão;
# 15% < C.V. <= 30%  tem-se média dispersão;
# C.V > 30%          tem-se alta dispersão.

##### Distribuião de Frequência ###############################################################################################################################################################################

prec_distribuicao_frequencia <- read.csv2("Dados Metereologicos.csv") %>% 
                                janitor::clean_names() %>% 
                                as_tibble() %>% 
                                select( -c(velocidade_vento, umidade_ar, temp_media_c)) %>% 
                                dplyr::group_by(nome) %>% 
                                summarise(prec_media = mean(precipitacao_mm, na.rm = T),
                                          prec_mediana = median(precipitacao_mm, na.rm = T),
                                          prec_variancia = var(precipitacao_mm, na.rm = T),
                                          prec_desvio = sd(precipitacao_mm, na.rm = T),
                                          prec_per_25 = quantile(precipitacao_mm,probs = .25, na.rm = T),
                                          prec_per_50 = quantile(precipitacao_mm,probs = .50, na.rm = T),       
                                          prec_per_75 = quantile(precipitacao_mm,probs = .75, na.rm = T),        
                                          prec_coef_var = sd(precipitacao_mm, na.rm = T)/ mean(precipitacao_mm, na.rm = T) * 100,
                                          cv_categoria = cut(prec_coef_var, breaks = c(-Inf,15,30,Inf), 
                                                             labels = c("Baixa Dispersão","Média Dispersão","Alta Dispersão")))        

##### Curso Alura #################################################################################################################################################################################
##### Diretório de Trabalho #################################################################################################################################################################################

setwd("C:/Users/Flávia Cristina/Documents/pos/estatistica/Dados Metereologicos/")

##### Leitura e tratamento dos Dados ###############################################################################################################################################################################

#Dados
dados <- read.csv("estatistica_descritiva/dados_pnad_2015.csv") %>% 
         janitor::clean_names() %>%
         as_tibble() 

##### Identificar as categorias #################################################################################################################################################################################        

arrange(unique(select(dados, anos_de_estudo)),anos_de_estudo) #Forma 1

c(arrange(unique(select(dados, anos_de_estudo)),anos_de_estudo)) #Forma 2

##### Minimo e Maximo  #################################################################################################################################################################################        

sprintf('De %s até %s anos', min(dados$idade), max(dados$idade)) #Forma 1

glue('De {min(dados$idade)} até {max(dados$idade)} anos.') #Forma 2

##### Distribuião de Frequência  #################################################################################################################################################################################        
##### Variaveis Qualitativa ##################################################################################################################################

table(dados$sexo)

prop.table(table(dados$sexo)) * 100

dist_freq_qualitativas <- cbind(freq = table(dados$sexo), percent = prop.table(table(dados$sexo)) * 100)

colnames(dist_freq_qualitativas) <- c('Frequência', 'Porcentagem (%)')

rownames(dist_freq_qualitativas) <- c('Masculino', 'Feminino')

dist_freq_qualitativas

frequencia <- table(dados$sexo, dados$cor)
rownames(frequencia) <- c('Masculino', 'Feminino')
colnames(frequencia) <-c('Indígena', 'Branca', 'Preta', 'Amarela', 'Parda')
frequencia <- cbind(frequencia)
frequencia

percentual <- prop.table(frequencia) * 100
percentual

medias <- tapply(dados$renda, list(dados$sexo, dados$cor), mean)  #Renda média por sexo e cor.
rownames(medias) <- c('Masculino', 'Feminino')
colnames(medias) <- c('Indígena', 'Branca', 'Preta', 'Amarela', 'Parda')
medias

##### Variaveis Quantitativas ##################################################################################################################################

min(dados$renda)

max(dados$renda)

classes <- c(0, 1576, 3152, 7880, 15760, 200000)

labels <- c('E', 'D', 'C', 'B', 'A')

frequencia <-  cut(dados$renda,breaks = classes, 
                  labels = labels, 
                  include.lowest = TRUE) %>% 
               table() 
frequencia

percentual <- prop.table(frequencia) * 100
percentual

dist_freq_quantitativas_personalizadas <- cbind('Frequência' = frequencia, 'Porcentagem (%)' = percentual)
dist_freq_quantitativas_personalizadas

dist_freq_quantitativas_personalizadas[
  order(row.names(dist_freq_quantitativas_personalizadas)),
]

##### Rascunho ###############################################################################################################################################################################

notas <- c(6,8,9.78,4,0,6,8,NA)

quantile(notas,probs = .25, na.rm = T) #Percentil 25 ou Primeiro Quartil
quantile(notas,probs = .50, na.rm = T) #Percentil 50 ou Segundo Quartil
quantile(notas,probs = .75, na.rm = T) #Percentil 75 ou Terceiro Quartil

cv <- sd(notas, na.rm = T)/ mean(notas, na.rm = T) * 100

cut(dados, breaks = c(-Inf,15,30,Inf), 
           labels = c("Baixa Dispersão","Média Dispersão","Alta Dispersão"))

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

sessionInfo()

unique(select(dados, precipitacao_mm))

sprintf('De %s até %s mm', min(dados$precipitacao_mm), max(dados$precipitacao_mm))

dados2 <- read.csv("C:/Users/Flávia Cristina/Documents/pos/estatistica/Dados Metereologicos/estatistica_descritiva/dados_pnad_2015.csv")
















