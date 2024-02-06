rm(list = ls())

##### Biblbiotecas #################################################################################################################################################################################

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
library(DescTools)

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

#renda média por sexo e raça
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

medias <- tapply(dados$renda, list(dados$sexo, dados$cor), mean)  #renda média por sexo e cor.
rownames(medias) <- c('Masculino', 'Feminino')
colnames(medias) <- c('Indígena', 'Branca', 'Preta', 'Amarela', 'Parda')
medias

##### Variaveis Quantitativas - Classes personalizadas ##################################################################################################################################

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

##### Variaveis Quantitativas - Classes de amplitude fixa  ##################################################################################################################################

#Regra de Sturges

n <- nrow(dados)
k <- 1 + (10 / 3) * log10(n)
k <- round(k)

labels <- c(
  '      0.00 |—|  11,764.70', 
  ' 11,764.70  —|  23,529.40', 
  ' 23,529.40  —|  35,294.10', 
  ' 35,294.10  —|  47,058.80', 
  ' 47,058.80  —|  58,823.50', 
  ' 58,823.50  —|  70,588.20', 
  ' 70,588.20  —|  82,352.90', 
  ' 82,352.90  —|  94,117.60', 
  ' 94,117.60  —| 105,882.00', 
  '105,882.00  —| 117,647.00', 
  '117,647.00  —| 129,412.00', 
  '129,412.00  —| 141,176.00', 
  '141,176.00  —| 152,941.00', 
  '152,941.00  —| 164,706.00', 
  '164,706.00  —| 176,471.00', 
  '176,471.00  —| 188,235.00', 
  '188,235.00  —| 200,000.00'
)

frequencia <- table(
  cut(
    x <- dados$renda,
    breaks <- k,
    labels <- labels,
    include.lowest <- TRUE
  )
)

percentual <- prop.table(frequencia) * 100

dist_freq_quantitativas_amplitude_fixa <- cbind('Frequência' = frequencia, 'Porcentagem (%)' = percentual)
dist_freq_quantitativas_amplitude_fixa

##### Histograma  ##################################################################################################################################

# O HISTOGRAMA é a representação gráfica de uma distribuição de frequências. 
# É um gráfico formado por um conjunto de retângulos colocados lado a lado, 
# onde a área de cada retângulo é proporcional à frequência da classe que ele representa.

options(repr.plot.width = 7, repr.plot.height = 4)

hist(dados$altura)

hist(
  x = dados$altura,
  breaks = 'Sturges',
  col = 'lightblue',
  main = 'Histograma das alturas',
  xlab = 'altura',
  ylab = 'Frequências',
  prob = TRUE,
  las = 1
)

ggplot(dados, aes(x = altura)) + 
  geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) + 
  ylab("Frequência") + 
  xlab("alturas") + 
  ggtitle('Histograma das alturas') +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, vjust = +0.2),
    axis.title.x = element_text(size = 12, vjust = -0.2),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )

formatos <- theme(
  plot.title = element_text(size = 14, hjust = 0.5),
  axis.title.y = element_text(size = 12, vjust = +0.2),
  axis.title.x = element_text(size = 12, vjust = -0.2),
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10)
)

ggplot(dados, aes(x = altura, y = ..density..)) + 
  geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) + 
  geom_density(color = 'green', size = 1.5) +
  ylab("Frequência") + 
  xlab("alturas") + 
  ggtitle('Histograma das alturas') +
  formatos

bar_chart <- data.frame(dist_freq_quantitativas_personalizadas)
bar_chart

ggplot(bar_chart, aes(x = row.names(bar_chart), y = bar_chart$Frequência)) + 
  geom_bar(stat = "identity") + 
  ylab("Frequência") + 
  xlab("Classes de renda") + 
  ggtitle('Gráfico Classes de renda') +
  formatos

##### Medidas de Tendência Central ###############################################################################################################################################################################

# DataFrame de exemplo

materias <- c('Matemática', 'Português', 'Inglês', 'Geografia', 'História', 'Física', 'Química')
Fulano <- c(8, 10, 4, 8, 6, 10, 8)
Beltrano <- c(10, 2, 0.5, 1, 3, 9.5, 10)
Sicrano <- c(7.5, 8, 7, 8, 8, 8.5, 7)

df <- data.frame(Fulano, Beltrano, Sicrano, row.names = materias)
df

##### Média Aritmética ###############################################################################################################################################################################

mean(df$Fulano)

mean(dados$renda)

aggregate(list(renda = dados$renda), list(Sexo = dados$sexo), mean)

##### Mediana ###############################################################################################################################################################################

# Exemplo 1 - n ímpar

df_fulano <- df[order(df$Fulano),]
df_fulano

n = nrow(df_fulano)
n

elemento_md <- (n + 1) / 2
elemento_md

df_fulano[elemento_md, ]

median(df$Fulano)

# Exemplo 2 - n par

set.seed(101)
sample(nrow(df), 6)

df_beltrano <- df[sample(nrow(df), 6), ]
df_beltrano

df_beltrano <- df_beltrano[order(df_beltrano$Beltrano), ]
df_beltrano

elemento_md = n / 2
elemento_md

mean(df_beltrano[c(elemento_md, elemento_md + 1), ]$Beltrano)

median(df_beltrano$Beltrano)

##### Moda ###############################################################################################################################################################################

exemplo_moda <- c(1,2,2,3,4,4,5,6,7,7)
exemplo_moda

freq <- table(exemplo_moda)
freq

freq[freq == max(freq)]

names(freq)[freq == max(freq)]

Moda <- function(x){
     freq <- table(x)
     return(names(freq)[freq == max(freq)])
  
}

Moda(exemplo_moda)

Moda(df$Fulano)

##### Relação entre média, mediana e moda ###############################################################################################################################################################################

# Avaliando a variável renda

ggplot(dados[dados$renda < 20000, ], aes(x = renda, y = ..density..)) + 
  geom_histogram(binwidth = 500) + 
  geom_density(color = 'green')

moda <- as.numeric(Moda(dados$renda))
moda

mediana <- median(dados$renda)
mediana

media <- mean(dados$renda)
media

# Avaliando a variável altura

ggplot(dados, aes(x = altura, y = ..density..)) + 
  geom_histogram() + 
  geom_density(color = 'green')

moda <- as.numeric(Moda(dados$altura))
moda

mediana <- median(dados$altura)
mediana

media <- mean(dados$altura)
media

# Avaliando a variável ANOS DE ESTUDO

ggplot(dados, aes(x = anos_de_estudo, y = ..density..)) + 
  geom_histogram() + 
  geom_density(color = 'green')

moda <- as.numeric(Moda(dados$anos_de_estudo))
moda

mediana <- median(dados$anos_de_estudo)
mediana

media = mean(dados$anos_de_estudo)
media

##### MEDIDAS SEPARATRIZES ###############################################################################################################################################################################
##### Quartis, decis e percentis ###############################################################################################################################################################################

quantile(dados$renda, c(0.25, 0.50, 0.75))

decis <- c()
for(i in 1:9){
  decis <- c(decis, i / 10)
}
decis

centis <- c()
for(i in 1:99){
  centis <- c(centis, i / 100)
}

centis

quantile(dados$renda, centis)

ggplot(data = dados, aes(x = idade)) + 
  geom_histogram (aes(y = cumsum(..count..)/sum(..count..), 
        bins = 10) )+ 
  geom_freqpoly(aes(y = cumsum(..count..)/sum(..count..)), 
        color = 'green')
    
    decis <- c()
    for(i in 1:9){
      decis <- c(decis, i / 10)
    }
    quantile(dados$idade, decis)

# Classificação percentual
length(dados$idade[dados$idade <= 40]) / length(dados$idade) * 100

##### Box-plot ###############################################################################################################################################################################

length(dados$renda[dados$renda <= 788/2 ]) / length(dados$renda) * 100 

sexo = c(
  'Masculino', 
  'Feminino'
)
cor = c(
  'Indígena', 
  'Branca', 
  'Preta', 
  'Amarela', 
  'Parda'
)
anos_de_estudo = c(
  'Sem instrução e menos de 1 ano', 
  '1 ano', 
  '2 anos', 
  '3 anos', 
  '4 anos', 
  '5 anos', 
  '6 anos', 
  '7 anos', 
  '8 anos', 
  '9 anos', 
  '10 anos', 
  '11 anos', 
  '12 anos', 
  '13 anos', 
  '14 anos', 
  '15 anos ou mais', 
  'Não determinados'
)


ggplot(data = dados, aes(x = "", y = altura)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("Metros") + 
  xlab("") + 
  ggtitle('Box-plot Alturas') +
  formatos

ggplot(data = dados, aes(x = sexo, y = altura, group = sexo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Metros") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Alturas X Sexo') +
  formatos

dados$cat.sexo <- factor(dados$sexo)
levels(dados$cat.sexo) <- sexo

ggplot(data = dados, aes(x = cat.sexo, y = altura)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Metros") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Alturas X Sexo') +
  formatos

ggplot(data = dados[dados$renda < 10000, ], aes(x = "", y = renda)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("R$") + 
  xlab("") + 
  ggtitle('Box-plot Renda') +
  formatos

ggplot(data = dados[dados$renda < 10000, ], aes(x = cat.sexo, y = renda)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("R$") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Renda X Sexo') +
  formatos

dados$cat_anos_de_estudo <- factor(dados$anos_de_estudo, order = TRUE)
levels(dados$cat_anos_de_estudo) <- anos_de_estudo

ggplot(data = dados, aes(x = "", y = anos_de_estudo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("Anos") + 
  xlab("") + 
  ggtitle('Box-plot Anos de Estudo') +
  formatos

ggplot(data = dados, aes(x = cat.sexo, y = anos_de_estudo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Anos") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Anos de Estudo X Sexo') +
  formatos

dados$cat_uf <- factor(dados$uf) 

ggplot( 
  data = dados[(dados$uf == 29 | dados$uf == 35) & dados$renda < 10000, ],  
  aes(y = renda, x = cat_uf) 
) +  
  stat_boxplot(geom ='errorbar', width = 0.4) +  
  geom_boxplot(fill = c('#3274A1', "orange")) +  
  coord_flip() + 
  ylab("R$") +  
  xlab("UF") +  
  ggtitle('Renda (R$) - Bahia X São Paulo') + 
  theme( 
    plot.title = element_text(size = 14, hjust = 0.5), 
    axis.title.y = element_text(size = 12, vjust = +0.2), 
    axis.title.x = element_text(size = 12, vjust = -0.2), 
    axis.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10) 
  )

dados[(dados$uf == 29 | dados$uf == 35) & dados$renda < 10000, ]

##### Medidas de Dispersão ###############################################################################################################################################################################
##### Desvio médio absoluto ###############################################################################################################################################################################

summary(df)

notas_fulano <- data.frame(Fulano = df$Fulano, row.names = row.names(df))
notas_fulano

nota_media_fulano <- mean(notas_fulano$Fulano)
nota_media_fulano

notas_fulano$Desvio <- notas_fulano$Fulano - nota_media_fulano
notas_fulano

notas_fulano$Desvio.Absoluto <- abs(notas_fulano$Desvio)
notas_fulano

ggplot(data = notas_fulano, aes(x = row.names(notas_fulano), y = Fulano)) + 
  geom_point() + 
  geom_hline(yintercept = mean(notas_fulano$Fulano), color = 'red') + 
  geom_segment(aes(x = 1, y = 10, xend = 1, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 2, y = 8, xend = 2, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 3, y = 6, xend = 3, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 4, y = 4, xend = 4, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 5, y = 8, xend = 5, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 6, y = 10, xend = 6, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 7, y = 8, xend = 7, yend = mean(notas_fulano$Fulano)))

mean(notas_fulano$Desvio.Absoluto)

MeanAD(df$Fulano)

##### Variância ###############################################################################################################################################################################

notas_fulano$Desvio2 <- notas_fulano$Desvio ^ 2
notas_fulano

sum(notas_fulano$Desvio2) / (nrow(notas_fulano) - 1)

variancia <- var(notas_fulano$Fulano)
variancia

##### Desvio padrão ###############################################################################################################################################################################

sqrt(variancia)

desvio_padrao <- sd(notas_fulano$Fulano)
desvio_padrao

Moda(df$Fulano)
Moda(df$Sicrano)

sd(df$Fulano)
sd(df$Sicrano)

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
















