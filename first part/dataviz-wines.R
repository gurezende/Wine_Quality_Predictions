# Dataset de vinhos
# Problema de Negócios: Temos um dataset com milhares de vinhos e o cliente quer respostas
# para algumas perguntas. É necessário enviar as respostas em formato mais visual possível,
# isto é = gráficos!

# Setup do nosso diretório de trabalho
setwd('/Users/santos@us.ibm.com/Documents/FCD/Mentoria')
getwd()

# Pacotes gráficos
library(plotly)
#Pacote de Data Wrangling
library(dplyr)
library(shiny)
library(DMwR)


# Carregando o dataset
df <- read.csv('winequality2.csv')

# Visualizando o dataset
View(df)

# Remover a coluna ID para melhor análise
df2 <- df[,-1]
View(df2)

# Verificando os tipos de dados em cada coluna
str(df2)
# Trazendo os primeiros insights com o sumário estatístico
summary(df2)

# Grava os nomes das colunas em uma variável
colunas <- names(df2)

# Criando um histograma para cada coluna
for (col in seq(1,length(names(df2)),1)) {
  hist(df2[,col], col = 'darkred', main = paste('Histogram of', colunas[col]), xlab = colunas[col])
} 


# QUESTÕES DO CLIENTE:

  # 1. Os melhores vinhos estão em qual faixa de pH?

# -------------------------------------------------------------------------------------
# Vamos tomar por base que os melhores vinhos estão com notas entre 6 e 9.
#Portanto, primeiramente vamos criar um subset com os vinhos nessa faixa
df_best <- df2[df2$quality > 5, c(9,12)]
# Colocar os pHs em bins para facilitar a visualização
min(df_best$pH)
max(df_best$pH)
df_best$pH_group <- cut(df_best$pH, seq(2.7,4.1 , 0.1))
View(df_best)

# Fazer agrupamento dos dados por bins e qualidade
df_best_notas <- as.data.frame(with(df_best, table(df_best$pH_group, df_best$quality)))
colnames(df_best_notas) = c('pH','nota','freq')
View(df_best_notas)
# Separar o dataset por notas
df_best_nota6 <- df_best_notas[df_best_notas$nota==6,]
df_best_nota7 <- df_best_notas[df_best_notas$nota==7,]
df_best_nota8 <- df_best_notas[df_best_notas$nota==8,]
df_best_nota9 <- df_best_notas[df_best_notas$nota==9,]

#Plotando o resultado
fig <- plot_ly(data = df_best_nota6, x = ~pH, y = ~freq, type = 'bar', name='nota6', alpha = 0.5)
fig <- fig %>% add_trace(data = df_best_nota7, y = ~freq, name = 'nota7', alpha = 0.6)
fig <- fig %>% add_trace(data = df_best_nota8, y = ~freq, name = 'nota8', alpha = 0.7)
fig <- fig %>% add_trace(data = df_best_nota9, y = ~freq, name = 'nota9', alpha = 0.9)
fig <- fig %>% layout(title='Notas por faixa de pH' ,barmode = 'overlay')
fig 

# Mostrando uma visão geral das notas por pH com BoxPlot. Box plot é uma boa opção para mostrar variação
boxplot(pH~quality,data=df2, main="Boxplot Nota vs pH", xlab="Nota", ylab="pH", col= 'darkred')

### Portanto, sabendo que o nível perfeito de pH para brancos deve estar entre 3,1 e 3,4. 
# Para tintos, a maioria dos produtores prefere atingir níveis ao redor de 3,3 e 3,6, 
# temos que os vinhos do cliente estão coerentes com o índice, tendo as melhores notas nestas faixas.

#ggplot(data= df, aes(x=quality, y=pH, col = pH)) + geom_jitter() + scale_colour_gradient(low='green', high='red')


# -------------------------------------------------------------------------------------

  # 2. Quanto mais alcoólico, melhor ou pior avaliado foi o vinho?
# -------------------------------------------------------------------------------------
#Primeiramente vamos criar um subset com os vinhos nota 6-9 e teor de alcool
df_alcool <- df2[df2$quality > 5, c(11,12)]
# Fazer agrupamento dos dados por bins
min(df_alcool$alcohol)
max(df_alcool$alcohol)
df_alcool$groups <- cut(df_alcool$alcohol, seq(8.0, 14.2, 1))
View(df_alcool)

df_alcool_notas <- as.data.frame(with(df_alcool, table(df_alcool$groups, df_alcool$quality)))
colnames(df_alcool_notas) = c('alcohol','nota','freq')

# Separar o dataset por notas
df_alcool_nota6 <- df_alcool_notas[df_alcool_notas$nota==6,]
df_alcool_nota7 <- df_alcool_notas[df_alcool_notas$nota==7,]
df_alcool_nota8 <- df_alcool_notas[df_alcool_notas$nota==8,]
df_alcool_nota9 <- df_alcool_notas[df_alcool_notas$nota==9,]

#Plotando o resultado
fig2 <- plot_ly(data = df_alcool_nota6, x = ~alcohol, y = ~freq, type = 'bar', name='nota6', alpha = 0.5)
fig2 <- fig2 %>% add_trace(data = df_alcool_nota7, y = ~freq, name = 'nota7', alpha = 0.7)
fig2 <- fig2 %>% add_trace(data = df_alcool_nota8, y = ~freq, name = 'nota8', alpha = 0.7)
fig2 <- fig2 %>% add_trace(data = df_alcool_nota9, y = ~freq, name = 'nota9', alpha = 0.9)
fig2 <- fig2 %>% layout(title='Notas por grau alcoólico',barmode = 'overlay')
fig2 

# Boxplot para checar a variação por nota
boxplot(alcohol~quality,data=df2, main="Boxplot Nota vs Grau Alcoólico", xlab="Nota", ylab="Alcool", col= 'darkred')
cor.test(df2$alcohol, df2$quality)

### Para a segunda questão, chegamos à conclusão de que as melhores notas foram dadas aos 
# vinhos entre 11 e 13% de vol alcoolico.
# -------------------------------------------------------------------------------------

  # 3. Qual é a melhor faixa de Total sulfur dioxide?
# -------------------------------------------------------------------------------------
#Primeiramente vamos criar um subset com os vinhos nota 6-9 e teor de alcool
df_dioxide <- df2[df2$quality > 5, c(7,12)]
# Fazer agrupamento dos dados por bins
min(df_dioxide$total.sulfur.dioxide)
max(df_dioxide$total.sulfur.dioxide)
table(df_dioxide$total.sulfur.dioxide, df_dioxide$quality)
df_dioxide$groups <- cut(df_dioxide$total.sulfur.dioxide, seq(5, 295, 10))
View(df_dioxide)

df_dioxide_notas <- as.data.frame(with(df_dioxide, table(df_dioxide$groups, df_dioxide$quality)))
colnames(df_dioxide_notas) = c('total_dioxide','nota','freq')

# Separar o dataset por notas
df_dioxide_nota6 <- df_dioxide_notas[df_dioxide_notas$nota==6,]
df_dioxide_nota7 <- df_dioxide_notas[df_dioxide_notas$nota==7,]
df_dioxide_nota8 <- df_dioxide_notas[df_dioxide_notas$nota==8,]
df_dioxide_nota9 <- df_dioxide_notas[df_dioxide_notas$nota==9,]

#Plotando o resultado
fig3 <- plot_ly(data = df_dioxide_nota6, x = ~total_dioxide, y = ~freq, type = 'bar', name='nota6', alpha = 0.6)
fig3 <- fig3 %>% add_trace(data = df_dioxide_nota8, y = ~freq, name = 'nota7', alpha = 0.7)
fig3 <- fig3 %>% add_trace(data = df_dioxide_nota8, y = ~freq, name = 'nota8', alpha = 0.7)
fig3 <- fig3 %>% add_trace(data = df_dioxide_nota9, y = ~freq, name = 'nota9', alpha = 0.9)
fig3 <- fig3 %>% layout(title ='Notas por qtd de Dioxide' ,barmode = 'overlay')
fig3 

#Boxplot
boxplot(total.sulfur.dioxide~quality,data=df2, main="Boxplot Nota vs Grau Alcoólico", xlab="Nota", ylab="Alcool", col= 'darkred')

### Na terceira questão, chegamos à conclusão de que as melhores notas foram dadas aos 
# vinhos com dioxido sulfuroso total entre 95 e 135.
# Na Comunidade Econômica Européia, o limite autorizado para vinhos é de 160 mg/L para vinhos tintos
# e 210 mg/L para vinhos brancos. 
# De fatos, temos apenas 8 vinhos que ultrapassam o limite total de 210 mg/L.
# É interessante observar que há uma parcela de vinhos bem avaliados na faixa próxima de 0.

# -------------------------------------------------------------------------------------
 
 # 4. Existe correlação entre nível de sulfato e pontuação do vinho?
# -------------------------------------------------------------------------------------
#Primeiramente vamos criar um subset com os vinhos nota 7-9 e teor de alcool
df_sulphate <- df2[df2$quality > 5, c(10,12)]
# Fazer agrupamento dos dados por bins
min(df_sulphate$sulphates)
max(df_sulphate$sulphates)
df_sulphate$groups <- cut(df_sulphate$sulphates, seq(0.22, 1.95, 0.2))
View(df_sulphate)

df_sulphate_notas <- as.data.frame(with(df_sulphate, table(df_sulphate$groups, df_sulphate$quality)))
colnames(df_sulphate_notas) = c('sulphates','nota','freq')

# Separar o dataset por notas
df_sulphate_nota6 <- df_sulphate_notas[df_sulphate_notas$nota==6,]
df_sulphate_nota7 <- df_sulphate_notas[df_sulphate_notas$nota==7,]
df_sulphate_nota8 <- df_sulphate_notas[df_sulphate_notas$nota==8,]
df_sulphate_nota9 <- df_sulphate_notas[df_sulphate_notas$nota==9,]

#Plotando o resultado
fig4 <- plot_ly(data = df_sulphate_nota6, x = ~sulphates, y = ~freq, type = 'bar', name='nota6', alpha = 0.6)
fig4 <- fig4 %>% add_trace(data = df_sulphate_nota7, y = ~freq, name = 'nota7', alpha = 0.7)
fig4 <- fig4 %>% add_trace(data = df_sulphate_nota8, y = ~freq, name = 'nota8', alpha = 0.7)
fig4 <- fig4 %>% add_trace(data = df_sulphate_nota9, y = ~freq, name = 'nota9', alpha = 0.9)
fig4 <- fig4 %>% layout(title ='Notas por qtd de Sulphate' ,barmode = 'overlay')
fig4 

### Na quarta questão, chegamos à conclusão de que as melhores notas foram dadas aos 
# vinhos com sulphates entre 0.42 e 0.62.
# Consideramos quanto menos sulfitos no vinho, melhor, uma vez que pesquisas indicam a busca por
# remove-los no futuro.

#BoxPlot
boxplot(sulphates~quality,data=df2, main="Boxplot Nota vs Sulphates", xlab="Nota", ylab="Alcool", col= 'darkred')


# 5. Quais são as correlações mais fortes entre as variáveis e a qualidade?
# -------------------------------------------------------------------------------------

# Vamos buscar a correlação entre as variáveis e colocar o resultado em uma variável.
correlacao <- cor(df2)

#Plotando o resultado
library(corrplot)
corrplot(correlacao, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot.mixed(correlacao, lower.col = "black")

# Agora que encontramos que as maiores relações com quality (nota) são Álcool, volatile.acidity e density
# vamos plotar mais alguns gráficos antes de prosseguirmos com a clusterização

# Quality vs Density
boxplot(density~quality,data=df2, main="Boxplot Nota vs Density", xlab="Nota", ylab="Alcool", col= 'darkred')

# Quality vs volatile.acidity
boxplot(volatile.acidity~quality,data=df2, main="Boxplot Nota vs volatile.acidity", xlab="Nota", ylab="Alcool", col= 'darkred')
# Acidez volátil: Componente presente no vinho que, em dose elevada, origina o aroma a vinagre.
# Em excesso é o resultado da falta de cuidados durante a vinificação.
# Aqui, faz bastante sentido, pois vemos que é negativamente correlato à nota.
#---------------------

# Agora vamos trabalhar com o KMEans e separar o dataset em vinhos brancos e tintos.
#------------------------------------------------------
# Problema: Nosso dataset contem vinhos brancos e tintos. Entretanto, o cliente não sabe quais são de 
#que tipo. Vamos usar o algoritmo KMeans para separar os dados em dois clusters.

# Em pesquisas no mercado de vinho, sabe-se que : "A maceração consiste fundamentalmente em manter
# a uva refrigerada, uma vez que é encubada em depósitos de fermentação ou dentro das prensas,
# onde pode permanecer algumas horas, no caso de vinhos brancos, até semanas, em alguns tintos. "

# Logo, sabemos que, para ser vinho branco, a uva não pode passar de algumas horas na maceração.
# Isto nos leva a usar esta coluna (maceration) como chave para a clusterização.

# Verificando NAs
sum(is.na(df2))

# Criando uma cópia do dataset com apenas a coluna desejada.
# Como tenho apenas uma coluna, não há necessidade de padronizar os dados
#visto que já estão na mesma escala

# Versão 1
#-----
df_tocluster <- df2[,c(13)]
# Calculando estatística Hopkins: calcula as distâncias entre pontos e atribui pesos.
# Calcula o quanto o DF é clusterizável. > 0.5 = No. | < 0.5 = Yes
library('clustertend')
hopkins(df_tocluster, n = length(df_tocluster)-1)

# Calcular o número ideal de cluster
library('NbClust')
num_clust <- NbClust(df_tocluster, distance = 'euclidean',
                     min.nc = 2, max.nc = 5,
                     method = 'kmean',
                     index = 'silhouette')
# Best
num_clust$Best.nc

# Vemos que o computador nos indica criar 4 clusters, porém, como o cliente nos pediu a divisão entre 
# tintos e brancos, logo vamos usar 2.

# KMeans
modelo <- kmeans(df_tocluster, 2)
print(modelo)

df$cluster <- modelo$cluster
View(df)

# Trazendo o dataset com os clusters
key_df <- read.csv('dados/winequality-types.csv')
key_df <- key_df[,c(1,15)]
key_df$cluster <- ifelse(key_df$Type == 'White',1,2)

# Checando a acurácia do modelo
df$check <- ifelse(key_df$cluster == df$cluster,1,0)
acuracia <- sum(df$check)/nrow(df)
cat('A acurácia do modelo foi: ', acuracia, '%')
# A acurácia do modelo foi:  0.98 %

#--------

# Versão 2
#-----------
# Incluímos o pH, pois sabemos que são diferentes para vinhos brancos e tintos.
df_tocluster2 <- df2[,c(9,11,13)]
# Calculando estatística Hopkins: calcula as distâncias entre pontos e atribui pesos.
# Calcula o quanto o DF é clusterizável. > 0.5 = No. | < 0.5 = Yes
hopkins(df_tocluster2, n = length(df_tocluster2)-1)
# 0.17

# Calcular o número ideal de cluster
num_clust <- NbClust(df_tocluster2, distance = 'euclidean',
                     min.nc = 2, max.nc = 5,
                     method = 'kmean',
                     index = 'silhouette')
# Best
num_clust$Best.nc

# Vemos que o computador nos indica criar 2 clusters agora

# KMeans
modelo2 <- kmeans(df_tocluster2, 2)
df$cluster <- modelo2$cluster

# Trazendo o dataset com os clusters
key_df2 <- read.csv('dados/winequality-types.csv')
key_df2 <- key_df2[,c(1,15)]
View(key_df2)
key_df2$cluster <- ifelse(key_df2$Type == 'White',1,2)


# Checando a acurácia do modelo
df$check <- ifelse(key_df2$cluster == df$cluster,1,0)
acuracia <- sum(df$check)/nrow(df)
cat('A acurácia do modelo foi: ', acuracia, '%')
#A acurácia do modelo foi:  0.98 %
View(df[df$check==0,])
#-----------

# Reduzindo dimensionalidade
df_pca <- prcomp(df[c(2:14)], center = TRUE, scale = TRUE)
# Plotando nosso modelo
plot(df_pca$x[,1], df_pca$x[,2], col = df$cluster)


# A última parte do desafio é criar previsões de notas para os vinhos brancos.
#------------------------------------------------------
# Problema: Nosso dataset contem vinhos brancos e tintos. Agora que nós já o separamos, vamos
# pegar apenas os vinhos brancos e criar um modelo preditivo para prever notas.

# Vamos criar um modelo de Classificação binomial, considerando as notas de 3-6 como Low
# e notas 7-9 como High.

# Separando apenas os vinhos brancos do dataset.
df_whites <- df[df$cluster==1,]
df_whites <- df_whites[,-c(1,14,15,16)]

# Transformando coluna Quality
df_whites$qualityBin <- ifelse(df_whites$quality >= 7 ,'High','Low')
df_whites$qualityBin <- as.factor(df_whites$qualityBin)
View(df_whites)

# Verificando novamente se há dados missing
sum(is.na(df_whites))

# Por não termos dados missing e nem outras transformações relevantes a serem feitas, vamos normalizar
#os dados para treinar o dataset
# df_whites_norm <- as.data.frame(scale(df_whites[,-c(12, 13)]))

smote_data <- SMOTE(qualityBin ~ ., df_whites, perc.over = 650, k = 5, perc.under = 115)
prop.table(table(smote_data$qualityBin))
View(smote_data)

#Split dos dados em treino e teste
index <- sample(x= nrow(smote_data), size=0.9*nrow(smote_data), replace=F)
df_train <- as.data.frame(smote_data[index,-12])
df_test <- as.data.frame(smote_data[-index,-12])

View(df_train)


# Classificação
library('caret')
# Transformar a coluna quality em fator, para classificação
#df_train$qualityBin <- as.factor(df_train$qualityBin)


TrainingParameters <- trainControl(method = "repeatedcv", number = 5, repeats=5)

model_rf <- train(qualityBin~alcohol+volatile.acidity+density+chlorides+pH,data = df_train,
                  method='rf', metric='Accuracy',
                  trControl = TrainingParameters)
print(model_rf) # 95%


model_ada <- train(Class~alcohol+volatile.acidity+density+chlorides+pH,data = df_train,
                    method= 'adaboost')

print(model_ada) #85%


y_test <- as.factor(df_test[,12])
X_test <- df_test[,-12]

predictions <-predict(model_rf, X_test)
confusionMatrix(predictions, y_test) #96% accuracy ok


# Saving the model to be used on the Shiny App
saveRDS(model_rf, "model_rf_wines.rds")
