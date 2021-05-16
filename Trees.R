#Remove objetos da memória do R
rm(list=ls(all=TRUE))

#Instala bibliotecas
install.packages('mlbench') #biblioteca com conjunto de dados Pima Indians Diabetes
install.packages('caret',dependencies = TRUE) #biblioteca para trabalhar com machine learning
install.packages('rpart') #rpart traz o algoritmo decision tree
install.packages('rpart.plot') #visualizar a arvore de decisao

#Carrega as bibliotecas
library(mlbench)
library(caret)
library(rpart)
library(rpart.plot)

#Carrega o conjunto de dados PimaIndiansDiabetes na memoria do R
data(PimaIndiansDiabetes)

#Armazena o conjunto de dados PimaIndiansDiabetes em um data frame
dataframe <- PimaIndiansDiabetes

#visualiza as estatisticas descritivas das variaveis do conjunto de dados
summary(dataframe)

#Separa conjunto de dados para treino e teste no método de hold-out
conjunto <- createDataPartition(dataframe$diabetes, #Variavel resposta do conjunto de dados
                                p = 0.8, #Definir percentual para treino em 80%
                                list = F #Manter lista
)

#Separa as bases de teste e treino para o modelo preditivo
base_treino <- dataframe[conjunto,]
base_teste <- dataframe[-conjunto,]

#Planta a semente
set.seed(1)

#Treinar algoritmo de árvore de decisao com profundidade 25
arvore <- train(diabetes ~ .,
                data = base_treino,
                method = "rpart",
                control = rpart.control(
                  minsplit = 5, 
                  minbucket = 5,
                  maxdepth =25)) 

#Visualiza resumo do treinamento do modelo
arvore

#Visualiza arvore de decisao
rpart.plot(arvore$finalModel , cex = 0.9)

#Realizar predicao nos dados separados para teste
predicoes <- predict(arvore, newdata = base_teste)

#Visualiza matriz de confusao
confusionMatrix(predicoes,
                base_teste$diabetes,
                positive = 'pos')
