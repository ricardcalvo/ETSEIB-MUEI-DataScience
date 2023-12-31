#1 Loading Libraries -------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(leaps) #needed for the best subset modeling
library(MASS) # lda, qda functions
library(ROCR) #to compute the AUC
library(stargazer)
library(dplyr)

#2 Loading and Cleaning Data ------------------------------------------------

#carregar base de dades
df <- read.csv2(file = 'data/wisconsin.csv', dec=',')
str(df)

#Canviar nom columna resposta!
colnames(df)[which(names(df) == "diagnosis")] <- "resposta"

summary(df) #minim, mitjana, mediana .....

table(df$resposta)


# Fem pairs nomes de les columnes que volem i separem per classes


pairs(df, col = df$resposta + 1, pch = 16,)

stars(df[,-c(1,2)], col.stars = df$resposta+1)

#3. Five folds  (o n folds)-----------------------------------------------------
set.seed(1964) #Llavor
nfolds=5
nObs <- nrow(df) #Numero observacions
folds <- sample(nfolds, size = nObs, replace = T)
df$fold <- folds

table(df$fold, df$resposta)

#3.1 Five folds (accurate)  (o n folds)-----------------------------------------
benigno<-filter(df, resposta==0)
maligno<-filter(df, resposta==1)

set.seed(1963)

# assign a fold to benign cases
tamanypaquet=trunc(nrow(benigno)/nfolds)
tamanypaquetultim=(nrow(benigno))-(nfolds-1)*tamanypaquet
nB = c(rep(1:(nfolds-1),each=tamanypaquet),rep(nfolds,tamanypaquetultim))
nB = sample(nB)
benigno$fold = nB

# assign a fold to malign cases

tamanypaquet2=trunc(nrow(maligno)/nfolds)
tamanypaquetultim2=(nrow(maligno))-(nfolds-1)*tamanypaquet2
nM = c(rep(1:(nfolds-1),each=tamanypaquet2),rep(nfolds,tamanypaquetultim2))
nM = sample(nM)
maligno$fold = nM

# rejoin data
df <- rbind(benigno,maligno)
table(df$fold, df$resposta)







#4.Regressio Logistica------------------------------------------------
# a warning message appears as some classification are pure (100% true or false). No need to worry

for(fold in 1:nfolds) {
  
  modelLog <- glm(resposta ~ radius + texture + area + smoothness + concavepoints,
                  family = 'binomial', data = df[df$fold != fold,])
  
  predLog <- predict(modelLog, newdata = df[df$fold == fold,], type = 'response')
  
  df$logisticPred[df$fold == fold] <- predLog
  
}

#definim threshold (threshold 0 -> tothom ?s 1. Threshold 1-> tothom ?s 0)
th=0.5

table(df$resposta, df$logisticPred>th, dnn = c('actual','predicted'))

summary(modelLog)  #per veure devian?a



# 5. Lineal Discriminant analysis -----------------------------------------



for(fold in 1:nfolds) {
  
  modLda = lda(resposta ~ .,
               data = df[df$fold != fold, 2:12])
  
  predLda = predict(modLda,
                    newdata = df[df$fold == fold, ])
  
  df$ldaPred[df$fold == fold] <- predLda$posterior[,2]
  
}

table(df$resposta, df$ldaPred>th, dnn = c('actual','predicted'))


# 6. Quadratic Discriminant Analysis --------------------------------------


for(fold in 1:nfolds) {
  
  modQda = qda(resposta ~ .,
               data = df[df$fold != fold, 2:12])
  
  predQda = predict(modQda,
                    newdata = df[df$fold == fold, ])
  
  df$qdaPred[df$fold == fold] <- predQda$posterior[,2]
  
}

table(df$resposta, df$qdaPred > th, dnn = c('actual','predicted'))

#7 Comparar els 3 MODELS -----------------------------------------
#7.1 Matrius de confusió
#7.1.1 Matriu confusio regressioLog

taulaLog<-table(df$resposta, df$logisticPred>th, dnn = c('actual','predicted'))
print(taulaLog)

#Error rate es fa servir quan els falsos negatius i falsos positius tenen costos similars
ErrorRate= (taulaLog[2]+taulaLog[3])/nObs
ErrorRate

#Accuracy (igual q error rate pero mires el encert) es fa servir quan els falsos negatius i falsos positius tenen costos similars
Accuracy=1-ErrorRate
Accuracy

#Sensitivitat es fa servir quan no acceptes falsos negatius. Ex: cancer o Covid 
Sensitivity=taulaLog[4]/(taulaLog[2]+taulaLog[4])
Sensitivity

#Specificity quan no vols donar falses alarmes 
Specicity = taulaLog[1]/(taulaLog[1]+taulaLog[3])
Specicity

#Precission quan vols minimitzar falsos positius (Ex:spam)
Precission= taulaLog[4]/(taulaLog[3]+taulaLog[4])
Precission

#7.1.2 Matriu confusio LDA
taulaLDA<-table(df$resposta, df$ldaPred>th, dnn = c('actual','predicted'))
print(taulaLDA)

#Error rate es fa servir quan els falsos negatius i falsos positius tenen costos similars
ErrorRate= (taulaLDA[2]+taulaLDA[3])/nObs
ErrorRate

#Accuracy (igual q error rate pero mires el encert) es fa servir quan els falsos negatius i falsos positius tenen costos similars
Accuracy=1-ErrorRate
Accuracy

#Sensitivitat es fa servir quan no acceptes falsos negatius. Ex: cancer o Covid 
Sensitivity=taulaLDA[4]/(taulaLDA[2]+taulaLDA[4])
Sensitivity

#Specificity quan no vols donar falses alarmes 
Specicity = taulaLDA[1]/(taulaLDA[1]+taulaLDA[3])
Specicity

#Precission quan vols minimitzar falsos positius (Ex:spam)
Precission= taulaLDA[4]/(taulaLDA[3]+taulaLDA[4])
Precission

#7.1.3 Matriu confusio QDA
taulaQDA<-table(df$resposta, df$qdaPred > th, dnn = c('actual','predicted'))

#Error rate es fa servir quan els falsos negatius i falsos positius tenen costos similars
ErrorRate= (taulaQDA[2]+taulQLDA[3])/nObs
ErrorRate

#Accuracy (igual q error rate pero mires el encert) es fa servir quan els falsos negatius i falsos positius tenen costos similars
Accuracy=1-ErrorRate
Accuracy

#Sensitivitat es fa servir quan no acceptes falsos negatius. Ex: cancer o Covid 
Sensitivity=taulaQDA[4]/(taulaQDA[2]+taulaQDA[4])
Sensitivity

#Specificity quan no vols donar falses alarmes 
Specicity = taulaQDA[1]/(taulaQDA[1]+taulaQDA[3])
Specicity

#Precission quan vols minimitzar falsos positius (Ex:spam)
Precission= taulaQDA[4]/(taulaQDA[3]+taulaQDA[4])
Precission


#8 AUC -----------------------------------------------------
#AUC calcula l'area sota la corba ROC. Quan m?s alt AUC millor ?s el model

predROClog <- prediction(predictions = df[,'logisticPred'],labels = df$resposta)
predROClda <- prediction(predictions = df[,'ldaPred'],labels = df$resposta)
predROCqda <- prediction(predictions = df[,'qdaPred'],labels = df$resposta)

perfROClog <- performance(prediction.obj = predROClog, measure = 'auc')
perfROClda <- performance(prediction.obj = predROClda, measure = 'auc')
perfROCqda <- performance(prediction.obj = predROCqda, measure = 'auc')

perfROClog@y.values[[1]] 

perfROClda@y.values[[1]]

perfROCqda@y.values[[1]]

#ROC curves

ROC <- list(performance(prediction.obj = predROClog, measure = 'tpr',x.measure = 'fpr'),
            performance(prediction.obj = predROClda, measure = 'tpr',x.measure = 'fpr'),
            performance(prediction.obj = predROCqda, measure = 'tpr',x.measure = 'fpr'))

ggplot(data = NULL,aes(x=x,y=y)) +
  geom_step(data = data.frame(x = ROC[[2]]@x.values[[1]],
                              y = ROC[[2]]@y.values[[1]]), aes(col='LDA'), direction = 'vh', size = 1) + 
  geom_step(data = data.frame(x = ROC[[3]]@x.values[[1]],
                              y = ROC[[3]]@y.values[[1]]), aes(col='QDA'), direction = 'vh', size = 1) +
  geom_step(data = data.frame(x = ROC[[1]]@x.values[[1]],
                              y = ROC[[1]]@y.values[[1]]), aes(col='Logistic'), direction = 'vh', size = 1) +
  labs(x='1-specificity (False Positive Rate: FPR)',y='sensitivity (True Positive Rate (TPR)') +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) 


#Predir un cas en concret 

predict(modelLog3, newdata = data.frame(gre=70, gpa=3), type = 'response')
