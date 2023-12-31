# Treball del Curs Data Science MUEI
# Students: Jan Álvarez Alonso, Ricard Calvo, Steven Chen
# 09-01-2023

# 1. Introducció ---------------------------------------------------------------

# Loading Libraries

library(ggplot2)
library(tidyverse)
library(leaps) # needed for the best subset modeling
library(MASS) # lda, qda functions
library(ROCR) #to compute the AUC
library(stargazer)
library(dplyr)
library(lubridate)
library(tree)
library(randomForest) # bagging and random forest
library(gbm) # boosting
library(caTools)
library(car)
library(pROC)
require(pROC)

# Loading Data

dforiginal <- read.csv2(file = 'Accidents_de_tr_nsit_amb_morts_o_ferits_greus_a_Catalunya.csv', sep=',')
accidents <- read.csv2(file = 'Accidents_de_tr_nsit_amb_morts_o_ferits_greus_a_Catalunya.csv', sep=',')

#Canviem els "sense especificar" per NA
accidents[accidents == "Sense Especificar"] <- NA
accidents[accidents == "Sense especificar"] <- NA

# 2. Afegir/eliminar/nateja de dades -------------------------------------------


# Add column of accident Score:  9 x F_MORTS + 3 x F_GREUS + 1 x F_LLEUS

accidents$accident_Score <- 9*accidents$F_MORTS+3*accidents$F_FERITS_GREUS+accidents$F_FERITS_LLEUS

# Add column "ES_GREU". Aquesta serà la variable resposta a predir
accidents$ES_GREU <- ifelse(accidents$accident_Score >= 9, 1, 0)

table(accidents$ES_GREU)

# Add column of month. A partir de la data creem una columna dels mesos

accidents$month <- month(accidents$dat)
accidents$month <- as.character(accidents$month)

# Add column of day time slot. 
# A partir de la hora creem columna de les hores com a categories

accidents$hor <- as.character(as.integer(accidents$hor))

# Eliminem la columna "grupHor" ja que està correlacionada amb "hor"

accidents<-subset(accidents,select = -c(grupHor))

# Add column that defines if it's a high-traffic hour or not

accidents$hora_punta <- as.factor(ifelse(
  # high-traffic hours are from 7:30 to 9:30 and 18:00 to 20:00 during work days
  (accidents$grupDiaLab %in% c("Feiner")) & 
    ((accidents$hor >= "7.30" & accidents$hor <= "9.30") | 
       (accidents$hor >= "18.00" & accidents$hor <= "20.00")), 
  1,  # high-traffic hour
  # high-traffic hours are from 10:00 to 12:00 and 18:00 to 21:00 on weekends
  ifelse(
    (accidents$grupDiaLab %in% c("CapDeSetmana", "Festiu")) & 
      ((accidents$hor >= "10.00" & accidents$hor <= "12.00") | 
         (accidents$hor >= "18:00" & accidents$hor <= "21:00")), 
    1,  # high-traffic hour
    0  # not a high-traffic hour
  )
))

# Delete of columns
# S'eliminen columnes que considrem inneceçàries per l'anàlisi
accidents<-subset(accidents,select = -c(Any,dat, pk, F_MORTS, F_FERITS_GREUS,
                                        F_FERITS_LLEUS, F_VICTIMES, D_GRAVETAT,
                                        D_SUBTIPUS_ACCIDENT, accident_Score, 
                                        F_UNIT_DESC_IMPLICADES)) 


# A partir d'aquí mirarem les seguents columnes:
# via, nomMun,
# D_CARRIL_ESPECIAL,D_CLIMATOLOGIA,
# D_FUNC_ESP_VIA,D_INFLUIT_SOLCS_RASES,
# D_INTER_SECCIO,D_SUPERFICIE,nomCom,
# D_ACC_AMB_FUGA,D_CIRCULACIO_MESURES_ESP,
# D_INFLUIT_CIRCULACIO,D_INFLUIT_ESTAT_CLIMA,
# D_INFLUIT_LLUMINOSITAT, D_INFLUIT_MESU_ESP,
# D_INFLUIT_OBJ_CALCADA,
# D_LLUMINOSITAT,
# D_REGULACIO_PRIORITAT, D_SENTITS_VIA,
# D_SUBTIPUS_TRAM, D_VENT
# Aquestes columnes tenen el problema de contenir moltes subcategories.


## Eliminem columna "via" perq te 737 categories
length(levels(as.factor(accidents$via)))
accidents<-subset(accidents,select = -c(via))

## Eliminem columna "nomMun" perq te 877 categories
length(levels(as.factor(accidents$nomMun)))
accidents<-subset(accidents,select = -c(nomMun))

## Eliminem columna "nomCom" perq te 42 categories
table(accidents$nomCom, useNA = "always")
length(levels(as.factor(accidents$nomCom)))
accidents<-subset(accidents,select = -c(nomCom))


## F_UNITATS_IMPLICADES
# Eliminem la columna F_UNITATS_IMPLICADES, perquè és la suma de
# les següents columnes

accidents<-subset(accidents,select = -c(F_UNITATS_IMPLICADES))


## D_CARRIL_ESPECIAL
table(accidents$D_CARRIL_ESPECIAL, useNA = "always")
# Add column of D_CARRIL_ESPECIAL2 (1 si n'hi ha , 0 si no hi ha)
accidents$D_CARRIL_ESPECIAL2 <- as.factor(ifelse(accidents$D_CARRIL_ESPECIAL == "No n'hi ha", 0, 1))

# Eliminem files NA de D_CARRIL_ESPECIAL2
accidents <-accidents %>% drop_na(D_CARRIL_ESPECIAL2)

# Eliminem la columna original D_CARRIL_ESPECIAL
accidents<-subset(accidents,select = -c(D_CARRIL_ESPECIAL))


## D_CLIMATOLOGIA
table(accidents$D_CLIMATOLOGIA, useNA = "always")

# Eliminem files NA
accidents <-accidents %>% drop_na(D_CLIMATOLOGIA)

# Canviem les categories a Bon temps==0 mal temps ==1
accidents$D_CLIMATOLOGIA2 <- as.factor(ifelse(accidents$D_CLIMATOLOGIA == "Bon temps",
                                    0, 1))

# Eliminem la columna original D_CLIMATOLOGIA
accidents<-subset(accidents,select = -c(D_CLIMATOLOGIA))
table(accidents$D_CLIMATOLOGIA2, useNA = "always")


## D_FUNC_ESP_VIA
table(accidents$D_FUNC_ESP_VIA, useNA = "always")

# Eliminem files NA
accidents <-accidents %>% drop_na(D_FUNC_ESP_VIA)

# Canviem les categories a Sense funció especial==0,  la resta==1
accidents$D_FUNC_ESP_VIA2 <- as.factor(ifelse(accidents$D_FUNC_ESP_VIA == "Sense funció especial",
                                    0, 1))
# Eliminem la columna original D_FUNC_ESP_VIA
accidents<-subset(accidents,select = -c(D_FUNC_ESP_VIA))
table(accidents$D_FUNC_ESP_VIA2, useNA = "always")


## D_INFLUIT_SOLCS_RASES
table(accidents$D_INFLUIT_SOLCS_RASES, useNA = "always")

# Eliminem files NA
accidents <-accidents %>% drop_na(D_INFLUIT_SOLCS_RASES)


## D_INTER_SECCIO
table(accidents$D_INTER_SECCIO, useNA = "always")


## D_SUPERFICIE
table(accidents$D_SUPERFICIE, useNA = "always")

# Canviem les categories a Sec i net==0,  la resta==1
accidents$D_SUPERFICIE2 <- as.factor(ifelse(accidents$D_SUPERFICIE == "Sec i net",
                                    0, 1))

# Eliminem la columna original D_SUPERFICIE
accidents<-subset(accidents,select = -c(D_SUPERFICIE))
table(accidents$D_SUPERFICIE2, useNA = "always")


## D_ACC_AMB_FUGA
table(accidents$D_ACC_AMB_FUGA, useNA = "always")

# Eliminem files NA
accidents <-accidents %>% drop_na(D_ACC_AMB_FUGA)


## D_CIRCULACIO_MESURES_ESP
table(accidents$D_CIRCULACIO_MESURES_ESP, useNA = "always")

# Eliminem files NA
accidents <-accidents %>% drop_na(D_CIRCULACIO_MESURES_ESP)

# Canviem les categories a No n'hi ha==0,  la resta==1
accidents$D_CIRCULACIO_MESURES_ESP2 <- as.factor(ifelse(accidents$D_CIRCULACIO_MESURES_ESP == "No n'hi ha",
                                  0, 1))

# Eliminem la columna original D_CIRCULACIO_MESURES_ESP
accidents<-subset(accidents,select = -c(D_CIRCULACIO_MESURES_ESP))
table(accidents$D_CIRCULACIO_MESURES_ESP2, useNA = "always")


## D_INFLUIT_CIRCULACIO
table(accidents$D_INFLUIT_CIRCULACIO, useNA = "always")


## D_INFLUIT_ESTAT_CLIMA
table(accidents$D_INFLUIT_ESTAT_CLIMA, useNA = "always")


## D_INFLUIT_LLUMINOSITAT
table(accidents$D_INFLUIT_LLUMINOSITAT, useNA = "always")


## D_INFLUIT_MESU_ESP
table(accidents$D_INFLUIT_MESU_ESP, useNA = "always")


## D_INFLUIT_OBJ_CALCADA
table(accidents$D_INFLUIT_OBJ_CALCADA, useNA = "always")

# Observem com:
# D_INFLUIT_CIRCULACIO,D_INFLUIT_ESTAT_CLIMA,
# D_INFLUIT_LLUMINOSITAT, D_INFLUIT_MESU_ESP,
# D_INFLUIT_OBJ_CALCADA, 
# Tenen una freqüencia de si molt baixa, decidim eliminar aquestes columnes

accidents<-subset(accidents,select = -c(D_INFLUIT_CIRCULACIO,D_INFLUIT_ESTAT_CLIMA,
                                        D_INFLUIT_LLUMINOSITAT, D_INFLUIT_MESU_ESP,
                                        D_INFLUIT_OBJ_CALCADA))

## D_LLUMINOSITAT
table(accidents$D_LLUMINOSITAT, useNA = "always")

# Hi ha 6 categories
prop.table(table(accidents$D_LLUMINOSITAT,accidents$ES_GREU),1)
prop.table(table(accidents$D_LLUMINOSITAT,accidents$ES_GREU),2)

# Decidim crear una nova columna nomes amb 2 categories. 0 =="De dia, dia clar"
# 1 == la resta

accidents$D_LLUMINOSITAT2 <- as.factor(ifelse(accidents$D_LLUMINOSITAT == "De dia, dia clar",
                                  0, 1))

# Eliminem la columna original D_LLUMINOSITAT
accidents<-subset(accidents,select = -c(D_LLUMINOSITAT))
table(accidents$D_LLUMINOSITAT2, useNA = "always")
prop.table(table(accidents$D_LLUMINOSITAT2,accidents$ES_GREU),1)
prop.table(table(accidents$D_LLUMINOSITAT2,accidents$ES_GREU),2)


## D_REGULACIO_PRIORITAT
table(accidents$D_REGULACIO_PRIORITAT, useNA = "always")

# Moltes NA, eliminem la columna
accidents<-subset(accidents,select = -c(D_REGULACIO_PRIORITAT))


## D_SENTITS_VIA
table(accidents$D_SENTITS_VIA, useNA = "always")

# Moltes NA, eliminem columna
accidents<-subset(accidents,select = -c(D_SENTITS_VIA))


## D_SUBTIPUS_TRAM
table(accidents$D_SUBTIPUS_TRAM, useNA = "always")

# Moltes NA, eliminem columna
accidents<-subset(accidents,select = -c(D_SUBTIPUS_TRAM))


## D_VENT
table(accidents$D_VENT, useNA = "always")

# Decidim crear una nova columna nomes amb 2 categories. 0=="Calma, vent molt suau"
# 1 == la resta
accidents$D_VENT2 <- as.factor(ifelse(accidents$D_VENT == "Calma, vent molt suau",
                                    0, 1))

# Eliminem la columna original D_VENT
accidents<-subset(accidents,select = -c(D_VENT))
table(accidents$D_VENT2, useNA = "always")


## D_ACC_AMB_FUGA
table(accidents$D_ACC_AMB_FUGA, useNA = "always")


## D_BOIRA
table(accidents$D_BOIRA, useNA = "always")


## D_CARACT_ENTORN
table(accidents$D_CARACT_ENTORN, useNA = "always")

# Moltes NA, eliminem columna
accidents<-subset(accidents,select = -c(D_CARACT_ENTORN))


## D_INFLUIT_BOIRA
table(accidents$D_INFLUIT_BOIRA, useNA = "always")

# Moltes NA, eliminem columna
accidents<-subset(accidents,select = -c(D_INFLUIT_BOIRA))


## D_INFLUIT_CARACT_ENTORN
table(accidents$D_INFLUIT_CARACT_ENTORN, useNA = "always")

# Moltes NA (comparats amb "SI"), eliminem columna
accidents<-subset(accidents,select = -c(D_INFLUIT_CARACT_ENTORN))


## D_INFLUIT_INTEN_VENT
table(accidents$D_INFLUIT_INTEN_VENT, useNA = "always")

# Moltes NA (comparats amb "SI"), eliminem columna
accidents<-subset(accidents,select = -c(D_INFLUIT_INTEN_VENT))


## D_INFLUIT_SOLCS_RASES
table(accidents$D_INFLUIT_SOLCS_RASES, useNA = "always")

# Pocs "SI", eliminem columna
accidents<-subset(accidents,select = -c(D_INFLUIT_SOLCS_RASES))


## D_INFLUIT_VISIBILITAT
table(accidents$D_INFLUIT_VISIBILITAT, useNA = "always")

# Moltes NA (comparats amb "SI"), eliminem columna
accidents<-subset(accidents,select = -c(D_INFLUIT_VISIBILITAT))


## D_TITULARITAT_VIA
table(accidents$D_TITULARITAT_VIA, useNA = "always")

# Moltes NA, eliminem columna
accidents<-subset(accidents,select = -c(D_TITULARITAT_VIA))


## D_TRACAT_ALTIMETRIC
table(accidents$D_TRACAT_ALTIMETRIC, useNA = "always")

# Moltes NA, eliminem columna
accidents<-subset(accidents,select = -c(D_TRACAT_ALTIMETRIC))


## C_VELOCITAT_VIA
table(accidents$C_VELOCITAT_VIA, useNA = "always")

# Eliminem files NA
accidents <-accidents %>% drop_na(C_VELOCITAT_VIA)

# Eliminem les files amb valor =999 (deu ser un error)
accidents <- accidents[accidents$C_VELOCITAT_VIA != 999, ]


## Canviem les columnes amb chr datatype a factor datatype

accidents[sapply(accidents, is.character)] <- lapply(accidents[sapply(accidents, is.character)], 
                                                     as.factor)

str(accidents)


# 3. Anàlisi de les dades ------------------------------------------------------

summary(accidents)
head(accidents)

# Mirem si hi ha relació entre les variables numèriques

modelLogSencer <- glm(ES_GREU ~ F_VIANANTS_IMPLICADES 
                      + F_BICICLETES_IMPLICADES + F_CICLOMOTORS_IMPLICADES 
                      + F_MOTOCICLETES_IMPLICADES + F_VEH_LLEUGERS_IMPLICADES
                      +F_VEH_PESANTS_IMPLICADES + F_ALTRES_UNIT_IMPLICADES+
                        C_VELOCITAT_VIA,
                      family = 'binomial', data = accidents)
vif(modelLogSencer)

# Com no hi ha cap VIF>5 totes les variables númeriques son linaement indep


# 4. Stepwise approach per seleccionar variables -------------------------------

# null model

modelNull <- glm(ES_GREU ~ 1, data = accidents)

# full model

modelFull <- glm(ES_GREU ~ .,data = accidents)

# 4.1. Stepwise forward --------------------------------------------------------

modSWFwd <-  step(modelNull,
                  scope=list(lower=modelNull,upper=modelFull),
                  direction='forward',
                  trace=1)

# 4.2. Stepwise backward -------------------------------------------------------

modSWBwd <-  step(modelFull,
                  scope=list(lower=modelNull,upper=modelFull),
                  direction='backward',
                  trace=1)

# 4.3. Stepwise both directions ------------------------------------------------

modSWBoth <-  step(glm(ES_GREU ~ nomDem + F_BICICLETES_IMPLICADES + 
                         F_CICLOMOTORS_IMPLICADES + F_MOTOCICLETES_IMPLICADES + F_VEH_PESANTS_IMPLICADES +
                         C_VELOCITAT_VIA + D_ACC_AMB_FUGA + D_INTER_SECCIO, data = accidents),
                   scope=list(lower=modelNull,upper=modelFull),
                   direction='both',
                   trace=1)

lapply(list(modSWFwd, modSWBwd, modSWBoth), FUN = summary)

# Surten el mateix model amb el mateix AIC = 13147

# 4.4. Best glm model ----------------------------------------------------------

modelSW <- glm(formula = ES_GREU ~ D_SUBZONA + F_VEH_PESANTS_IMPLICADES + 
      F_VEH_LLEUGERS_IMPLICADES + tipAcc + hor + nomDem + C_VELOCITAT_VIA +
      F_CICLOMOTORS_IMPLICADES + D_TIPUS_VIA + tipDia + F_VIANANTS_IMPLICADES +
      D_INTER_SECCIO + D_ACC_AMB_FUGA + month + D_SUPERFICIE2 +
      F_ALTRES_UNIT_IMPLICADES, data = accidents)


modelFull <- glm(ES_GREU ~ .,
                family = 'binomial', data = accidents)

lapply(list(modelSW = modelSW, modelFull = modelFull),extractAIC)


# 5. CV - Five Folds -----------------------------------------------------------

# Canviem nom a la base de dades
df <- accidents

# Canviar nom columna resposta!
colnames(df)[which(names(df) == "ES_GREU")] <- "resposta"
set.seed(1964) # Llavor
nfolds=5
nObs <- nrow(df) # Numero observacions
folds <- sample(nfolds, size = nObs, replace = T)
df$fold <- folds

table(df$fold, df$resposta)

# 5.1 Five folds (accurate)  (o n folds) ---------------------------------------
ngreu<-filter(df, resposta==0)
greu<-filter(df, resposta==1)

set.seed(1963)

# assign a fold to ngreu cases
tamanypaquet=trunc(nrow(ngreu)/nfolds)
tamanypaquetultim=(nrow(ngreu))-(nfolds-1)*tamanypaquet
nB = c(rep(1:(nfolds-1),each=tamanypaquet),rep(nfolds,tamanypaquetultim))
nB = sample(nB)
ngreu$fold = nB

# assign a fold to greu cases
tamanypaquet2=trunc(nrow(greu)/nfolds)
tamanypaquetultim2=(nrow(greu))-(nfolds-1)*tamanypaquet2
nM = c(rep(1:(nfolds-1),each=tamanypaquet2),rep(nfolds,tamanypaquetultim2))
nM = sample(nM)
greu$fold = nM

# rejoin data
df <- rbind(ngreu,greu)
table(df$fold, df$resposta)


# 6. 5-CV amb ModelSW ----------------------------------------------------------

# definim threshold

th = 0.17

# 6.1 Regressio Logistica ------------------------------------------------------

df$logisticPred<-1

for(fold in 1:nfolds) {

  modelLog <- glm(resposta ~ D_SUBZONA + F_VEH_PESANTS_IMPLICADES + 
                    F_VEH_LLEUGERS_IMPLICADES + tipAcc + hor + nomDem + C_VELOCITAT_VIA +
                    F_CICLOMOTORS_IMPLICADES + D_TIPUS_VIA + tipDia + F_VIANANTS_IMPLICADES +
                    D_INTER_SECCIO + D_ACC_AMB_FUGA + month + D_SUPERFICIE2 +
                    F_ALTRES_UNIT_IMPLICADES,
                  family = 'binomial', data = df[df$fold != fold,])

  predLog <- predict(modelLog, newdata = df[df$fold == fold,], type = 'response')

  df$logisticPred[df$fold == fold] <- predLog

}

# 6.2. Lineal Discriminant Analysis --------------------------------------------

df$ldaPred<-1

for(fold in 1:nfolds) {

  modLda = lda(resposta ~ D_SUBZONA + F_VEH_PESANTS_IMPLICADES + 
                 F_VEH_LLEUGERS_IMPLICADES + tipAcc + hor + nomDem + C_VELOCITAT_VIA +
                 F_CICLOMOTORS_IMPLICADES + D_TIPUS_VIA + tipDia + F_VIANANTS_IMPLICADES +
                 D_INTER_SECCIO + D_ACC_AMB_FUGA + month + D_SUPERFICIE2 +
                 F_ALTRES_UNIT_IMPLICADES,
               data = df[df$fold != fold, 1:30])

  predLda = predict(modLda,
                    newdata = df[df$fold == fold, ])

  df$ldaPred[df$fold == fold] <- predLda$posterior[,2]

}

# 6.3. Quadratic Discriminant Analysis -----------------------------------------

df$qdaPred<-1
for(fold in 1:nfolds) {

  modQda = qda(resposta ~ D_SUBZONA + F_VEH_PESANTS_IMPLICADES + 
                 F_VEH_LLEUGERS_IMPLICADES + tipAcc + hor + nomDem + C_VELOCITAT_VIA +
                 F_CICLOMOTORS_IMPLICADES + D_TIPUS_VIA + tipDia + F_VIANANTS_IMPLICADES +
                 D_INTER_SECCIO + D_ACC_AMB_FUGA + month + D_SUPERFICIE2 +
                 F_ALTRES_UNIT_IMPLICADES,
               data = df[df$fold != fold, 1:30])

  predQda = predict(modQda,
                    newdata = df[df$fold == fold, ])

  df$qdaPred[df$fold == fold] <- predQda$posterior[,2]

}


# 7 Comparar els 3 MODELS LINEALS 5-V ModelSW ----------------------------------
# 7.1 Matriu confusio regressioLog ---------------------------------------------

nObs <- nrow(df)
taulaLog <- table(df$resposta, df$logisticPred>th, dnn = c('actual','predicted'))
print(taulaLog)

# Error rate es fa servir quan els falsos negatius i falsos positius tenen costos similars
ErrorRateLG= (taulaLog[2]+taulaLog[3])/nObs
ErrorRateLG

# Accuracy (igual q error rate pero mires el encert) es fa servir quan els falsos negatius i falsos positius tenen costos similars
AccuracyLG = 1-ErrorRateLG
AccuracyLG

# Sensitivitat es fa servir quan no acceptes falsos negatius. Ex: cancer o Covid
SensitivityLG = taulaLog[4]/(taulaLog[2]+taulaLog[4])
SensitivityLG

# Specificity quan no vols donar falses alarmes
SpecificityLG = taulaLog[1]/(taulaLog[1]+taulaLog[3])
SpecificityLG

# Precission quan vols minimitzar falsos positius (Ex:spam)
PrecissionLG = taulaLog[4]/(taulaLog[3]+taulaLog[4])
PrecissionLG

# 7.2 Matriu confusio LDA ------------------------------------------------------

taulaLDA <- table(df$resposta, df$ldaPred>th, dnn = c('actual','predicted'))
print(taulaLDA)

# Error rate es fa servir quan els falsos negatius i falsos positius tenen costos similars
ErrorRateLDA= (taulaLDA[2]+taulaLDA[3])/nObs
ErrorRateLDA

# Accuracy (igual q error rate pero mires el encert) es fa servir quan els falsos negatius i falsos positius tenen costos similars
AccuracyLDA = 1-ErrorRateLDA
AccuracyLDA

# Sensitivitat es fa servir quan no acceptes falsos negatius. Ex: cancer o Covid
SensitivityLDA = taulaLDA[4]/(taulaLDA[2]+taulaLDA[4])
SensitivityLDA

# Specificity quan no vols donar falses alarmes
SpecificityLDA = taulaLDA[1]/(taulaLDA[1]+taulaLDA[3])
SpecificityLDA

# Precission quan vols minimitzar falsos positius (Ex:spam)
PrecissionLDA = taulaLDA[4]/(taulaLDA[3]+taulaLDA[4])
PrecissionLDA

# 7.3 Matriu confusio QDA ------------------------------------------------------

taulaQDA <- table(df$resposta, df$qdaPred>th, dnn = c('actual','predicted'))
print(taulaQDA)

# Error rate es fa servir quan els falsos negatius i falsos positius tenen costos similars
ErrorRateQDA= (taulaQDA[2]+taulaQDA[3])/nObs
ErrorRateQDA

# Accuracy (igual q error rate pero mires el encert) es fa servir quan els falsos negatius i falsos positius tenen costos similars
AccuracyQDA = 1-ErrorRateQDA
AccuracyQDA

# Sensitivitat es fa servir quan no acceptes falsos negatius. Ex: cancer o Covid
SensitivityQDA = taulaQDA[4]/(taulaQDA[2]+taulaQDA[4])
SensitivityQDA

# Specificity quan no vols donar falses alarmes
SpecificityQDA = taulaQDA[1]/(taulaQDA[1]+taulaQDA[3])
SpecificityQDA

# Precission quan vols minimitzar falsos positius (Ex:spam)
PrecissionQDA = taulaQDA[4]/(taulaQDA[3]+taulaQDA[4])
PrecissionQDA


# 7.4 AUC dels 3 models lineals ------------------------------------------------

predROClog <- prediction(predictions = df[,'logisticPred'],labels = df$resposta)
predROClda <- prediction(predictions = df[,'ldaPred'],labels = df$resposta)
predROCqda <- prediction(predictions = df[,'qdaPred'],labels = df$resposta)

perfROClog <- performance(prediction.obj = predROClog, measure = 'auc')
perfROClda <- performance(prediction.obj = predROClda, measure = 'auc')
perfROCqda <- performance(prediction.obj = predROCqda, measure = 'auc')

perfROClog@y.values[[1]]

# AUC = 0.7319659

perfROClda@y.values[[1]]

# AUC = 0.7327411

perfROCqda@y.values[[1]]

# AUC = 0.6836969

# ROC curves

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


# 8. Tree based methods --------------------------------------------------------
# 8.1 Random forest (factors) --------------------------------------------------

# 8.1.1 Splitting data in train and test data ----------------------------------

set.seed(120)

trainSize = round(dim(accidents)[1]*0.7,0)
train = sample(1:dim(accidents)[1], trainSize)
test = -train

# 8.1.2 Fitting Random Forest to the train dataset -----------------------------
set.seed(120)  # Setting seed

randForestfact = randomForest(as.factor(ES_GREU) ~ .,  
                              data = accidents[train,],
                              ntree = 3000,
                              mtry = round((sqrt(ncol(accidents)-1))))

# 8.1.3 Predicting the Test set results ----------------------------------------
RFfactPred = predict(randForestfact, newdata = accidents[test,], type='prob')

RFfactPred <- (RFfactPred[,2])

taulaRF = table(accidents[test,]$ES_GREU, RFfactPred > th,dnn = c('actual','predicted'))
print(taulaRF)

# 8.1.4 Plotting model ---------------------------------------------------------
plot(randForestfact)
varImpPlot(randForestfact, pch = 16)

# Error rate es fa servir quan els falsos negatius i falsos positius tenen costos similars
ErrorRateRF= (taulaRF[2]+taulaRF[3])/nObs
ErrorRateRF

# Accuracy (igual q error rate pero mires el encert) es fa servir quan els falsos negatius i falsos positius tenen costos similars
AccuracyRF = 1-ErrorRateRF
AccuracyRF

# Sensitivitat es fa servir quan no acceptes falsos negatius. Ex: cancer o Covid
SensitivityRF = taulaRF[4]/(taulaRF[2]+taulaRF[4])
SensitivityRF

# Specificity quan no vols donar falses alarmes
SpecificityRF = taulaRF[1]/(taulaRF[1]+taulaRF[3])
SpecificityRF

# Precission quan vols minimitzar falsos positius (Ex:spam)
PrecissionRF = taulaRF[4]/(taulaRF[3]+taulaRF[4])
PrecissionRF

# 8.2 AUC Random Forest factors ------------------------------------------------

predROCRFfact <- prediction(predictions = as.numeric(as.character(RFfactPred)),labels = accidents[test,]$ES_GREU)

perfROCRFfact <- performance(prediction.obj = predROCRFfact, measure = 'auc')

perfROCRFfact@y.values[[1]]

# AUC = 0.7310612

ROC <- list(performance(prediction.obj = predROClog, measure = 'tpr',x.measure = 'fpr'),
            performance(prediction.obj = predROClda, measure = 'tpr',x.measure = 'fpr'),
            performance(prediction.obj = predROCqda, measure = 'tpr',x.measure = 'fpr'),
            performance(prediction.obj = predROCRFfact, measure = 'tpr',x.measure = 'fpr'))

ggplot(data = NULL,aes(x=x,y=y)) +
  geom_line(data = data.frame(x = ROC[[2]]@x.values[[1]],
                              y = ROC[[2]]@y.values[[1]]), aes(col='LDA'), size = 1) + 
  geom_line(data = data.frame(x = ROC[[3]]@x.values[[1]],
                              y = ROC[[3]]@y.values[[1]]), aes(col='QDA'), size = 1) +
  geom_line(data = data.frame(x = ROC[[1]]@x.values[[1]],
                              y = ROC[[1]]@y.values[[1]]), aes(col='Logistic'), size = 1) +
  geom_line(data = data.frame(x = ROC[[4]]@x.values[[1]],
                              y = ROC[[4]]@y.values[[1]]), aes(col='Random Forest Factor'), size = 1)+
  labs(x='1-specificity (False Positive Rate: FPR)',y='sensitivity (True Positive Rate (TPR)') +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2))

# 9. Boosted tree --------------------------------------------------------------

set.seed(120)  # Setting seed
boostedTree = gbm(ES_GREU ~ .,
                  data = accidents[train,],
                  distribution = 'bernoulli',         #Bernuilli perq és de dos factors 0 i 1
                  shrinkage = 0.001,
                  n.trees = 200,
                  interaction.depth = 2)

summary(boostedTree, main = 'Boosted tree (B = 200; d = 3; lambda = 0.01)')

gbmPred <- predict(boostedTree, newdata = accidents[test,], type = 'response')

# Confusion matrix
taulaBT = table(accidents[test,]$ES_GREU, gbmPred > th,dnn = c('actual','predicted'))
print(taulaBT)

# Error rate es fa servir quan els falsos negatius i falsos positius tenen costos similars
ErrorRateBT= (taulaBT[2]+taulaBT[3])/nObs
ErrorRateBT

# Accuracy (igual q error rate pero mires el encert) es fa servir quan els falsos negatius i falsos positius tenen costos similars
AccuracyBT = 1-ErrorRateBT
AccuracyBT

# Sensitivitat es fa servir quan no acceptes falsos negatius. Ex: cancer o Covid
SensitivityBT = taulaBT[4]/(taulaBT[2]+taulaBT[4])
SensitivityBT

# Specificity quan no vols donar falses alarmes
SpecificityBT = taulaBT[1]/(taulaBT[1]+taulaBT[3])
SpecificityBT

# Precission quan vols minimitzar falsos positius (Ex:spam)
PrecissionBT = taulaBT[4]/(taulaBT[3]+taulaBT[4])
PrecissionBT

# 9.1 AUC Boosted tree ---------------------------------------------------------

predROCbt <- prediction(predictions = gbmPred,labels = accidents[test,]$ES_GREU)

perfROCbt <- performance(prediction.obj = predROCbt, measure = 'auc')

perfROCbt@y.values[[1]]
#AUC = 0.6913626


# 10. ROC dels 5 models --------------------------------------------------------

ROC <- list(performance(prediction.obj = predROClog, measure = 'tpr',x.measure = 'fpr'),
            performance(prediction.obj = predROClda, measure = 'tpr',x.measure = 'fpr'),
            performance(prediction.obj = predROCqda, measure = 'tpr',x.measure = 'fpr'),
            performance(prediction.obj = predROCRFfact, measure = 'tpr',x.measure = 'fpr'),
            performance(prediction.obj = predROCbt, measure = 'tpr',x.measure = 'fpr'))

ggplot(data = NULL,aes(x=x,y=y)) +
  geom_line(data = data.frame(x = ROC[[2]]@x.values[[1]],
                              y = ROC[[2]]@y.values[[1]]), aes(col='LDA'), size = 1) + 
  geom_line(data = data.frame(x = ROC[[3]]@x.values[[1]],
                              y = ROC[[3]]@y.values[[1]]), aes(col='QDA'), size = 1) +
  geom_line(data = data.frame(x = ROC[[1]]@x.values[[1]],
                              y = ROC[[1]]@y.values[[1]]), aes(col='Logistic'), size = 1) +
  geom_line(data = data.frame(x = ROC[[4]]@x.values[[1]],
                              y = ROC[[4]]@y.values[[1]]), aes(col='Random Forest'), size = 1)+
  geom_line(data = data.frame(x = ROC[[5]]@x.values[[1]],
                              y = ROC[[5]]@y.values[[1]]), aes(col='Boosted Tree'), size = 1)+
  labs(x='1-specificity (False Positive Rate: FPR)',y='sensitivity (True Positive Rate (TPR)') +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2))


# 11. Analitzar el model -------------------------------------------------------

#model final:

modelLogFinal <- glm(ES_GREU ~ D_SUBZONA + F_VEH_PESANTS_IMPLICADES + 
                  F_VEH_LLEUGERS_IMPLICADES + tipAcc + hor + nomDem + C_VELOCITAT_VIA +
                  F_CICLOMOTORS_IMPLICADES + D_TIPUS_VIA + tipDia + F_VIANANTS_IMPLICADES +
                  D_INTER_SECCIO + D_ACC_AMB_FUGA + month + D_SUPERFICIE2 +
                  F_ALTRES_UNIT_IMPLICADES,
                family = 'binomial', data = accidents)


summary(modelLog)
summary(modelLog)$coef

# 11.1 Escrivim eqüació logOdds ------------------------------------------------

# Extract the coefficients
coefficients <- modelLogFinal$coefficients

# Get the names of the variables
variable_names <- names(coefficients)

# The intercept is the first element in the coefficients vector
intercept <- coefficients[1]

# Initialize an empty equation
equation <- ""

# Loop through the rest of the coefficients
for(i in 2:length(coefficients)) {
  # Round the coefficient to the nearest cent
  coefficient <- round(coefficients[i], 2)
  
  # Add the term for this variable to the equation
  equation <- paste(equation, "+", coefficient, "*", variable_names[i])
}

# Prefix the intercept
equation <- paste("log(P(ES_GREU = 1)/ P(ES_GREU = 0)) =", intercept, equation)

# Print the equation
print(equation)

# 12. Plot Graphics ------------------------------------------------------------

# Count the number of accidents for each month
accidents_by_month <- table(accidents$month)

# Calculate the percentage of accidents for each month
accidents_by_month_percent <- (accidents_by_month / sum(accidents_by_month)) * 100

# Create a bar chart
barplot(accidents_by_month_percent[order(as.numeric(names(accidents_by_month_percent)))],
        main = "Percentage of Accidents by Month",
        xlab = "Month",
        ylab = "Percentage of Accidents",
        names.arg = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Count the number of accidents for each month
accidents_by_hor <- table(accidents$hor)

# Calculate the percentage of accidents for each hor
accidents_by_hor_percent <- (accidents_by_hor / sum(accidents_by_hor)) * 100

# Create a bar chart
barplot(accidents_by_hor_percent[order(as.numeric(names(accidents_by_hor_percent)))],
        main = "Percentage of Accidents by Hour",
        xlab = "Hours",
        ylab = "Percentage of Accidents")

# Count the number of accidents for each month
accidents_by_month <- table(accidents$month)

# Calculate the percentage of accidents for each month
accidents_by_month_percent <- prop.table(table(accidents$ES_GREU, accidents$month),2)[2,]

# Create a bar chart
barplot(accidents_by_month_percent[order(as.numeric(names(accidents_by_month_percent)))],
        main = "Percentage of Accidents molt greus by Month",
        xlab = "Month",
        ylab = "Percentage of Accidents Molt Greus",
        names.arg = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Count the number of accidents for each month
accidents_by_hor <- table(accidents$hor)

# Calculate the percentage of accidents for each hor
accidents_by_hor_percent <- prop.table(table(accidents$ES_GREU, accidents$hor),2)[2,]

# Create a bar chart
barplot(accidents_by_hor_percent[order(as.numeric(names(accidents_by_hor_percent)))],
        main = "Percentage of Accidents Molt Greus by Hour",
        xlab = "Hours",
        ylab = "Percentage of Accidents Molt Greus")