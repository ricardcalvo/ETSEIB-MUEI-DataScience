#1 Loading Libraries -------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(leaps) #needed for the best subset modeling

library(stargazer)

#2 Loading and Cleaning Data ------------------------------------------------

# Create the data frame.

basecreada <- 	data.frame(
  gender = factor(c("Male", "Male","Female")), 
  height = c(162, 181.5, 165.4), 
  weight = c(45,93, 64),
  Age = c(42,38,26)
)


#carregar base de dades
df <- read.csv2(file = 'data/southAfricanHeartDisease.csv', dec=',')
str(df)

summary(df) #minim, mitjana, mediana .....

#x= ... 
#y= ...
table(x,y)  # Taula de freq?encia -> x files, y columnes

head(df) #primeres files

tail(df)  #?ltimes files?

plot(df) #forma basica presentacio (tipo pairs)
str(df) #estructura de la base de dades

#Propietats de la base de dades

# Number of rows
nrow(df)

# Number of columns
ncol(df)

# Dimensions
dim(df)

# Column names
colnames(df)

#pairs
pairs(df, col = 'red', pch = 16)

# 3.Regression Models --------------------------------------------------

##3.1 Opcio1: Crear model lineal----------Transformar variables
resposta= #nom de la columna resposta
var1= #nom de la columna corresponent primera variable explicativa
var2= #nom de la segona variable explicativa
var3= #podem transformar una variable ex: I(var1^2)
var4: #log10(var1) o I(1/var1)
  
model1 <- lm(resposta ~ var1, data=df)
summary(model1)
par(mfrow = c(2,2))
plot(model1, pch = 16, main = 'Titol model 1')


model2 <- lm(resposta ~ var2, data=df)
summary(model2)
par(mfrow = c(2,2))
plot(model2, pch = 16, main = 'Titol model 2')


model3 <- lm(resposta ~ var1 + var2, data=df)
summary(model3)
par(mfrow = c(2,2))
plot(model3, pch = 16, main = 'Titol model 3')

model4 <- lm(resposta ~ var3, data=df)
summary(model4)
par(mfrow = c(2,2))
plot(model3, pch = 16, main = 'Titol model 4')

#cal fixar-se en la forma dels residus, si no és correcte cal transformar avriables

##Representar models que depenen nomes d'una variable Ex: var1, var1^2, 1/var1


ggplot(data = df, aes(x = var1, y = resposta))+
  geom_point(col = 'red') +
  geom_line(aes(x = var1, y = predict(mod1), col = 'Títol model 1'), size = 1) +
  geom_line(aes(x = var1, y = predict(mod2), col = 'Títol model 2'), size = 1) +
  geom_line(aes(x = var1, y = predict(mod3), col = 'Títol model 3'), size = 1)

#Mirem el valor del AIC
lapply(list(mod1,mod2,mod3,mod4), extractAIC)


#Mesures per triar el millor model
##R^2adj alt
##AIC baix
##BIC baix

# 4. Best subsets ------------------------------------------------------------


#Best Subset approach
resposta= #nom de la columna resposta
var1= #nom de la columna corresponent primera variable explicativa
var2= #nom de la segona variable explicativa
var3= #3a variable explicativa
var4: #4a variable explicativa

sub.fit <- regsubsets(resposta ~ var1 + var2 + var3 + var4,
                      data = df,
                      nbest = 1,
                      nvmax = NULL,
                      method = 'exhaustive')
bestSubsetSummary <- summary(sub.fit)
bestSubsetSummary
par(mfrow=c(2,2))
plot(bestSubsetSummary$rsq, type='b', pch=16, 
     ylab='R2', xlab='Model size', main = 'R2')
plot(bestSubsetSummary$bic, type='b', pch=16, 
     ylab='BIC', xlab='Model size', main = 'BIC')
plot(bestSubsetSummary$adjr2, type='b', pch=16, 
     ylab='R2adj', xlab='Model size', main = 'R2adj')
#Ens quedem amb el model amb el BIC mes baix


# 5. Stepwise -------------------------------------------------------------


#3.3 Stepwise approach

# null model
resposta= #nom de la columna resposta
modelNull <- lm(resposta ~ 1, data = df)
summary(modelNull)

# full model

modelFull <- lm(resposta ~ .,data = df)
summary(modelFull)

# stepwise forward

modSWFwd <-  step(modelNull,
                  scope=list(lower=modelNull,upper=modelFull),
                  direction='forward',
                  trace=1)
summary(modSWFwd)
extractAIC(modSWFwd)

# stepwise backward

modSWBwd <-  step(modelFull,
                  scope=list(lower=modelNull,upper=modelFull),
                  direction='backward',
                  trace=1)
summary(modSWBwd)
extractAIC(modSWBwd)


# Predict mitjana

round(predict(mod2, newdata = data.frame(Age = 60, Mileage = 50000), interval = c('confidence'), level=0.90), 1)

# Predict un numero en concret

round(predict(mod2, newdata = data.frame(Age = 60, Mileage = 50000), interval = c('prediction'), level=0.90), 1)


