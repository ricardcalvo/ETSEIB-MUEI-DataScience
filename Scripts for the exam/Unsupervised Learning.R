
# Manera 1 de PCR and PLS regression-------------------------------------------------------------------------


# Library
library(tidyverse)
library(caret)    
library(pls)    

# Load the data
df <- read.csv2(file = 'data/wisconsin.csv', dec=',')

# Split the data into training and test
set.seed(3)

# Train sample identifier, caret::createDataPartition()
split <- df$resposta %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df[split, ]
test.data <- df[-split, ]


# 1. Principal components regression -----------------------------------------

set.seed(3) #llavor

# 1.1 Fem pcr del dataset complet

pcr.fit <- pcr(resposta~., data=df, scale=TRUE, validation="CV") 
#Podriem seleccionar amb quines variables treballar, aqui ho fem amb totes. 
#scale = TRUE serveix per estandaritzar les variables (s'ha de fer sempre)

summary(pcr.fit) #podem veure com explica la variablitat cada component
# Veiem com el 100% de la variabilitat nomes l'expliquen tots els components

validationplot(pcr.fit,val.type="MSEP")
# Ens hem de fixar amb quin nombre de components hi ha el MSEP més petit

# 1.2 Train the model: fem el mateix nomès amb el dataset de train
set.seed(3)
pcr.fit <- pcr(resposta ~., data=train.data, scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
summary(pcr.fit)

# 1.3 Test
# fem la prediccio amb n components
pcr.pred <- predict(pcr.fit, test.data, ncomp=2)
#Càlcul de l'error
mean((pcr.pred - test.data$Salary)^2)

#Un cop veiem q l'error no es massa gran (no estem sobreajustant),
# 1.4 creem el model amb totes les dades i ncomponents
pcr.fit <- pcr(resposta ~ ., data=df, scale=TRUE,ncomp=2)
summary(pcr.fit)


# 2. Partial Last Squares ----------------------------------------------------

# Train
set.seed(1)
pls.fit <- plsr(resposta~., data=train.data, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

# Test
predictions <- predict(pls.fit, test.data, ncomp=2)
mean((predictions - test.data$resposta)^2)

pls.fit <- plsr(resposta ~ ., data=df, scale=TRUE,ncomp=2)
summary(pls.fit)


# Manera 2 de fer PCR i PLS -----------------------------------------------

# 1. PCR ------------------------------------------------------------------


# Build the model on training set
set.seed(1)
model <- train(
  medv ~ ., 
  data = train.data, 
  method = "pcr",
  scale = TRUE,
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

# Plot model RMSE vs different values of components
plot(model)

# Print the best tuning parameter ncomp that
# minimize the cross-validation error, RMSE
model$bestTune

# Summarize the final model
summary(model$finalModel)

# Make predictions
predictions <- model %>% predict(test.data)

# Model performance metrics
(caret::RMSE(predictions, test.data$resposta))
(caret::R2(predictions, test.data$resposta))




# 2. PLS ------------------------------------------------------------------

# Build the model on training set
set.seed(1)
model <- train(
  medv ~ ., 
  data = train.data, 
  method = "pls",
  scale = TRUE,
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

# Plot model RMSE vs different values of components
plot(model)

# Print the best tuning parameter ncomp that
# minimize the cross-validation error, RMSE
model$bestTune

# Summarize the final model
summary(model$finalModel)

# Make predictions
predictions <- model %>% predict(test.data)

# Model performance metrics
caret::RMSE(predictions, test.data$resposta)
caret::R2(predictions, test.data$resposta)


# Clustering --------------------------------------------------------------

# Center and scale the numerical data:
df_scaled <- scale(df[3:32], center=TRUE, scale=TRUE) #Seleccionem les cols numèriques
#df <- as.data.frame(df)
df<-df_scaled

# 3. K-Means clustering----------------------------------------------------------------------

# 3.1 K-means, with K=2
km.out2 <- kmeans(df, 2, nstart = 21)   #nstart es com la llavor (quantes vegades repetim l'algoritme)

# Cluster assigments for each point
km.out2$cluster

# Plot the cluster
km.out2$cluster
ggplot(df) +
  geom_point(aes(x, y, color=as_factor(km.out2$cluster)))

# 3.2 K-means, with K=3

km.out <- kmeans(df, 3, nstart = 20)   #nstart es com la llavor

# Cluster assigments for each point
km.out$cluster

# Plot the cluster
km.out$cluster
ggplot(df) +
  geom_point(aes(x, y, color=as_factor(km.out$cluster)))


# 3.3 Control the initial cluster assignments: nstart
set.seed(3)
km.out <- kmeans(df, 3, nstart = 1)
km.out$tot.withinss

km.out <- kmeans(df, 3, nstart = 20)
km.out$tot.withinss





# 4. Hierarchical clustering ----------------------------------------------

# Same x data
x <- df

#Calcular distancies heuclidies entre parelles:
dist(x)

# create a hierarchical  clustering model: 
#Depenem de l'enunciat fem un linkage o atre (method)

hc.complete <- hclust(dist(x), method = "complete")
plot(hc.complete) #plot the model
hc.average <- hclust(dist(x), method = "average")
plot(hc.average) #plot the model
hc.single <- hclust(dist(x), method = "single")
plot(hc.single) #plot the model

# Linkage comparison: comparem els tres models
par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = 0.9)

# labels at the bottom: si volem que tots els nivells arribin abaix
plot(hc.complete, hang = -0.01, cex = 0.7)

# Cut the tree
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

ggplot(df) +
  geom_point(aes(x, y, color=cutree(hc.complete, 2)))

ggplot(df) +
  geom_point(aes(x, y, color=cutree(hc.average, 2)))


ggplot(df) +
  geom_point(aes(x, y, color=cutree(hc.single, 2)))


# Cut the tree hc by 'k' clusters or at an height 'h'
# k=4
fit <- cutree(hc.complete, k=4)
table(fit)

# plot the clusters
rect.hclust(hc.complete, k=4, border = "red")
rect.hclust(hc.complete, k=5, border = "blue")

# 2.3 Height for 4 clusters
cutree(model_hc, h=20)


# Table comparing results
km.out2$cluster
cutree(hc.complete, 2)
table(km.out2$cluster,cutree(hc.complete, 2))


