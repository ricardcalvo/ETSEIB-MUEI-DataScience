library(ggplot2)
library(tidyverse)
library(MASS) #Boston
library(tree)
library(randomForest) # bagging and random forest
library(gbm) #boosting


# Load Data ---------------------------------------------------------------


load(file = 'data/Hitters.RData')

df<-Hitters

colnames(df)[which(names(df) == "Salary")] <- "resposta"

summary(df) #minim, mitjana, mediana .....

table(df$resposta)

#Transofrmem resposta a log (depen del cas)
hist(df$resposta, col = 'grey', main = 'Salary distribution', xlab = 'Salary ($)')
df$resposta = log(df$resposta)


#1. Single Regression tree -------------------------------------------------------------

fulltree = tree(resposta ~ . - resposta,  data = df, method = 'deviance') # removes column Salary

summary(fulltree)
deviance(fulltree)

#Plot the tree
par(mfrow=c(1,1))
plot(fulltree)
text(fulltree)

fulltree

# the deviance is the RSS
#prediction<-predict(fulltree, newdata=df)
#sum((df$resposta-prediction)^2)

deviance(fulltree)


# 1.2. Prune tree -----------------------------------------------------------

#Evolució de l'arbre

for (treeSize in 2:10) {
  treePruned = prune.tree(tree = fulltree, best = treeSize)
  plot(treePruned)
  text(treePruned)
}


# 1.3. Regresion Tree validation set ----------------------------------------

par(mfrow=c(1,1))

set.seed(1964)

trainSize = round(dim(df)[1]*0.7,0)
train = sample(1:dim(df)[1],trainSize)
test = -train
dftrain=df[train,]


#Arbre entrenat nomes amb set train
full.train.tree = tree(resposta ~ . - resposta, data = dftrain)

plot(full.train.tree)
text(full.train.tree)
summary(full.train.tree)
full.train.tree


# 1.4. k-fold cross-validation to prune the tree -------------------------------

set.seed(1936)
pruned = cv.tree(obj = full.train.tree,
                 FUN = prune.tree,
                 K = 5)
pruned


# plot RSS evolution as a function of tree depth
data.frame(size = pruned$size, rss = pruned$dev) %>%
  ggplot(aes(x = size, y = rss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:12)

# estimate the tranining tree with the pruned depth
#Si el agafem el size amb rss menor (aqui 5)
pruned.train.tree = prune.tree(tree = full.train.tree, 
                               best = 5)
plot(pruned.train.tree)
text(pruned.train.tree)
pruned.train.tree
summary(pruned.train.tree)

# estimate the test MSE

RSS = sum((df$resposta[test]-predict(pruned.train.tree, newdata = df[test,]))^2)
nTest = dim(df[test,])[1]
RSS/nTest


# 2. Classification single tree -------------------------------------------

#load data
load(file = 'data/Carseats.RData')

df<-Carseats[,2:12]

colnames(df)[which(names(df) == "High")] <- "resposta"

summary(df) #minim, mitjana, mediana .....

table(df$resposta)

#Full tree

fullclastree = tree(resposta ~ . -resposta , data = df, split = 'deviance')
plot(fullclastree)
text(fullclastree, cex = 0.7)
summary(fullclastree)
fullclastree
deviance(fullclastree)


# 2.2 Validation set error estimate ---------------------------------------

set.seed(1964)

trainSize = round(dim(df)[1]*0.7,0)
train = sample(1:dim(df)[1],trainSize)
test = -train
dftrain=df[train,]

tree.training.df = tree(resposta ~ . , data = dftrain)

plot(tree.training.df)
text(tree.training.df, cex = 0.7)

test.prediction = predict(tree.training.df, newdata = df[-train,], type = 'class')
table(df$resposta[-train],test.prediction)

sum(df$resposta[-train] == test.prediction)/length(df$resposta[-train])


# 2.3 Tree pruning --------------------------------------------------------

# prune the tree
set.seed(3)
pruned = cv.tree(obj = tree.training.df,
                 FUN = prune.misclass,
                 K = 5)
pruned


# plot deviance evolution as a function of tree depth
data.frame(size = pruned$size, errorRate = pruned$dev/length(train)) %>%
  ggplot(aes(x = size, y = errorRate)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:30) 


# estimate the tranining tree with the pruned depth

pruned.train.tree = prune.tree(tree = tree.training.df, best = 10)

train.prediction = predict(pruned.train.tree, type = 'class')
sum(df$resposta[train] == train.prediction)/length(df$resposta[train])

# estimate the test error rate

test.prediction = predict(pruned.train.tree, newdata = df[-train,], type = 'class')
table(df$resposta[-train],test.prediction,dnn = c('actual','predicted'))

sum(df$resposta[-train] == test.prediction)/length(df$resposta[-train])

# plot the tree

plot(pruned.train.tree)
text(pruned.train.tree)

pruned.train.tree


# 2.3.1 Pruned tree evolution ---------------------------------------------


for (k in 2:12) {
  treePruned = prune.tree(tree = tree.training.df, best = k)
  plot(treePruned)
  text(treePruned)
}


# 3. Bagging -------------------------------------------------------------

#Load data
df<-Boston
set.seed(120)
#Canviar nom columna resposta
colnames(df)[which(names(df) == "medv")] <- "resposta"

summary(df) #minim, mitjana, mediana .....

#Perform bagging

set.seed(1925)
par(mfrow = c(1,1))

trainSize = round(dim(df)[1]*0.7,0)
train = sample(1:dim(df)[1],trainSize)
test = -train

baggedTrees = randomForest(resposta ~ .,  
                           data = df,
                           subset = train,
                           ntree = 20,
                           nodesize = 5,
                           mtry = ncol(df)-1,
                           replace = T,
                           keep.forest = TRUE,
                           xtest = df[test,-14],
                           ytest = df$resposta[test],
                           do.trace = 1)
baggedTrees

#Plot of MSE depenent del n d'arbres
plot(baggedTrees$test$mse, type = 'b', pch = 16, xlab = 'no trees', ylab = 'test MSE', 
     main = 'Test set MSE')

# compute MSE by hand

mean((df$resposta[test]- predict(baggedTrees, newdata = df[test,]))^2)

# the randomForest returns a test list including the error for each test value

mean((df$resposta[test] - baggedTrees$test$predicted)^2)


#3.1 Variable importance Bagging-------------------------------------------------------------------------

baggedTree = randomForest(resposta ~ .,  
                          data = df, 
                          ntree = 100,
                          mtry = dim(df)[2]-1,
                          keep.forest = TRUE)

baggedTree$importance

importance(baggedTree)

varImpPlot(baggedTree, main = 'XXXX dataset. Forest of n bagged trees', pch = 16)



#4 Random forest -------------------------------------------------------
#Splitting data in train and test data

set.seed(120)

trainSize = round(dim(df)[1]*0.7,0)
train = sample(1:dim(df)[1], trainSize)
test = -train



randForest = randomForest(resposta ~ .,  
                          data = df[train,],
                          ntree = 200,
                          mtry = round((ncol(df)-1)/3))  #round((sqrt(ncol(accidents)-1)))


randForest$mse[200]
importance(randForest)

# Var importance Random forest --------------------------------------------

varImpPlot(randForest, pch = 16)

#Taula veritat (en cas de random forest factors)

th=0.5
RFPred = predict(randForest, newdata = df[test,], type='prob')

RFPred <- (RFPred[,2])

taulaRF = table(df[test,]$resposta, RFPred > th,dnn = c('actual','predicted'))
print(taulaRF)


