
#1. Load data ---------------------------------------------------------------

df <- read.csv2(file = 'data/wisconsin.csv')
str(df)
df<-subset(df,select = -c(diagnosis))

# 2.What is the Q1 and Q3 values of the fractal variable? ---------------------------------------------------------------------

summary(df$fractal)

# Min.     1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.04996 0.05770 0.06154 0.06280 0.06612 0.09744 


# 3. Build a matrix scatterplot  --------
# 3. Build a matrix scatterplot relating the 
# response (fractal) to the rest of the predictors
                                              
pairs(df, pch = 19)


# 4 --------------------------------------------------------------------


# 5. ----------------------------------------------------------------------

# 5. Do you think it makes sense to include radius 
# and perimeter in the model as independent variables 
# at the same time? Why?
# No, estan molt correlacionades (volen dir el mateix)


# 6. Best subsets----------------------------------------------------------------------

# 6. With the regsubset function, obtain the best regression model to predict 
# the fractal value using one variable, two variables, etc. up to all available 
# variables. Do not include the perimeter and diagnosis 
# as independent variables in any model. 
library(leaps)


sub.fit <- regsubsets(fractal ~ . -perimeter,
                      data = df,
                      nbest = 1,
                      nvmax = NULL,
                      method = 'forward')
bestSubsetSummary <- summary(sub.fit)
bestSubsetSummary
par(mfrow=c(2,2))
plot(bestSubsetSummary$rsq, type='b', pch=16, 
     ylab='R2', xlab='Model size', main = 'R2')
plot(bestSubsetSummary$bic, type='b', pch=16, 
     ylab='BIC', xlab='Model size', main = 'BIC')
plot(bestSubsetSummary$adjr2, type='b', pch=16, 
     ylab='R2adj', xlab='Model size', main = 'R2adj')
#Ens quedem amb el model amb el BIC mes baix -> el de 3 variables



# 9. Liniar model----------------------------------------------------------------------

# 9. Write down the formula for 
# the best model chosen from previous question 8.

bestmodel <- lm(fractal ~ radius + smoothness + concavity, data=df)
summary(bestmodel)
par(mfrow = c(2,2))
plot(bestmodel, pch = 16, main = 'Best model')


predict(bestmodel, newdata = df[20,])



# 2a part: Arbres ---------------------------------------------------------
# a. Load the data from the file wisconsin.csv
# b. Convert the variable diagnosis from integer to factor (this is the label)

df <- read.csv2(file = 'data/wisconsin.csv')
df$diagnosis<- as.factor(df$diagnosis)
str(df)


# 1.  ---------------------------------------------------------------------

# 1. Split the 596 in two random groups, one having 417 observations and the other the remaining 
# 179. This is a 70-30% training to test split.

set.seed(1936)

trainSize = 417
train = sample(1:dim(df)[1], trainSize)
test = -train

dftrain<-df[train,]
dftest<-df[test,]


# 2. ----------------------------------------------------------------------

# 2. Check and write down the percentage of malign/benign cases in the 
# training and test sets. They  should be similar. 
prop.table(table(dftrain$diagnosis))
prop.table(table(dftest$diagnosis))


# Single tree -------------------------------------------------------------


# 3. ----------------------------------------------------------------------

# 3. Using the training set only, build a classification tree. 
# Although it is the R function default, if you 
# want to specify it, use the deviance criteria to split nodes. Plot the tree.

fullclastree = tree(diagnosis ~ . -diagnosis , data = dftrain, split = 'deviance')
plot(fullclastree)
text(fullclastree, cex = 0.7)
summary(fullclastree)
fullclastree
deviance(fullclastree)



# 4. ----------------------------------------------------------------------

# 4. Focusing only on the first cut (first splitting variable), 
# what is the first chosen variable? What is the 
# percentage or malign cases in the left and right branches 
# of this first cut? 
# R: concavepoints
# Left:0.122699 0.877301  Right:0.952756 0.047244
fullclastree$frame[c('1','2','3'),]


# 5. ----------------------------------------------------------------------

# 5. Using the training tree, predict the test set. 
# What is the misclassification error? What is the classifier 
# sensitivity?

test.prediction = predict(fullclastree, newdata = dftest, type = 'class')
taulatree<-table(dftest$diagnosis,test.prediction,dnn = c('actual','predicted'))
taulatree

ErrorRatetree= (taulatree[2]+taulatree[3])/(taulatree[1]+taulatree[2]+taulatree[3]+taulatree[4])
ErrorRatetree
SensitivityTree = taulatree[4]/(taulatree[2]+taulatree[4])
SensitivityTree


# Bagging -----------------------------------------------------------------


# 6. ----------------------------------------------------------------------

# 6. Implement a bagged forest using 1,000 trees and a minimum node size of 5. 
baggedTrees = randomForest(diagnosis ~ .,  
                           data = dftrain,
                           ntree = 1000,
                           nodesize = 5,
                           mtry = ncol(df)-1,
                           replace = T,
                           keep.forest = TRUE,
                           xtest = dftest[2:10],
                           ytest = dftest$diagnosis,
                           do.trace = F)
baggedTrees


# 7. ----------------------------------------------------------------------

# . Which is the OOB misclassification with the build forest? 
# R:6%
# What is OOB? 

# What does it do?


# 8. ----------------------------------------------------------------------

# 8. Using the training bagged forest, predict the test set. 
# What is the misclassification error? What is the classifier sensitivity?

th=0.5

BFfactPred = predict(baggedTrees, newdata = dftest, type='prob')

BFfactPred <- (BFfactPred[,2])

taulaBF = table(dftest$diagnosis, BFfactPred > th,dnn = c('actual','predicted'))
print(taulaBF)


ErrorRatetree= (taulaBF[2]+taulaBF[3])/(taulaBF[1]+taulaBF[2]+taulaBF[3]+taulaBF[4])
ErrorRatetree
SensitivityTree = taulaBF[4]/(taulaBF[2]+taulaBF[4])
SensitivityTree


# Random forest -----------------------------------------------------------


# 9. ----------------------------------------------------------------------

# 9. Build a random forest with 1,000 trees, a minimum node size of 5 and 
# only 3 variables available at  each split

randForestfact = randomForest(diagnosis ~ .,  
                          data = dftrain,
                          ntree = 1000,
                          nodesize=5,
                          mtry = round((sqrt(ncol(dftrain)-1))),
                          replace = T,
                          keep.forest = T,
                          do.trace = F,
                          xtest = dftest[2:10],
                          ytest = dftest$diagnosis,
                          importance = F)
randForestfact

# 10. ---------------------------------------------------------------------

# 10.Using the training tree, predict the test set. 
# What is the misclassification error? What is the classifier 
# sensitivity?
th=0.5

RFfactPred = predict(randForestfact, newdata = dftest, type='prob')

RFfactPred <- (RFfactPred[,2])

taulaRF = table(dftest$diagnosis, RFfactPred > th,dnn = c('actual','predicted'))
print(taulaRF)


ErrorRatetree= (taulaRF[2]+taulaRF[3])/(taulaRF[1]+taulaRF[2]+taulaRF[3]+taulaRF[4])
ErrorRatetree
SensitivityTree = taulaRF[4]/(taulaRF[2]+taulaRF[4])
SensitivityTree


# 8.1.4 Plotting model
plot(randForestfact)
varImpPlot(randForestfact, pch = 16)
