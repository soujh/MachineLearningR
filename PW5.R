##### Regresion Trees ######

## Single Tree ##


library(MASS)
library(caTools)
set.seed(18)
Boston_idx = sample(1:nrow(Boston), nrow(Boston) / 2)
Boston_train = Boston[Boston_idx,]
Boston_test  = Boston[-Boston_idx,]




library(rpart)
library(rpart.plot)



Boston_tree= rpart(medv~.,data = Boston_train)
plot(Boston_tree)
text(Boston_tree, pretty = 0)
title(main = "Regression Tree")



rpart.plot(Boston_tree)



#Q5
summary(Boston_tree)
Boston_tree$cptable
printcp(Boston_tree) 
plotcp(Boston_tree)
# As a rule of thumb, itâ€™s best to prune a decision tree using the cp of smallest tree that is within 
# one standard deviation of the tree with the smallest xerror. In this example, the best xerror is
best_cv_error <- Boston_tree$cptable[which.min(Boston_tree$cptable[,4]),4]
# achieved at :
index_best <- which.min(Boston_tree$cptable[,4])
# 34
# and equal to 
Boston_tree$cptable[index_best,4]



Boston_tree$cptable
# 0.2314857 with standard deviation 
Boston_tree$cptable[index_best,5]
# 0.04287064. So, we want the smallest tree with xerror less than  0.2314857 + 0.04287064
# = 0.2743
# This is the tree with cp =  0.01665273, so weâ€™ll want to prune our tree with a cp slightly greater than than 0.2743.



Boston_tree <- prune(Boston_tree,cp=  0.02929834)
rpart.plot(Boston_tree)



par(mfrow=c(1,1))
X <- Boston_train$lstat
Y <- Boston_train$rm
Z <- Boston_train$medv
ZZ = abs(Z)/max(Z)
plot(Y~X,  col=rgb(0, 0, ZZ), main="trainins set")
abline(v=5.3,lty=2,col=4)
abline(h=7.4,lty=2,col=4)




#Q6
actuel=Boston_test$medv
Boston_tree_pred= predict(Boston_tree,data=Boston_test)



rmse = function(actual,predicted){
  sqrt(mean((actual-predicted)^2))
}



rmse(Boston_tree_pred,Boston_test$medv)



#Q7

Boston_lm= lm(medv~.,data=Boston_train)
Boston_lm_pred= predict(Boston_lm,newdata = Boston_test)
rmse(Boston_lm_pred,Boston_test$medv)
library(randomForest)
Boston_forest=randomForest(medv~ ., data=Boston_train,mtry=13,importance=TRUE,ntrees=500)
Forest_predict= predict(Boston_forest,newdata = Boston_test)
rmse(Forest_predict,Boston_test$medv)

## Bagging ## 

# Question 8 et 9#

#8 Fit a bagged model, using the randomForest() function from the randomForest package.

library(randomForest)
Boston_bagging = randomForest(medv ~ ., data = Boston_train, mtry = 13,
                              importance = TRUE, ntrees = 500)
Boston_bagging

Boston_bagging_pred = predict(Boston_bagging, newdata = Boston_test)
rmse(Boston_bagging_pred, Boston_test$medv)
################################################################################
plot(Boston_bagging, col = "#cd0050", lwd = 2, main = "Bagged Trees: Error vs Number of Trees")
grid()
Boston_forest = randomForest(medv ~ ., data = Boston_train, mtry = 4,
                             importance = TRUE, ntrees = 500)
Boston_forest
Boston_forest_pred = predict(Boston_forest, newdata = Boston_test)

rmse(Boston_forest_pred, Boston_test$medv)

plot(Boston_tree_pred, Boston_test$medv, 
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Single Tree, Test Data",
     col = "#cd0050", pch = 20)
grid()
abline(0, 1, col = "dodgerblue", lwd = 2)


#Question 10# 

library(gbm)
Boston_boost = gbm(medv ~ ., data = Boston_train, distribution = "gaussian", 
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)

Boston_boost_pred = predict(Boston_boost, newdata = Boston_test)

rmse(Boston_boost_pred, Boston_test$medv)

# Question 11 # 
summary(Boston_boost)

##### Classification Trees #####

calc_acc=function(predicted,actual){
  mean(predicted==actual)
}

dataset=read.csv("spam.csv")

#Exploring the dataset
str(dataset) 
head(dataset)

#Split the dataset

set.seed(123)
spam_idx = sample(1:nrow(dataset), nrow(dataset) / 2)
spam_train = dataset[spam_idx,]
spam_test  = dataset[-spam_idx,]

# Logistic Regression #


rmse = function(actual,predicted){
  sqrt(mean((actual-predicted)^2))
}

Spam_lm= lm(spam~.,data = spam_train)
spam_predicted= predict(Spam_lm,newdata = spam_test)
rmse(spam_predicted,dataset$spam)

# Simple Classification tree # 

spam_tree= rpart(spam ~.,data = spam_train)
plot(spam_tree)
text(spam_tree, pretty = 0)
title(main = "Regression Tree")

rpart.plot(spam_tree)

#Il faut ajuster le cp pour plus de précisions

spam_tree$cptable
printcp(spam_tree) 
plotcp(spam_tree)
# As a rule of thumb, itâ€™s best to prune a decision tree using the cp of smallest tree that is within 
# one standard deviation of the tree with the smallest xerror. In this example, the best xerror is
best_cv_error <-  spam_tree$cptable[which.min(spam_tree$cptable[,4]),4]
# achieved at :
index_best <- which.min(spam_tree$cptable[,4])
#10
# and equal to 
spam_tree$cptable[index_best,4]

spam_tree$cptable
# 0.3616830 with standard deviation 
spam_tree$cptable[index_best,5]
# 0.020175134. So, we want the smallest tree with xerror less than  0.3616830+0.020175134
# = 0.3818581
#so we want to prune our tree with a cp slightly greater than than 0.2743.

spam_tree <- prune(spam_tree,cp=0.3818581)
rpart.plot(Boston_tree)
#Ne fonctionne pas, CP est trop grand donc le modèle ne classe rien

# Bagging Random Forests, and Boosting Models # 

#Random Forest
spam_forest=randomForest(spam~.,data=spam_train,mtry=19,importance=TRUE,ntrees=500)
spam_predicted_forest=predict(spam_forest,newdata = spam_test)
rmse(spam_predicted_forest,dataset$spam)
#Bagging 

spam_bagging= randomForest(spam~.,data=spam_train,mtry=57,importance=TRUE,ntrees=500) 
spam_predicted_bagging= predict(spam_bagging,newdata = spam_test)
rmse(spam_predicted_bagging,dataset$spam)

#Boosting 

spam_boosting = gbm(spam~., data = spam_train, distribution = "gaussian", 
                  n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
spam_boosting_predict= predict(spam_boosting,spam_test)
rmse(spam_boosting_predict,spam_test$spam)

#Accuracy

calc_acc(spam_predicted,dataset$spam)

calc_acc(spam_predicted_forest,dataset$spam)

calc_acc(spam_predicted_bagging,dataset$spam) 

calc_acc(spam_boosting_predict,dataset$spam)

# ConclusionBagging est le meilleurs modèle Accuracy 0.01





