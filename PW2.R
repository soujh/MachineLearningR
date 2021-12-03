#importing the package mass

library(MASS, lib.loc = "/Library/Frameworks/R.framework/Versions/4.0/Resources/library")
head(Boston)

#Split the data 

train=1:400 #training set 
test=-train #test set 

 

training_data=Boston[train,]
test_data=Boston[test,]

#check the dimension of the new dataset
dim(training_data)


cor(training_data$medv,training_data$age)

model=lm(medv ~ age, data =training_data)


#plot the model

plot(training_data$age,training_data$medv,
xlab="Age",
ylab="Prix",
col="red",pch=20)


abline(model,col="blue",lwd=3)

#new model 

model2 <- lm(medv ~ log(lstat)+age,data=training_data)
model2


rgl::plot3d(log(Boston$lstat),
            Boston$age,
            Boston$medv, type = "p",
            xlab = "log(lstat)",
            ylab = "age",
            zlab = "medv", site = 5, lwd = 15)


rgl::planes3d(model2$coefficients["log(lstat)"],
              model2$coefficients["age"], -1,
              model2$coefficients["(Intercept)"], alpha = 0.3, front = "line")

summary(model2)

#the predictors are sefingicnt beacause the p-values are small


model3 <- lm(medv~.,data=training_data)
summary((model3))

model4 <- lm(medv~ . -lstat+log(lstat),data=training_data)
summary(model4)

round(cor(training_data),2)
library(corrplot)

corrplot.mixed((cor(training_data)))


#Question16 

model5 <- lm(medv~ . -tax-lstat+log(lstat),data=training_data)


y=test_data$medv

y_hat=predict(model5,data.frame(test_data))

error=y-y_hat
error_squared=error^2

MSE=mean(error_squared)

MSE


####ANOVA###

table(training_data$chas)

boxplot(medv~chas,data=training_data)

aggregate(formula=medv~chas,data=training_data,FUN = mean)

anovatest<-aov(medv~chas,data=training_data)
anovatest

summary(anovatest)

model6 <- lm(medv~chas+crim,data=training_data)
summary(model6)

model23 <- lm(medv~lstat*age,data=training_data)
summary((model23))
