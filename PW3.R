#Question1
dataset <- read.csv('Social_Network_Ads.csv')

View(dataset)

#Question2
str(dataset)

summary(dataset)

#Question3 

library(caTools) # install it first in the console
set.seed(123)
# we use the function set.seed() with the same seed number
# to randomly generate the same values, you already know that right? 
#and you know why we want to generate the same values, am I wrong? 
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
# here we chose the SplitRatio to 75% of the dataset,
# and 25% for the test set.
training_set = subset(dataset, split == TRUE)
# we use subset to split the dataset
test_set = subset(dataset, split == FALSE)

#Question4

training_set[c(3,4)]=scale(training_set[c(3,4)])
test_set[c(3,4)]=scale(test_set[c(3,4)])


#Question5 

model <- glm(Purchased ~ Age , family = binomial, data = training_set)

#Question6 

summary(model)

#AIC remplace le R carré 

#question7


#question8 

#Oui car la p-value est tres petite 
#la p-valeur est un le résultat d'un test statstique 

#Question9 

plot(training_set$Age,training_set$Purchased)
curve(predict(model,data.frame(Age=x),type='response'),add=TRUE)

library(ggplot2)

ggplot(training_set,aes(x=Age,y=Purchased)) + geom_point() + stat_smooth(method='glm', method.args=list(family='binomial'),se=FALSE)

#Question11

model1 <- glm(Purchased ~ Age + EstimatedSalary, family = binomial, data = training_set)
summary(model1)


