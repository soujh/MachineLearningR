head(EU)

# lm(formula = response (Y) ~ predictor (X), data = data)


mod <- lm(formula = Seats2011 ~ Population2010, data = EU)
mod
names(mod)

mod$coefficients

# mod is indeed a list of objects whose names are
# We can access these elements by $

sumMod <- summary(mod)
sumMod

# The residuals : distance par rapport à la realité 

# The fitted value : valeur trouver par le model
#la p-value msure si le resultat est le simple fruit du hasard
# si la p-value est petite  alors on rejette l'hypothèse que le resultat est le simple fruitdu hasard 

#1.9.2

library(MASS, lib.loc = "/Library/Frameworks/R.framework/Versions/4.0/Resources/library")

dim(Boston)
head(Boston)


# Split the data by using the first 400 observations as the training
# data and the remaining as the testing data
train = 1:400
test = -train

# Speficy that we are going to use only two variables (lstat and medv)
variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]

# Check the dimensions of the new dataset
dim(training_data)
#ans> [1] 400   2

plot(training_data$lstat, training_data$medv)

plot(log(training_data$lstat), training_data$medv)

model = lm(medv ~ log(lstat), data = training_data)
model
#ans> 
#ans> Call:
#ans> lm(formula = medv ~ log(lstat), data = training_data)
#ans> 
#ans> Coefficients:
#ans> (Intercept)   log(lstat)  
#ans>        51.8        -12.2

summary(model)