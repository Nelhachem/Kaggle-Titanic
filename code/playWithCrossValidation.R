rm(list=ls())
library(rpart)
library(cvTools)
data(iris)

# via model fitting function
# The first parameter could be a model name, then you need to specify the formula of using this model to do prediction
cv <- cvFit(rpart, formula=Species~., data=iris, cost=function(y, yHat) (y != yHat) + 0, predictArgs=c(type='class'))

# via model fit (then we must provide y in the parameter)
# fit an rpart model
fit <- rpart(formula=Species~., data=iris)
cv <- cvFit(fit, data=iris, y = iris$Species, cost=function(y, yHat) (y != yHat) + 0, predictArgs=c(type='class'))

# via function call (then we must provide y in the parameter)
# set up function call
call <- call("rpart", formula = Species ~ .)
cv <- cvFit(call, data=iris, y = iris$Species, cost=function(y, yHat) (y != yHat) + 0, predictArgs=c(type='class'))