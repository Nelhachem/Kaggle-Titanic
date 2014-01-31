rm(list=ls())

setwd("./data")
###################### loading the train and test data ######################
train <- read.csv("../data/train.csv", header=TRUE)
test <- read.csv("../data/test.csv", header=TRUE)

###################### Sampling on the training data #######################
# As we tried earlier and people posted on the forum, using the full train data to do the classification could take up to 6 hours.
# One suggestion is that to do sampling on the training data and select the Model on the sampled data. Once selected, use the full
# data to get the final hypothesis.
library(sampling)

# the strata suggests sorting the data before sampling. And in our case, the order of the data does not matter
train <- train[order(train$label),]

# not sure how much we should use to do the modeling so let us try half first.
sampled <- strata(train,c("label"),size=as.vector(ceiling(0.5 * table(train$label))), method="srswor")

sampled.data <- train[sampled$ID_unit,]

# then we need to separate the sampled.train into two parts: one for real training and one for validation.
sampled.data <- sampled.data[order(sampled.data$label),]
sampled.train.id <- strata(sampled.data,c("label"),size=as.vector(ceiling(0.8 * table(sampled.data$label))), method="srswor")

sampled.train <- sampled.data[sampled.train.id$ID_unit,]
sampled.validation <- sampled.data[-1*sampled.train.id$ID_unit,]

rm(sampled)
rm(sampled.train.id)
rm(sampled.data)

labels <- sampled.train[,1]
sampled.train <- sampled.train[,-1]

validation.labels <- sampled.validation[,1]
sampled.validation <- sampled.validation[,-1]

system.time(results <- (0:9)[knn(sampled.train, sampled.validation, labels, k = 10, algorithm="cover_tree")])

error.rate <- length(which(results != validation.labels))/nrow(sampled.validation)

##################### try different number of neigbors ###########################
# parameter tuning.
require(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)

Sys.time()
error.rates <- foreach(num.of.neigbors=1:20, .export=c("knn")) %dopar% {
  results <- (0:9)[knn(sampled.train, sampled.validation, labels, k = num.of.neigbors, algorithm="cover_tree")]
  error.rate <- length(which(results != validation.labels))/nrow(sampled.validation)
}
Sys.time()
stopCluster(cl)
# the above experiment took about 26 minutes with parallel computing
