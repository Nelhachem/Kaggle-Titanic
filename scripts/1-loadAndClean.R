# clean up the work space
rm(list=ls())

# loading both train and test data
train <- read.csv(file="data/raw//train.csv", header=T, sep=",")
test <- read.csv(file="data/raw//test.csv", header=T, sep=",")

