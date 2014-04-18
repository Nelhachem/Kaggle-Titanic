rm(list=ls())

require(boot)

load("data//processed//train.processed.v1.RData")

train.processed$Survived <- as.factor(train.processed$Survived)
temp <- which(names(train.processed) %in% c("PassengerId", "Name","Ticket","Cabin"))
train.processed <- train.processed[,-temp]
# ==================== split the data to train and validation ===========
set.seed(101)
rows <- nrow(train.processed)

# ================= build the learning curves =============
num.of.train.samples <- seq(from=100, to=nrow(train.processed), by=5)
train.matrix <- matrix(0,nrow=length(num.of.train.samples), ncol=20)
val.matrix <- matrix(0,nrow=length(num.of.train.samples), ncol=20)

cost <- function(labels,pred){
  mean(labels!=ifelse(pred > 0.5, 1, 0))
}

require(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)

print(date())
ptime <- system.time({
  r <- foreach(i=c(1:length(num.of.train.samples)), .export=c("glm","cv.glm")) %dopar% {
    res <- lapply(c(1:20), function(x, i){
      selected <- sample(c(1:nrow(train.processed)), num.of.train.samples[i])
      fit <- glm(Survived ~ ., family = binomial, data = train.processed[selected,])
      trainRes <- ifelse(predict(fit, type="response")>0.5, "1", "0")
      train.error <- mean(trainRes != train.processed[selected,]$Survived)
      cv.err <- cv.glm(train.processed[selected,], fit, cost, 10)$delta[1]
      c(train.error, cv.err)
    }, i)
    temp <- matrix(unlist(res), ncol=2, byrow=T)
    iErr <- apply(temp, 2, mean)
  }
})
stopCluster(cl)
print(date())

r <- matrix(unlist(r), ncol=2, byrow=T)

errors <- data.frame(sample.size=num.of.train.samples, train=r[,1], val=r[,2])

require(ggplot2)
p <- ggplot(data=errors) 
p + geom_line(aes(x=sample.size, y=train,colour="#000099")) + geom_point(aes(x=sample.size, y=val))
# ================= dirty test of the model ===============
glm.fit <- glm(Survived ~ ., family = binomial, data = train.processed)
load("data//processed//test.processed.v1.RData")
PassengerId <- test.processed$PassengerId
test.processed$Survived <- as.factor(test.processed$Survived)
temp <- which(names(test.processed) %in% c("PassengerId", "Name","Ticket","Cabin"))
test.processed <- test.processed[,-temp]
testRes <- ifelse(predict(glm.fit, newdata=test.processed, type="response")>0.5,
                  "1", "0")
names(testRes) <- NULL
submit <- data.frame(PassengerId = PassengerId, Survived = testRes)
write.csv(submit, file = "./results/res.GLM.v1.csv", row.names = FALSE)
