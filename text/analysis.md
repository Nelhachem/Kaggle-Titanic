Titanic Kaggle
========================================================

Load both the train and test data
--------------------------------------


```r
rm(list = ls())
train <- read.csv(file = "../data/raw//train.csv", header = T, sep = ",")
test <- read.csv(file = "../data/raw//test.csv", header = T, sep = ",")
```


Initial Analysis
---------------------------

```r
summary(train)
```

```
##   PassengerId     Survived         Pclass    
##  Min.   :  1   Min.   :0.000   Min.   :1.00  
##  1st Qu.:224   1st Qu.:0.000   1st Qu.:2.00  
##  Median :446   Median :0.000   Median :3.00  
##  Mean   :446   Mean   :0.384   Mean   :2.31  
##  3rd Qu.:668   3rd Qu.:1.000   3rd Qu.:3.00  
##  Max.   :891   Max.   :1.000   Max.   :3.00  
##                                              
##                                     Name         Sex           Age       
##  Abbing, Mr. Anthony                  :  1   female:314   Min.   : 0.42  
##  Abbott, Mr. Rossmore Edward          :  1   male  :577   1st Qu.:20.12  
##  Abbott, Mrs. Stanton (Rosa Hunt)     :  1                Median :28.00  
##  Abelson, Mr. Samuel                  :  1                Mean   :29.70  
##  Abelson, Mrs. Samuel (Hannah Wizosky):  1                3rd Qu.:38.00  
##  Adahl, Mr. Mauritz Nils Martin       :  1                Max.   :80.00  
##  (Other)                              :885                NA's   :177    
##      SibSp           Parch            Ticket         Fare      
##  Min.   :0.000   Min.   :0.000   1601    :  7   Min.   :  0.0  
##  1st Qu.:0.000   1st Qu.:0.000   347082  :  7   1st Qu.:  7.9  
##  Median :0.000   Median :0.000   CA. 2343:  7   Median : 14.5  
##  Mean   :0.523   Mean   :0.382   3101295 :  6   Mean   : 32.2  
##  3rd Qu.:1.000   3rd Qu.:0.000   347088  :  6   3rd Qu.: 31.0  
##  Max.   :8.000   Max.   :6.000   CA 2144 :  6   Max.   :512.3  
##                                  (Other) :852                  
##          Cabin     Embarked
##             :687    :  2   
##  B96 B98    :  4   C:168   
##  C23 C25 C27:  4   Q: 77   
##  G6         :  4   S:644   
##  C22 C26    :  3           
##  D          :  3           
##  (Other)    :186
```

```r
summary(test)
```

```
##   PassengerId       Pclass    
##  Min.   : 892   Min.   :1.00  
##  1st Qu.: 996   1st Qu.:1.00  
##  Median :1100   Median :3.00  
##  Mean   :1100   Mean   :2.27  
##  3rd Qu.:1205   3rd Qu.:3.00  
##  Max.   :1309   Max.   :3.00  
##                               
##                                         Name         Sex     
##  Abbott, Master. Eugene Joseph            :  1   female:152  
##  Abelseth, Miss. Karen Marie              :  1   male  :266  
##  Abelseth, Mr. Olaus Jorgensen            :  1               
##  Abrahamsson, Mr. Abraham August Johannes :  1               
##  Abrahim, Mrs. Joseph (Sophie Halaut Easu):  1               
##  Aks, Master. Philip Frank                :  1               
##  (Other)                                  :412               
##       Age            SibSp           Parch            Ticket   
##  Min.   : 0.17   Min.   :0.000   Min.   :0.000   PC 17608:  5  
##  1st Qu.:21.00   1st Qu.:0.000   1st Qu.:0.000   113503  :  4  
##  Median :27.00   Median :0.000   Median :0.000   CA. 2343:  4  
##  Mean   :30.27   Mean   :0.447   Mean   :0.392   16966   :  3  
##  3rd Qu.:39.00   3rd Qu.:1.000   3rd Qu.:0.000   220845  :  3  
##  Max.   :76.00   Max.   :8.000   Max.   :9.000   347077  :  3  
##  NA's   :86                                      (Other) :396  
##       Fare                   Cabin     Embarked
##  Min.   :  0.0                  :327   C:102   
##  1st Qu.:  7.9   B57 B59 B63 B66:  3   Q: 46   
##  Median : 14.5   A34            :  2   S:270   
##  Mean   : 35.6   B45            :  2           
##  3rd Qu.: 31.5   C101           :  2           
##  Max.   :512.3   C116           :  2           
##  NA's   :1       (Other)        : 80
```

A few things worth noting based on the summary:

* The Survived variable should be a factor variable with two levels, but we can always use it as a factor using the "as.factor()" method.

* There are a few missing values in Age in both sets. We can fill them with different methods but whatever methods we use must only be based on the train data. The same works for the Fare variable.

* Embarked variable has two missing values in the train data. Since this is a very small portion, we can first fill them with the value "S" (the largest level).

* PClass should probably be a factor variable but there is possibly a ranking in the variable (first class, middle class, and third class). So in order to keep this info, we leave it as is (although this may not precisely represent the difference between two classes). We can think about ordinal encoding but the algorithms we are using in R may not accept ordinal variables.

* We should probably not include PassengerId, Ticket and Cabin in the modeling. But we can always check them later.

* Usually I would treat the Name variable the same as the three variables above. But according to the tutorial on Kaggle, the text in the Name may actually provide something useful. So We can keep it now.

Preprocessing
--------------------------
### Filling the missing values
There are a few different ways for filling missing values. We will start with the simplest one -- fill all missing value with the median value in the train data.


```r
train.processed <- train
test.processed <- test

# filling the missing value in the Age variable
train.processed$Age[is.na(train.processed$Age)] <- median(train.processed$Age, 
    na.rm = T)
test.processed$Age[is.na(test.processed$Age)] <- median(train.processed$Age, 
    na.rm = T)

# filling the missing value in the Fare variable in test data
test.processed$Fare[is.na(test.processed$Fare)] <- median(train.processed$Fare, 
    na.rm = T)

# fix the missing values in the Embarked variable this may be a bit tricky.
# In order to keep the same level order in the variable we have to merge the
# train and test data together first
test.processed$Survived <- NA
combined <- rbind(train.processed, test.processed)
combined$Embarked[combined$Embarked == ""] <- "S"
combined$Embarked <- factor(combined$Embarked)

# split back to train and test data
train.processed <- combined[1:891, ]
test.processed <- combined[-c(1:891), ]

save(train.processed, file = "../data/processed/train.processed.v1.RData")
save(test.processed, file = "../data/processed/test.processed.v1.RData")
```


After this simple preprocessing, let's take a look the summary.

```r
summary(train.processed)
```

```
##   PassengerId     Survived         Pclass    
##  Min.   :  1   Min.   :0.000   Min.   :1.00  
##  1st Qu.:224   1st Qu.:0.000   1st Qu.:2.00  
##  Median :446   Median :0.000   Median :3.00  
##  Mean   :446   Mean   :0.384   Mean   :2.31  
##  3rd Qu.:668   3rd Qu.:1.000   3rd Qu.:3.00  
##  Max.   :891   Max.   :1.000   Max.   :3.00  
##                                              
##                                     Name         Sex           Age       
##  Abbing, Mr. Anthony                  :  1   female:314   Min.   : 0.42  
##  Abbott, Mr. Rossmore Edward          :  1   male  :577   1st Qu.:22.00  
##  Abbott, Mrs. Stanton (Rosa Hunt)     :  1                Median :28.00  
##  Abelson, Mr. Samuel                  :  1                Mean   :29.36  
##  Abelson, Mrs. Samuel (Hannah Wizosky):  1                3rd Qu.:35.00  
##  Adahl, Mr. Mauritz Nils Martin       :  1                Max.   :80.00  
##  (Other)                              :885                               
##      SibSp           Parch            Ticket         Fare      
##  Min.   :0.000   Min.   :0.000   1601    :  7   Min.   :  0.0  
##  1st Qu.:0.000   1st Qu.:0.000   347082  :  7   1st Qu.:  7.9  
##  Median :0.000   Median :0.000   CA. 2343:  7   Median : 14.5  
##  Mean   :0.523   Mean   :0.382   3101295 :  6   Mean   : 32.2  
##  3rd Qu.:1.000   3rd Qu.:0.000   347088  :  6   3rd Qu.: 31.0  
##  Max.   :8.000   Max.   :6.000   CA 2144 :  6   Max.   :512.3  
##                                  (Other) :852                  
##          Cabin     Embarked
##             :687   C:168   
##  B96 B98    :  4   Q: 77   
##  C23 C25 C27:  4   S:646   
##  G6         :  4           
##  C22 C26    :  3           
##  D          :  3           
##  (Other)    :186
```

```r
summary(test.processed)
```

```
##   PassengerId      Survived       Pclass    
##  Min.   : 892   Min.   : NA   Min.   :1.00  
##  1st Qu.: 996   1st Qu.: NA   1st Qu.:1.00  
##  Median :1100   Median : NA   Median :3.00  
##  Mean   :1100   Mean   :NaN   Mean   :2.27  
##  3rd Qu.:1205   3rd Qu.: NA   3rd Qu.:3.00  
##  Max.   :1309   Max.   : NA   Max.   :3.00  
##                 NA's   :418                 
##                                        Name         Sex     
##  Connolly, Miss. Kate                    :  1   female:152  
##  Kelly, Mr. James                        :  1   male  :266  
##  Abbott, Master. Eugene Joseph           :  1               
##  Abelseth, Miss. Karen Marie             :  1               
##  Abelseth, Mr. Olaus Jorgensen           :  1               
##  Abrahamsson, Mr. Abraham August Johannes:  1               
##  (Other)                                 :412               
##       Age            SibSp           Parch            Ticket   
##  Min.   : 0.17   Min.   :0.000   Min.   :0.000   PC 17608:  5  
##  1st Qu.:23.00   1st Qu.:0.000   1st Qu.:0.000   113503  :  4  
##  Median :28.00   Median :0.000   Median :0.000   CA. 2343:  4  
##  Mean   :29.81   Mean   :0.447   Mean   :0.392   16966   :  3  
##  3rd Qu.:35.75   3rd Qu.:1.000   3rd Qu.:0.000   220845  :  3  
##  Max.   :76.00   Max.   :8.000   Max.   :9.000   347077  :  3  
##                                                  (Other) :396  
##       Fare                   Cabin     Embarked
##  Min.   :  0.0                  :327   C:102   
##  1st Qu.:  7.9   B57 B59 B63 B66:  3   Q: 46   
##  Median : 14.5   A34            :  2   S:270   
##  Mean   : 35.6   C101           :  2           
##  3rd Qu.: 31.5   C23 C25 C27    :  2           
##  Max.   :512.3   C78            :  2           
##                  (Other)        : 80
```

There are no more missing values except for the Survived variable in the test, which is the target we are going to predict.

Prediction
-----------------------------
### Random Forest
If you haven't install the randomForest package, install it first.

```r
# install.packages('randomForest')
require(randomForest)
```

```
## Loading required package: randomForest
```

```
## Warning: package 'randomForest' was built under R version 3.0.3
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
set.seed(101)

fit <- randomForest(as.factor(Survived) ~ . - PassengerId - Name - Ticket - 
    Cabin, data = train.processed, ntree = 2000, importance = T)
```

Let's take a look at the variable importance.

```r
varImpPlot(fit)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

It seems that the variable Sex is dominating. This may not be good but let's see the prediction of this model first.

```r
test.processed$Survived <- NULL
Prediction <- predict(fit, test.processed)
submit <- data.frame(PassengerId = test.processed$PassengerId, Survived = Prediction)
write.csv(submit, file = "../results/res.randomforest.v1.csv", row.names = FALSE)
```

Now this submission on April 12th, 2014 achieved 77.512% accuracy, ranking 803rd/1264. Good job submitting the first result. We have a lot space for improvement. Keep going!

Analysis on April 13, 2014
--------------------------------
Yesterday we used the variable median to fill the missing values in the data. What if we use another way to do that instead?

We use the same method in the tutorial--the rpart with anova method.

```r
# we set the value of original empty cells in Age back to NA
train.processed$Age[is.na(train$Age)] <- NA
test.processed$Age[is.na(test$Age)] <- NA

# install.package('rpart')
require(rpart)
```

```
## Loading required package: rpart
```

```
## Warning: package 'rpart' was built under R version 3.0.3
```

```r
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = train.processed[!is.na(train.processed$Age), 
    ], method = "anova")

train.processed$Age[is.na(train.processed$Age)] <- predict(Agefit, train.processed[is.na(train.processed$Age), 
    ])
test.processed$Age[is.na(test.processed$Age)] <- predict(Agefit, test.processed[is.na(test.processed$Age), 
    ])

save(train.processed, file = "../data/processed/train.processed.v2.RData")
save(test.processed, file = "../data/processed/test.processed.v2.RData")
```

Prediction
-----------------------------
### Random Forest
If you haven't install the randomForest package, install it first.

```r
# install.packages('randomForest')
require(randomForest)
set.seed(101)

fit <- randomForest(as.factor(Survived) ~ . - PassengerId - Name - Ticket - 
    Cabin, data = train.processed, ntree = 2000, importance = T)
```

Let's take a look at the variable importance.

```r
varImpPlot(fit)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

It seems that the variable Sex is dominating. This may not be good but let's see the prediction of this model first.

```r
test.processed$Survived <- NULL
Prediction <- predict(fit, test.processed)
submit <- data.frame(PassengerId = test.processed$PassengerId, Survived = Prediction)
write.csv(submit, file = "../results/res.randomforest.v2.csv", row.names = FALSE)
```

Now this submission on April 13th, 2014 achieved 77.990% accuracy, ranking 741st/1258. Good job submitting the first result. We have a lot space for improvement. Keep going!

Next Step
-------------------------
Instead of adding more features like what the tutorial does, go back and try out what we have learned in the courses: 
* Learning Curves
* Start with simple algorithms like logistic regression
* Based on the learning curve, think about ways for improvement like adding more variables, polynomial terms, or less variables.
