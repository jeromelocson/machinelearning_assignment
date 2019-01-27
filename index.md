Prediction Assignment 
========================================================

This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **Help** toolbar button for more details on using R Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
library(caret)
library(rpart)
library(randomForest)
# library(rpart.plot)
#library(RColorBrewer)
# library(rattle)

# library(gbm)
# library(plyr)
```

# Data Exploration and Cleaning


```r
# Load the training dataset
training_dataset <- read.csv("data/pml-training.csv", na.strings=c("NA","#DIV/0!",""))
# Load the testing dataset
testing_dataset <- read.csv("data/pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

dim(training_dataset)
```

```
## [1] 19622   160
```

```r
dim(testing_dataset)
```

```
## [1]  20 160
```

```r
table(training_dataset$classe)
```

```
## 
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```

```r
set.seed(1983)
independent_training  <- createDataPartition(training_dataset$classe, p=0.7, list=FALSE)
training_set <- training_dataset[ independent_training, ]
testing_set  <- training_dataset[-independent_training, ]

dim(training_set)
```

```
## [1] 13737   160
```

```r
dim(testing_set)
```

```
## [1] 5885  160
```

```r
cleaned_trainingset <- nearZeroVar(training_set)
training_set <- training_set[ , -cleaned_trainingset]
testing_set  <- testing_set[ , -cleaned_trainingset]

dim(training_set)
```

```
## [1] 13737   132
```

```r
dim(testing_set)
```

```
## [1] 5885  132
```

```r
na_var <- sapply(training_set, function(x) mean(is.na(x))) > 0.95
training_set <- training_set[ , na_var == FALSE]
testing_set  <- testing_set [ , na_var == FALSE]

dim(training_set)
```

```
## [1] 13737    59
```

```r
dim(testing_set)
```

```
## [1] 5885   59
```

```r
str(training_set)
```

```
## 'data.frame':	13737 obs. of  59 variables:
##  $ X                   : int  1 2 3 4 5 7 8 9 10 11 ...
##  $ user_name           : Factor w/ 6 levels "adelmo","carlitos",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ raw_timestamp_part_1: int  1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
##  $ raw_timestamp_part_2: int  788290 808298 820366 120339 196328 368296 440390 484323 484434 500302 ...
##  $ cvtd_timestamp      : Factor w/ 20 levels "02/12/2011 13:32",..: 9 9 9 9 9 9 9 9 9 9 ...
##  $ num_window          : int  11 11 11 12 12 12 12 12 12 12 ...
##  $ roll_belt           : num  1.41 1.41 1.42 1.48 1.48 1.42 1.42 1.43 1.45 1.45 ...
##  $ pitch_belt          : num  8.07 8.07 8.07 8.05 8.07 8.09 8.13 8.16 8.17 8.18 ...
##  $ yaw_belt            : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt    : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ gyros_belt_x        : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.03 0.03 ...
##  $ gyros_belt_y        : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z        : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 0 -0.02 ...
##  $ accel_belt_x        : int  -21 -22 -20 -22 -21 -22 -22 -20 -21 -21 ...
##  $ accel_belt_y        : int  4 4 5 3 2 3 4 2 4 2 ...
##  $ accel_belt_z        : int  22 22 23 21 24 21 21 24 22 23 ...
##  $ magnet_belt_x       : int  -3 -7 -2 -6 -6 -4 -2 1 -3 -5 ...
##  $ magnet_belt_y       : int  599 608 600 604 600 599 603 602 609 596 ...
##  $ magnet_belt_z       : int  -313 -311 -305 -310 -302 -311 -313 -312 -308 -317 ...
##  $ roll_arm            : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm           : num  22.5 22.5 22.5 22.1 22.1 21.9 21.8 21.7 21.6 21.5 ...
##  $ yaw_arm             : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm     : int  34 34 34 34 34 34 34 34 34 34 ...
##  $ gyros_arm_x         : num  0 0.02 0.02 0.02 0 0 0.02 0.02 0.02 0.02 ...
##  $ gyros_arm_y         : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 -0.03 ...
##  $ gyros_arm_z         : num  -0.02 -0.02 -0.02 0.02 0 0 0 -0.02 -0.02 0 ...
##  $ accel_arm_x         : int  -288 -290 -289 -289 -289 -289 -289 -288 -288 -290 ...
##  $ accel_arm_y         : int  109 110 110 111 111 111 111 109 110 110 ...
##  $ accel_arm_z         : int  -123 -125 -126 -123 -123 -125 -124 -122 -124 -123 ...
##  $ magnet_arm_x        : int  -368 -369 -368 -372 -374 -373 -372 -369 -376 -366 ...
##  $ magnet_arm_y        : int  337 337 344 344 337 336 338 341 334 339 ...
##  $ magnet_arm_z        : int  516 513 513 512 506 509 510 518 516 509 ...
##  $ roll_dumbbell       : num  13.1 13.1 12.9 13.4 13.4 ...
##  $ pitch_dumbbell      : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
##  $ yaw_dumbbell        : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
##  $ total_accel_dumbbell: int  37 37 37 37 37 37 37 37 37 37 ...
##  $ gyros_dumbbell_x    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ gyros_dumbbell_y    : num  -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 ...
##  $ gyros_dumbbell_z    : num  0 0 0 -0.02 0 0 0 0 0 0 ...
##  $ accel_dumbbell_x    : int  -234 -233 -232 -232 -233 -232 -234 -232 -235 -233 ...
##  $ accel_dumbbell_y    : int  47 47 46 48 48 47 46 47 48 47 ...
##  $ accel_dumbbell_z    : int  -271 -269 -270 -269 -270 -270 -272 -269 -270 -269 ...
##  $ magnet_dumbbell_x   : int  -559 -555 -561 -552 -554 -551 -555 -549 -558 -564 ...
##  $ magnet_dumbbell_y   : int  293 296 298 303 292 295 300 292 291 299 ...
##  $ magnet_dumbbell_z   : num  -65 -64 -63 -60 -68 -70 -74 -65 -69 -64 ...
##  $ roll_forearm        : num  28.4 28.3 28.3 28.1 28 27.9 27.8 27.7 27.7 27.6 ...
##  $ pitch_forearm       : num  -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.8 -63.8 -63.8 -63.8 ...
##  $ yaw_forearm         : num  -153 -153 -152 -152 -152 -152 -152 -152 -152 -152 ...
##  $ total_accel_forearm : int  36 36 36 36 36 36 36 36 36 36 ...
##  $ gyros_forearm_x     : num  0.03 0.02 0.03 0.02 0.02 0.02 0.02 0.03 0.02 0.02 ...
##  $ gyros_forearm_y     : num  0 0 -0.02 -0.02 0 0 -0.02 0 0 -0.02 ...
##  $ gyros_forearm_z     : num  -0.02 -0.02 0 0 -0.02 -0.02 0 -0.02 -0.02 -0.02 ...
##  $ accel_forearm_x     : int  192 192 196 189 189 195 193 193 190 193 ...
##  $ accel_forearm_y     : int  203 203 204 206 206 205 205 204 205 205 ...
##  $ accel_forearm_z     : int  -215 -216 -213 -214 -214 -215 -213 -214 -215 -214 ...
##  $ magnet_forearm_x    : int  -17 -18 -18 -16 -17 -18 -9 -16 -22 -17 ...
##  $ magnet_forearm_y    : num  654 661 658 658 655 659 660 653 656 657 ...
##  $ magnet_forearm_z    : num  476 473 469 469 473 470 474 476 473 465 ...
##  $ classe              : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
training_set <- training_set[ , -(1:6)]
testing_set  <- testing_set [ , -(1:6)]

dim(training_set)
```

```
## [1] 13737    53
```

```r
dim(testing_set)
```

```
## [1] 5885   53
```

# Random Forest Model


```r
set.seed(2019)
rf <- randomForest(classe~., data=training_set)
rf
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = training_set) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.5%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3903    3    0    0    0 0.0007680492
## B   11 2643    4    0    0 0.0056433409
## C    0   20 2376    0    0 0.0083472454
## D    0    0   22 2228    2 0.0106571936
## E    0    1    1    5 2518 0.0027722772
```

```r
predict1 <- predict(rf, training_set)
head(predict1)
```

```
## 1 2 3 4 5 7 
## A A A A A A 
## Levels: A B C D E
```

```r
head(training_set$classe)
```

```
## [1] A A A A A A
## Levels: A B C D E
```

```r
confusionMatrix(predict1, training_set$classe)
```

```
## Error: package e1071 is required
```
