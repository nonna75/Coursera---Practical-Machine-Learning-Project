---
title: "Practical Machine Learning Project"
output: pdf_document
---

## Introduction


Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways

## Loading and preprocessing the data

```{r echo=TRUE }
library(rpart)
library(caret)
library(rpart.plot)
library(rattle)
library(randomForest)



fileTraing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileTraing, destfile = paste0(getwd(), '/pml-training.csv'), method = "curl")

fileTesting <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileTesting, destfile = paste0(getwd(), '/pml-testing.csv'), method = "curl")
```

## Reading csv Data into Data.Table. 
```{r echo=TRUE}
traingSet <- read.csv("pml-training.csv")
testSet <- read.csv("pml-testing.csv")
```


### A summary of Data (traingSet)

```{r echo=TRUE, cache=TRUE}
str(traingSet)
colSums(is.na(traingSet))

```

The training data set is made of 19622 observations on 160 columns. We can notice that many columns have NA values or blank values on almost every observation. So we will remove them.

## Cleaning the input data

```{r echo=TRUE}
traingSet <- traingSet[, colSums(is.na(traingSet))==0]
testSet <- testSet[, colSums(is.na(testSet))==0]

```
also we wil remove first sever colmns which contain information for people who did the test, and also timestamps

```{r echo=TRUE}

traingSet <- traingSet[, -c(1:7)]
testSet <- testSet[, -c(1:7)]

```

## Preparing the datasets for prediction. 

```{r echo=TRUE}

set.seed(1234)
inTrain <- createDataPartition(traingSet$classe,p =0.7,list = FALSE)
trainData <- traingSet[inTrain, ]
testData <- traingSet[-inTrain, ]

```
    ##    Mean_Steps Median_Steps
    ## 1:   10766.19        10765


## Cleaning the variables that are near-zero-variance
```{r echo=TRUE}

near_zero <- nearZeroVar(testData)
trainData <- trainData[,-near_zero]
testData <- testData[,-near_zero]
```

## Prediction with classification trees
```{r echo=TRUE}
set.seed(12345)

DessTree <-rpart(classe ~ ., data=trainData,method="class")
fancyRpartPlot(DessTree)

```

## Prediction on Test dataset
```{r echo=TRUE}
predTreeDess <- predict(DessTree,newdata = testData,type="class")
cofMatTreeDess <- confusionMatrix(predTreeDess,testData$classe)
cofMatTreeDess
```
We see that the accuracy rate of the model is low: 0.6967.

## Train with random forests

```{r echo=TRUE}
tcontrol <- trainControl(method = "cv", number = 5)
model_RF <- train(classe ~., data = trainData, method = "rf", trControl = tcontrol)
model_RF$finalModel

```

```{r echo=TRUE}
predict_rf <- predict(model_RF,newdata = testData)
confMatrix_rf <- confusionMatrix(predict_rf,testData$classe)
confMatrix_rf

```

With random forest, we reach an accuracy of 99.3% using cross-validation with 5 steps

### let's plot the model 
```{r echo=TRUE}
plot(model_RF)

```

## Train with gradient boosting method

```{r echo=TRUE}
# total number of steps taken per day

model_GBM <- train(classe~., data=trainData, method="gbm", trControl=tcontrol, verbose=FALSE)
print(model_GBM)
```

let's plot the model 

```{r echo=TRUE}

plot(model_GBM)

```


```{r echo=TRUE}
train_GBM <- predict(model_GBM,newdata=testData)

confMatGBM <- confusionMatrix(testData$classe,train_GBM)
confMatGBM

```

## Applying the best model to the validation data
The accuracy of the 3 regression modeling methods above are:

Random Forest : 0.9942

Decision Tree : 0.6879

GBM : 0.9633

In that case, the Random Forest model will be applied to predict  (testing dataset) :

```{r echo=TRUE}
predictTEST <- predict(model_RF, newdata=testSet)
predictTEST

```
