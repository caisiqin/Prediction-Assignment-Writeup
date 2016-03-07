---
title: "Prediction Assignment Writeup"
author: "Siqin Cai"
date: "March 4, 2016"
output: html_document
---

## Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: [http://groupware.les.inf.puc-rio.br/har] (see the section on the Weight Lifting Exercise Dataset).

## Load Data
Makde sure the data is available for loading and processing. If the files do not exist, download them from the original website.

* The training data for this project are available here:
[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv]
* The test data are available here:
[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv]

```{r,cache=TRUE}
if (!file.exists("pml-training.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                  destfile = "pml-training.csv")
}
if (!file.exists("pml-testing.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                  destfile = "pml-testing.csv")
}
```

## Pre-processing Data
Transform the data in order to create a tidy dataset
1.Remove the column if NA's appear more than 80%
2.Remove the column if it doesn't carry additional information

```{r,cache=TRUE}
# 1. Check to see if NA's appear more than 80%
checkNAs <- function(a){
  if(sum(is.na(a))/length(a)>0.8){
    result <- TRUE
  }else{
    result <- FALSE
  }
  invisible(result)
}

# 2. Check to see if there is no additional information
preProcessData <- function(dataset){
  options(warn=-1)
  # remove first 7 columns
  subsetTraining <- dataset[,-(1:7)] 
  
  # find class index
  classInd <- ncol(subsetTraining) 
  
  # convert everything to numeric except the class
  subsetTraining[,-classInd] <- data.frame(sapply(subsetTraining[,-classInd], as.numeric))
  
  # check to see which column contains 80% NAs
  col80NA <- sapply(subsetTraining, checkNAs)
  
  # remove these columns
  subsetTraining <- subsetTraining[,!col80NA]
  
  # remove those variables which do not contribute
  nzv <-nearZeroVar(subsetTraining[,-classInd],saveMetrics = TRUE)
  subsetTraining <- subsetTraining[,!as.logical(nzv$nzv)]
  
  # using a trained bagged tree to predict the remaining missing values
  if(any(is.na(subsetTraining))){
    predMis <- preProcess(subsetTraining[,-classInd],method = "bagImpute")
    subsetTraining[,-classInd]<-predict(predMis,subsetTraining[,classInd])
  }
  
  invisible(subsetTraining)
}
```

## Model
To build the model, use 75% of data as the training set.
```{r,cache=TRUE}
library(caret)
library(randomForest)
library(e1071)
set.seed(1988)

# read the csv file
training <- read.csv("pml-training.csv",header = TRUE,stringsAsFactors=FALSE)

# use 75% of the data as the subsetTraining
inTrain <- createDataPartition(training$classe,p=0.75,list = FALSE)
subsetTraining <-training[inTrain,]

# pre-process the data
TrainingSet <-preProcessData(subsetTraining)
```


Use random forest to fit the model
```{r,results='hide',cache=TRUE}
trainPar <- trainControl(allowParallel = TRUE, method = "cv", number = 5)
mod_rf <- train(classe~.,data=TrainingSet,
                method="rf",
                preProcess="pca", 
                trainControl = trainPar, 
                importance=TRUE)
```

## Cross-Validation
To evaluate the model, use 25% of data as the validation set.
```{r,cache=TRUE}
subsetTesting <-training[-inTrain,]
TestingSet <-preProcessData(subsetTesting)
confusionMatrix(TestingSet$classe, predict(mod_rf,TestingSet))
```

## Test set prediction
Finally, load the testing data file and predict the reult as the following:
```{r,cache=TRUE}
testingPred <- read.csv("pml-testing.csv",header = TRUE,stringsAsFactors=FALSE)
testingPred$classe <-1:nrow(testingPred)
testingPred<-preProcessData(testingPred)

predict(mod_rf,testingPred)
```

