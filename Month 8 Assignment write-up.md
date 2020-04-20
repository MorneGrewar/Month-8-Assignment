---
title: "Month 8 Assignment write-up"
author: "Morne Grewar"
date: "20/04/2020"
output:
  html_document: default
  pdf_document: default
---

# Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

### Load all required packages,data and set working directory

```{r setup}
library(caret)
library(randomForest)
setwd('C:/Users/Public/Documents/Coursera')
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', 'training.csv')
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv', 'testing.csv')
testing <- read.csv('testing.csv')
training <- read.csv('training.csv')
```


### Some data exploration and analysis

Looking at the structure of the training dataset, thre are quite a few variables which might be useless, these can be removed. The first 5 columns (X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp) have no influence on the classe output and can thus be removed. There is also significant NA values under some variables.Variables with high amount of NA-values can be removed as these will most likely be useless predictors. It is important to clean-up the data before hand to limit the possibilities of incorporating unnecessary information in to the model computations and extending processing time. It is also important to note that both the training and testing datasets be cleaned in the same manor.
After removing all the "useless" variables there are 88 variables left, this is still quite a large amount of variables

```{r UselessVariables,echo=F}
str(training)
training <- training[ ,-(1:5)]
testing <- testing[ ,-(1:5)]

NAvars <- sapply(training, function(x) mean(is.na(x))) > 0.9
training <- training[ , NAvars==F]
testing<- testing[ , NAvars==F]

```

```{r Dim1}
dim(training)
dim(testing)

```

An extra method which can be used to remove insignificant variables, is to use the nearZeroVar function to identify all the variables which show very little varability and can therefore be seen as irrelevant to predicting the outcome. After applying this tool we are left with 54 variables, which is must more efficient when compared to the 159 variables started with in the beginning. 

```{r NearZeroVar,echo=F}
zerovar <- nearZeroVar(training)
training <- training[ , -zerovar]
testing <- testing[ , -zerovar]
```

```{r Dim2}
dim(training)
dim(testing)

```

### Model construction and testing

Now that the data has been tidied up, the prediction model can be constructed. The first step is to split the original training dataset into two group in order to construct the model and to test it before it is applied to the final testing dataset i.e. validation set. The second step is to construct the prediction model, in this case a Random Forrest approach will be used as it should give a fairly accurate model. Some training cpntrols will also be applied in the form of a 3-fold cross validation

```{r DataSplit,echo=F}
set.seed(1)
trainsplit <- createDataPartition(y=training$classe,p=0.7,list=F)
trainset <- training[trainsplit, ]
testset <- training[-trainsplit, ]

rfmodel <- train(classe~.,data=trainset,method="rf",trControl=trainControl(method="cv", number=3, verboseIter=F))
```

```{r Model}
rfmodel$finalModel
```

From the finalModel information, it can be seen that the model tried 27 Variables at each split over 500 trees and the estimated error rate is 0.23%
This seems like a good error rate, but testing the model on the testset will give a true error rate. 


```{r ModelTest}
predict <- predict(rfmodel,testset)
confusionMatrix(testset$classe,predict)$overall
```

From the confusion matrix info, we see the overall accuracy is 99.7%, i.e. the out of sample error rate is very low. This is an acceptable result to warn no further model tests.

### Running model on given test set to predict classe for each row

```{r Final}
predictFinal <- predict(rfmodel,testing)
pml_write_files <- function(x) {
    n <- length(x)
    for(i in 1:n) {
        filename <- paste0("Test_id_", i, ".txt")
        write.table(x[i], file=filename, quote=F, row.names=F, col.names=F)
    }
}

# create prediction files to submit
pml_write_files(predictFinal)

predictFinal
```



