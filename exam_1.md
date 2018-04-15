
---
output:
  html_document:
    keep_md: yes
---


<style>
body {
text-align: justify}
</style>

#Predicting how well they do barbell lifts
##### by AJSS
## Summary
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).


##Data 
The training data for this project are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har.


## Reading and cleaning the data
In this program, is assumed that the data already has been downloaded and is in the same folder as this program 

First we are going to import the libraries

```r
library(caret)
```

Now we are going to read the data

```r
df_train = read.csv("pml-training.csv")
```

In this part the data is going to be cleaned according to the next points:

* Removing the row number 
* Remove all columns that have more than 50% of their cells as NA
* Remove all columns that have more than 50% of their cells as blank i.e ""
* Remove columns with no predicting power such as name and all the timestamps


```r
df_train$X=NULL

# remove all columns that have more than half of their values as NA
df_train2=df_train[, colSums(is.na(df_train)) < (nrow(df_train)/2)]

# remove all columns that have more than half of their values as empty string
df_train3=df_train2[, colSums(df_train2=="") < (nrow(df_train2)/2)]

df_train3 = df_train3[,-nearZeroVar(df_train3)]

unuseful_v = c("user_name","raw_timestamp_part_1",
                      "raw_timestamp_part_2", "cvtd_timestamp")

df_train3[,unuseful_v] = NULL
# use part of the data as test
```


## Training the model
The model that was chosen was random forest this was because of its robustness, accuracy, and capability of handling datasets with a lot of variables that are of different kinds. The training method uses 3-fold cross-validation with 3 repetitions this is done to mitigate the possible overfitting.For validating the model we split the training data set into a training and test dataset.



```r
train_cut =  createDataPartition(y=df_train$classe,
                               p=0.7, list=FALSE)
train_train = df_train3[train_cut,]
train_test = df_train3[-train_cut,]

model_rf = train(classe~ .,data=train_train,method="rf",
                 trControl=trainControl(method = "repeatedcv",
                                        number = 3, search="random",repeats = 2))


train_predictions = predict(model_rf, train_train)

cM_train = confusionMatrix(train_predictions, train_train$classe)

inSample_Error = 1-sum(train_predictions==train_train$classe)/length(train_train$classe)
```


The in sample error was 0 and the confusion matrix with the train data was 



```r
print(as.matrix(cM_train))
```

```
##      A    B    C    D    E
## A 3906    0    0    0    0
## B    0 2658    0    0    0
## C    0    0 2396    0    0
## D    0    0    0 2252    0
## E    0    0    0    0 2525
```


```r
test_predictions = predict(model_rf, train_test)

CM_test = confusionMatrix(test_predictions, train_test$classe)

OutSample_Error = 1-sum(test_predictions==train_test$classe)/length(train_test$classe)
```

The out of sample error was 0.0016992 and the confusion matrix with the test data was 

```r
print(as.matrix(CM_test))
```

```
##      A    B    C   D    E
## A 1674    4    0   0    0
## B    0 1135    2   0    0
## C    0    0 1024   0    0
## D    0    0    0 964    4
## E    0    0    0   0 1078
```

As we can see the out of sample error is very small and the confusion matrix looks good so we can conlude that the model is a good predictor.

##Validation
Now the model is going to be used in the test data set first the data set is going to be cleaned with the same procedure that was used in the training dataset, next we are going to predict the values of the test set


```r
df_test = read.csv("pml-testing.csv")

#clean_data

df_test$X=NULL

# remove all columns that have more than half of their values as NA
df_test2=df_test[, colSums(is.na(df_test)) < (nrow(df_test)/2)]

# remove all columns that have more than half of their values as empty string
df_test3=df_test2[, colSums(df_test2=="") < (nrow(df_test2)/2)]

df_test3 = df_test3[,-nearZeroVar(df_test3)]

unuseful_v = c("user_name","raw_timestamp_part_1",
               "raw_timestamp_part_2", "cvtd_timestamp")

df_test3[,unuseful_v] = NULL


test_predictions = predict(model_rf, df_test3)
```

The predicted values for the test data are this:

```r
print(test_predictions)
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
