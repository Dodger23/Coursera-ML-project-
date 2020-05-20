library(caret)



# Reading the data 

data = read.csv("data/pml-training.csv")


# Splitting the data into 75% raining and 25% testing 

inTrain = createDataPartition(y= data$classe , p = 0.75 , list = FALSE)
training = data[inTrain , ]
testing = data[-inTrain , ]




# Exploring the training data

explore = function(data)
{
  str(data)
  find_na = apply(is.na(data),2,sum)
  print(find_na)
}

explore(training)

#Because the data contains a lot of variables we need to remove similar ones to reduce computation complexity.

#Also the data contains a lot of NA's, so we need to clean it.
#If there were few NA's maybe we could use KNN, but with this number of NA's in the columns it's better to remove it .

## Cleaning the training data 

clean_data = function(data)
{
  nzv = nearZeroVar(data , saveMetrics = T)
  keepFeat = row.names(nzv[nzv$nzv == FALSE, ])
  data = data[, keepFeat]
  data = data[, colSums(is.na(data)) == 0]
  data 
}

training = clean_data(training)




# Modeling
#Using cross validation and random forest

#Saving trainControl() call with required parameters
odCtl <- trainControl(method = 'cv', number = 5)
set.seed(2384)
modRf <- train(classe ~. , data = training, method = 'rf', trControl = modCtl)
modRf$finalModel



# Predicting

pred = predict(modRf , testing)
confusionMatrix(pred , testing$classe)
confusionMatrix(pred , testing$classe)$overall[1]



# Qruiz 

Quiz = read.csv("data/pml-testing.csv")
Qpred = predict(modRf , Quiz)
Qpred
