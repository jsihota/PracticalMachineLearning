# install.packages("RCurl")
library(RCurl)
library(caret)

trainingDataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testDataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainingData <- read.csv(textConnection(getURL(trainingDataURL)), na.strings = c("NA", ""))
testData <- read.csv(textConnection(getURL(trainingDataURL)), na.strings = c("NA", ""))

#clean up data
NAs <- apply(trainingData, 2, function(x) {
  sum(is.na(x))
})

cleanTrainingData <- trainingData[, which(NAs == 0)]
cleanTestData <- testData[, which(NAs == 0)]

summary(cleanTrainingData)
summary(cleanTestData)

##Using 70% for training and 30% for Cross Validation. None generated for testing since that set is already provided.
trainIndex <- createDataPartition(y = cleanTrainingData$classe, p = 0.7, list = FALSE)
trainSet <- cleanTrainingData[trainIndex, ]
crossValidationSet <- cleanTrainingData[-trainIndex, ]
# Removing variables that have time, or names in it, also new_window.
# Columns 1..6
removeIndex <- as.integer(c(1, 2, 3, 4, 5, 6))
trainSet <- trainSet[, -removeIndex]
testSet <- cleanTestData[, -removeIndex]

dim(trainSet)
dim(testSet)


####
library(randomForest)
library(caret)
set.seed(13333)
fitControl<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)

modelFit <- train(trainSet$classe ~ ., data = trainSet, method = "rf", trControl = fitControl)

modelFit


#Calculation the errors using the Cross Validation Set.

predicted <- predict(modelFit, crossValidationSet)
SampleError <- sum(predicted == crossValidationSet$classe)/nrow(crossValidationSet)
#So the Out of Sample Error we get is: 0.9981
confusionMatrix(predicted, crossValidationSet$classe)

answers <- predict(modelFit, testSet)