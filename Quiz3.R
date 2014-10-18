library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
# 1. Subset the data to a training set and testing set based on the Case variable in the data set.

inTrain <- createDataPartition(y = segmentationOriginal$Case, list = FALSE)
train <- subset(segmentationOriginal, Case == "Train")
test <- subset(segmentationOriginal, Case == "Test")

set.seed(125)

modFit <- train(Class ~ ., data = train, method = "rpart")
modFit$finalModel

plot(modFit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = .8)

fancyRpartPlot(modFit$finalModel)
fancyRpartPlot(modFit)


predict(modFit, newdata = train)


library(caret)
library(pgmm)
data(olive)
olive = olive[,-1]
library(randomForest)

#Fit a classification tree where Area is the outcome variable. 
# Then predict the value of area for the following data frame using the tree command with all defaults

model <- train(Area ~ ., data = olive, method = "rpart2")

newdata = as.data.frame(t(colMeans(olive)))

predict(model, newdata = newdata)



##########

install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

#Then set the seed to 13234 and fit a logistic regression model (method="glm", 
# be sure to specify family="binomial") with Coronary Heart Disease (chd) as the 
# outcome and age at onset, current alcohol consumption, obesity levels, 
# cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol 
# as predictors. Calculate the misclassification rate for your model using this 
# function and a prediction on the "response" scale:

model <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
               data = trainSA, method = "glm", family = "binomial")

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(testSA$chd, predict(model, newdata = testSA))
missClass(trainSA$chd, predict(model, newdata = trainSA))

#######
install.packages("randomForest")
library(randomForest)
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test)

set.seed(33833)

# Fit a random forest predictor relating the factor variable y to the remaining variables.
a <- randomForest(y ~ ., data = vowel.train, importance = TRUE)
a
b <- varImp(a)
b
order(b)
