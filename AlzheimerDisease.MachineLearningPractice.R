library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#data loaded 
ILs <- grep("^IL.*",names(training)) #colnames that begin with IL 
ILtrain <- training[,c(1,ILs)] #included diagnosis column and IL columns 


#NON-PCA Model 
modelFit1 <- train(diagnosis~., data = ILtrain, method = "glm")
#prediction 
NONPCApredict <- predict(modelFit1, newdata = testing)
confusionMatrix(NONPCApredict, testing$diagnosis)  #accuracy 65% 

#PCA Model 
ModelFitPCA <- train(diagnosis ~ ., 
                     data = ILtrain,
                     method = "glm",
                     preProc = "pca", 
                     trControl = trainControl(preProcOptions = list(thresh = .8)))

#train the model using GLM with the principle components identified 80% variance

PCApredict <- predict(ModelFitPCA, newdata = testing)
confusionMatrix(PCApredict, testing$diagnosis)  #accuracy 72% 

