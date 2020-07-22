#@ Dr. Kurban, July 2020, conctact hakurban@gmail.com for any questions.
############################## DATA PREPROCESSING #############################################
filelist = list.files(pattern = ".txt")
print(filelist)
datalist = lapply(filelist, function(x)read.table(x, header=F))
finalData =  data.frame(matrix(vector(), 0, 4, dimnames=list(c(), c("V1", "V2", "V3", "V4"))), stringsAsFactors=F)
temperature = seq(0,1000,50)
for (i in 1:length(datalist)){
  data <- as.data.frame(datalist[i]) 
  temp.data1 = rep(temperature[i],dim(as.data.frame(datalist[1]))[1])
  temp.data2 = cbind(data,temp.data1)
  finalData <- rbind(finalData,temp.data2)
}
colnames(finalData)[5]  <- "Temperature"
summary(finalData)
dim(finalData)
################################# PARTION DATA: TRAINING AND TESTING #########################################################
library(caret)
set.seed(333)
#Spliting data as training and test set. Using createDataPartition() function from caret
indxTrain <- createDataPartition(y = finalData$V1,p = 0.75,list = FALSE)
training <- finalData[indxTrain,]
testing <-finalData[-indxTrain,]

#Checking distibution in origanl data and partitioned data
prop.table(table(training$V1)) * 100
prop.table(table(testing$V1)) * 100
prop.table(table(finalData$V1)) * 100
testing[,1] <- as.factor(testing[,1])
###############################################################################################
#                                             MODELS
###############################################################################################
# packages and parameters
library(pROC)
ctrl <- trainControl(method="repeatedcv",number = 10, repeats = 10,classProbs=TRUE,summaryFunction = twoClassSummary)
###############################################################################################
library(ggfortify)
library(precrec)
#Knn
set.seed(444)
knnFit <- train(V1 ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
trellis.par.set(caretTheme())
knnTrainingROCGraph<- plot(knnFit,type ="S", main = "KNN Algorithm")

#metrics
otherPredict <- predict(knnFit,newdata = testing)
confusionMatrix(otherPredict, testing$V1 )
mean(otherPredict == testing$V1)
# ROC & precision-recall plots
knnPredict <- predict(knnFit,newdata = testing, type = "prob")
precrec_obj <- evalmod(scores = knnPredict[,2], labels = testing$V1)
autoplot(precrec_obj)
##################################################################################################
# Random forrest
rfFit <- train(V1 ~ ., data = training, method = "rf", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
rfTrainingROCGraph<- plot(rfFit,type ="S", main = "Random Forest Algorithm")
#metrics
otherPredict <- predict(rfFit,newdata = testing )
confusionMatrix(otherPredict, testing$V1 )
mean(otherPredict == testing$V1)
# ROC & precision-recall plots
rfPredict <- predict(rfFit,newdata = testing, type = "prob")
precrec_obj <- evalmod(scores = rfPredict[,2], labels = testing$V1)
autoplot(precrec_obj)s
##################################################################################################
#Naive Bayes
nbFit <- train(V1 ~ ., data = training, method = "naive_bayes", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
nbTrainingROCGraph<- plot(nbFit,type ="S", main = "Naive Bayes Algorithm")
#metrics
nbotherPredict <- predict(nbFit,newdata = testing )
confusionMatrix(otherPredict, testing$V1 )
mean(otherPredict == testing$V1)
# ROC & precision-recall plots
nbPredict <- predict(nbFit,newdata = testing, type = "prob")
precrec_obj <- evalmod(scores = nbPredict[,2], labels = testing$V1)
autoplot(precrec_obj)
##################################################################################################
#rFit
rFit <- train(V1 ~ ., data = training, method = "rpart", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
rTrainingROCGraph<- plot(rFit,type ="S", main = "Decision Tree Algorithm")
#metrics
rotherPredict <- predict(rFit,newdata = testing )
confusionMatrix(otherPredict, testing$V1 )
mean(rotherPredict == testing$V1)
# ROC & precision-recall plots
rPredict <- predict(rFit,newdata = testing, type = "prob")

precrec_obj <- evalmod(scores = rPredict[,2], labels = testing$V1)
autoplot(precrec_obj, main= "life")
##################################################################################################
# dot plot
library(mlbench)
results <- resamples(list(RF=rfFit, DT=rFit, NB= nbFit, KNN=knnFit))
dotplot(results)