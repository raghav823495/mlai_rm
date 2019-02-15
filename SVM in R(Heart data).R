library(caret)
library(e1071)

heart_df <-read.csv(file.choose())
str(heart_df)
head(heart_df)
summary(heart_df)
unique(heart_df$Sex)
heart_df = as.factor()

sum(is.na(heart_df))

#Creating test and train datasets
set.seed(3033)
intrain <- createDataPartition(y = heart_df$target, p= 0.7, list = FALSE)
training <- heart_df[intrain,]
testing <- heart_df[-intrain,]
dim(training)
dim(testing)

## summary and checking missing value
summary(training)
sapply(training,function(x) sum(is.na(x)))
##target variable from int to categorcial variable
testing$target = as.factor(testing$target)
## to check whether target has convert or not 
str(training)
# Building model 
## without cost 
svm_model <- svm(target ~ ., data=training)
summary(svm_model)
## with cost 
#svm_model <- svm(target ~ ., data=training,kernal="linear",cost=1,scale=FALSE)
#summary(svm_model)
#Plotting
svm_model$index
attach(training)
names(training)
plot(svm_model,training,Trestbps~Chol)
### Prediction
testing_pred<-predict(svm_model,newdata = testing)
testing_pred
# accuracy of model on test data
confusionMatrix(testing_pred, testing$target )
table(testing$target)

## how to increase the accuracy of the model (tuning) kernel can be linear,radial etc
tune.out=tune(svm,target ~ .,data=training ,kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))
summary (tune.out)

## best model
bestmod=tune.out$best.model
summary (bestmod )

## Prediction 
ypred=predict (bestmod,testing)
table(predict =ypred , truth= testing$target )
# accuracy of model on test data
confusionMatrix(ypred,testing$target)
