#--------------------------------------------------------------
# Will use Boston data set
# To predict Boston Median house prices
# the median value of owner-occupied homes for each of these surburbs, as the response variable.
#-------------------------
library (MASS)  # Boston dataset lies in Mass package
View(Boston)
attach(Boston)


#----------------------------------------------------------
# Check the data set
#---------------------
str(Boston)
summary(Boston)

#----------------------------------------------------------
# Split the dataset
#--------------------
set.seed (1)

library(caret)
Train <- createDataPartition(Boston$medv, p=0.7, list = FALSE) #List splits the data with rows.
dataTrain <- Boston[Train, ]
dataTest <- Boston[-Train, ]

#-------------------------------------------------------
# Modal Creation 
#------------------
library(randomForest)

# Default Random Forest takes 500 trees
modalRandom =randomForest(medv~., data =dataTrain) 

cat("Total trees = ", modalRandom$ntree)
#cat("MSE = ", modalRandom$mse)

summary (modalRandom)

#---------------------------------------------
# Ploting
#--------------
plot(modalRandom)
text(modalRandom ,pretty =0)

#---------------------------------------------
# Prediction
#-------------

predictModal=predict(modalRandom,dataTest,type ="class")
predictModal

# Confusion Matrix ------
confusionMatrix(predictModal, dataTest$medv)

#---------------------------------------------------------
# Boosting
#------------
library(gbm)

modalBoosting = gbm(medv~., data = dataTrain, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(modalBoosting)


#---------------------------------------------
# Prediction
#-------------

predictModal=predict(modalBoosting,dataTest)
predictModal
results = data.frame(dataTest$medv,predictModal)
results




# Confusion Matrix ------
confusionMatrix(predictModal, dataTest$medv)








#-----------------------------------------------
# Tree Creation
#----------------
cv.boston =cv.tree(tree.boston)
cv.boston
plot(cv.boston$size,cv.boston$dev,type="b")
prune.boston=prune.tree(tree.boston,best =4)
plot(prune.boston)
text(prune.boston,pretty =0)
yhat=predict (tree.boston ,newdata =Boston [-train ,])
boston.test=Boston [-train ,"medv"]


