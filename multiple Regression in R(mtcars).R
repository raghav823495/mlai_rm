newdata<-mtcars
newdata1<-mtcars

newdata$am<-NULL
names(newdata)
str(newdata)
summary(newdata)
boxplot(newdata)
cor(newdata)
#model <- step(lm(mpg~., data = newdata),direction = "backward")
model <- lm(mpg~.-disp-cyl-hp, data = newdata)
model
summary(model)
library(car)
vif(model)
