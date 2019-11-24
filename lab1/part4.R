## PART 1: Load data
library("readxl")
#install.packages("glmnet", dependencies = TRUE)
library(Metrics)
library(ggplot2)
library(MASS)
require(glmnet)
data <- read_excel("tecator.xlsx")
head(data)
plot(data$Moisture, data$Protein, xlab = "Moisture", ylab = "Protein")

## PART 2: 
#P(y) = b0 + b1x + ... + bIx^I + e



## PART #:
# Split into train & test set
n=dim(data)[1] 
set.seed(12345)  
id=sample(1:n, floor(n*0.5))  
train=data[id,]  
test=data[-id,]  

## Create models
M1 <- lm(Moisture ~ Protein, data = train )
M2 <- lm(Moisture ~ Protein + I(Protein^2), data = train )
M3 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3), data = train )
M4 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4), data = train )
M5 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4) + I(Protein^5), data = train )
M6 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4) + I(Protein^5) + I(Protein^6), data = train )

# Predict M1
M1.test_preds <- predict(M1, test, type = "response")
M1.train_preds <- predict(M1, train, type = "response")
M1.test_mse <- mean((M1.test_preds - test$Moisture)^2)
M1.train_mse <- mean((M1.train_preds - train$Moisture)^2)

# Predict M2
M2.test_preds <- predict(M2, test, type = "response")
M2.train_preds <- predict(M2, train, type = "response")
M2.test_mse <- mean((M2.test_preds - test$Moisture)^2)
M2.train_mse <- mean((M2.train_preds - train$Moisture)^2)

# Predict M3
M3.test_preds <- predict(M3, test, type = "response")
M3.train_preds <- predict(M3, train, type = "response")
M3.test_mse <- mean((M3.test_preds - test$Moisture)^2)
M3.train_mse <- mean((M3.train_preds - train$Moisture)^2)

# Predict M4
M4.test_preds <- predict(M4, test, type = "response")
M4.train_preds <- predict(M4, train, type = "response")
M4.test_mse <- mean((M4.test_preds - test$Moisture)^2)
M4.train_mse <- mean((M4.train_preds - train$Moisture)^2)

# Predict M5
M5.test_preds <- predict(M5, test, type = "response")
M5.train_preds <- predict(M5, train, type = "response")
M5.test_mse <- mean((M5.test_preds - test$Moisture)^2)
M5.train_mse <- mean((M5.train_preds - train$Moisture)^2)

# Predict M6
M6.test_preds <- predict(M6, test, type = "response")
M6.train_preds <- predict(M6, train, type = "response")
M6.test_mse <- mean((M6.test_preds - test$Moisture)^2)
M6.train_mse <- mean((M6.train_preds - train$Moisture)^2)

MSE.test<- c(M1.test_mse,M2.test_mse, M3.test_mse, M4.test_mse, M5.test_mse, M6.test_mse)
MSE.train<- c(M1.train_mse,M2.train_mse, M3.train_mse, M4.train_mse, M5.train_mse, M6.train_mse)
x <- seq(1, 6, by=1)

MSE_res <-
  data.frame(
    var0 = MSE.test,
    var1 = MSE.train,
    x = x
  )

ggplot(MSE_res, aes(x)) + 
  geom_line(aes(y = var0, colour = "Test")) + 
  geom_line(aes(y = var1, colour = "Train")) +
  ylab('MSE')+xlab('i')


## Part 4
linearModel <- lm(Fat ~ . - Protein - Moisture - Sample, data = data )
aic <- stepAIC(linearModel)
aic$anova

## Part 5

x=model.matrix(Fat~. - Protein - Moisture - Sample,data=data)


fit.ridge=glmnet(x,data$Fat,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)

fit.lasso=glmnet(x,data$Fat,alpha=1)
plot(fit.lasso,xvar="lambda",label=TRUE)

## cross-validation
cv.lasso=cv.glmnet(x,data$Fat, alpha=1)
cv.lasso$lambda.min
coef(cv.lasso, s="lambda.min")
plot(cv.lasso)



##CLEAN UP 
rm(list = ls())
gc()
cat("\014") 





