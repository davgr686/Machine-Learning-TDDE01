# Loading data
library("readxl")
#library("caret")
library("xtable")
library("kknn")

data <- read_excel("spambase.xlsx")

# make spam categorical
data$Spam <- as.factor(data$Spam)

# Split into train & test set
n=dim(data)[1] 
set.seed(12345)  
id=sample(1:n, floor(n*0.5))  
train=data[id,]  
test=data[-id,]  

# Fit the model on train set and predict on train
logRegModel <- glm(Spam ~ ., data = train, family = binomial)

#predict on train set with 0.5 cutoff
train_probs <- predict(logRegModel, train, type = "response")
train_pred <- ifelse(train_probs > 0.5, "1", "0")
table("Actual" = train$Spam, "Predicted" = train_pred )
mean(train_pred != train$Spam)

#predict on test set with 0.5 cutoff
test_probs <- predict(logRegModel, test, type = "response")
test_pred <- ifelse(test_probs > 0.5, "1", "0")
table("Actual" = test$Spam, "Predicted" = test_pred )
mean(test_pred != test$Spam)

#predict on train set with 0.8 cutoff
train_pred08 <- ifelse(train_probs > 0.8, "1", "0")
table("Actual" = train$Spam, "Predicted" = train_pred08 )
mean(train_pred08 != train$Spam)

#predict on test set with 0.8 cutoff
test_pred08 <- ifelse(test_probs > 0.8, "1", "0")
table("Actual" = test$Spam, "Predicted" = test_pred08 )
mean(test_pred08 != test$Spam)

## KKNN with k = 30

kknn_classifier = kknn(Spam ~ ., train, train, k=30)
fit <- fitted(kknn_classifier)
table(train$Spam, fit)
mean(fit != train$Spam)

kknn_classifier = kknn(Spam ~ ., train, test, k=30)
fit <- fitted(kknn_classifier)
table(test$Spam, fit)
mean(fit != test$Spam)



## KKNN with k = 1
kknn_classifier = kknn(Spam ~ ., train, train, k=1)
fit <- fitted(kknn_classifier)
table(train$Spam, fit)
mean(fit != train$Spam)


kknn_classifier = kknn(Spam ~ ., train, test, k=1)
fit <- fitted(kknn_classifier)
table(test$Spam, fit)
mean(fit != test$Spam)

##CLEAN UP 
rm(list = ls())
gc()
cat("\014") 
