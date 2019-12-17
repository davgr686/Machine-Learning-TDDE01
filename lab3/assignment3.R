library(neuralnet) 
#install.packages("neuralnet")
set.seed(1234567890)
Var <- runif(50, 0, 10) 
trva <- data.frame(Var, Sin=sin(Var)) 
tr <- trva[1:25,] # Training 
va <- trva[26:50,] # Validation
#plot(tr)
# Random initialization of the weights in the interval [-1, 1] 
winit <- runif(10, min=-1, max=1)# Your code here
MSE = c()
thresholds = c()


for(i in 1:10) { 
  nn <- neuralnet(Sin ~ Var, data=tr, startweights = winit, threshold = i/1000, hidden = c(10))
  # Your code here
  pred <- predict(nn, va)
  #break
  MSE[i] <- mean((pred - va$Sin)^2)
  #break
  #print((i/1000))
  thresholds[i] <- (i/1000)
}

plot(y=MSE, x=thresholds, type = 'o', main="Validation MSE for different thresholds")
best_threshold <- thresholds[which.min(MSE)]

nn <- neuralnet(Sin ~ Var, data=trva, startweights = winit, threshold = best_threshold, hidden = c(10))
plot(nn)
# Plot of the predictions (black dots) and the data (red dots) 
plot(prediction(nn)$rep1,  col = "blue", main="Predicions")
points(trva, col = "red")
