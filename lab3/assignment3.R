library(neuralnet) 
#install.packages("neuralnet")
set.seed(1234567890)
Var <- runif(50, 0, 10) 
trva <- data.frame(Var, Sin=sin(Var)) 
tr <- trva[1:25,] # Training 
va <- trva[26:50,] # Validation
plot(tr)
# Random initialization of the weights in the interval [-1, 1] 
winit <- -0.5# Your code here
for(i in 1:10) { 
  nn <- neuralnet(Sin ~ Var, data=tr, startweights = winit, threshold = i/1000, hidden = 10)
  # Your code here
  plot(prediction(nn)$rep1)
}

plot(nn <- neuralnet(Sin ~ Var, data=tr, startweights = -0.5, threshold = 2/1000))
  # Plot of the predictions (black dots) and the data (red dots) 
plot(prediction(nn)$rep1) 
points(trva, col = "red")
