# Loading data
library("readxl")
require(ggplot2)
#install.packages('kknn', dependencies=TRUE)
data <- read_excel("machines.xlsx")
head(data)
d <- density(data$Length) # returns the density data
plot(d) # plots the results

loglike <- function(theta, vector) {
  return (length(vector)*log(theta) - theta*sum(vector))
}

l_theta <- function(theta, vector) {
  lambda <- 10
  prior <- lambda*exp(-lambda*theta)
  return (log(prior) + loglike(theta, vector))
}

theta <- seq(from=0.01, to=5.00, by=0.01)
log_likelihood <- loglike(theta, data$Length)
log_likelihood1 <- loglike(theta, data$Length[0:6])


log_res <-
  data.frame(
    var0 = log_likelihood,
    var1 = log_likelihood1,
    x = theta
  )

ggplot(log_res, aes(x)) + 
  geom_line(aes(y = var0, colour = "Entire data set")) + 
  geom_line(aes(y = var1, colour = "6 first observations")) +
  ylab('Log-likelihood')+xlab('Theta')

################# PART 4

bayesian <- l_theta(theta, data$Length)

log_res <-
  data.frame(
    var0 = log_likelihood,
    var1 = log_likelihood1,
    var2 = bayesian,
    x = theta
  )

ggplot(log_res, aes(x)) + 
  geom_line(aes(y = var0, colour = "Entire data set")) + 
  geom_line(aes(y = var1, colour = "6 first observations")) +
  geom_line(aes(y = var2, colour = "Bayesian")) +
  ylab('Log-likelihood')+xlab('Theta')

################ PART 5

random_nmbrs <- hist(rexp(50, rate = 1.13), plot = FALSE)
orig_vals <- hist(data$Length, plot = FALSE)

plot( random_nmbrs, col=rgb(0,0,1,1/4), xlim=c(0,6))  # first histogram
plot( orig_vals, col=rgb(1,0,0,1/4), xlim=c(0,6), add=T) 


##CLEAN UP 
rm(list = ls())
gc()
cat("\014") 





