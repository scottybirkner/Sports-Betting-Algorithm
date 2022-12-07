
summary(BettingData$homeCover)
summary(BettingData$awayCover)
summary(BettingData$Home.Line.Open)
summary(BettingData$Away.Odds.Open)
cor(BettingData$Home.Line.Open, BettingData$Away.Odds.Open)
boxplot(BettingData$Home.Line.Open, xlab = "Home Line Boxplot", horizontal = TRUE)
boxplot(BettingData$Away.Odds.Open, xlab = "Away Odds Boxplot", horizontal = TRUE)


## each variable analyzed individually
alpha = .2
## away odds
x = as.matrix(BettingData[1:n,13])
# Construct a level 1-alpha confidence interval
beta_1_hat = sum((x - mean(x))*y) / sum((x - mean(x))^2)
se_beta_1_hat = sigma / sqrt(sum((x - mean(x))^2))
critical_z = qnorm(1 - alpha/2)  # qnorm gives the inverse Gaussian CDF
lower = beta_1_hat - critical_z * se_beta_1_hat; lower
upper = beta_1_hat + critical_z * se_beta_1_hat; upper

## home line
x = as.vector(BettingData[1:n,17])
# Construct a level 1-alpha confidence interval
beta_2_hat = sum((x - mean(x))*y) / sum((x - mean(x))^2)
se_beta_2_hat = sigma / sqrt(sum((x - mean(x))^2))
critical_z = qnorm(1 - alpha/2)  # qnorm gives the inverse Gaussian CDF
lower = beta_2_hat - critical_z * se_beta_2_hat; lower
upper = beta_2_hat + critical_z * se_beta_2_hat; upper

## at NE
x = as.matrix(BettingData[1:n,55])
# Construct a level 1-alpha confidence interval
beta_1_hat = sum((x - mean(x))*y) / sum((x - mean(x))^2)
se_beta_1_hat = sigma / sqrt(sum((x - mean(x))^2))
critical_z = qnorm(1 - alpha/2)  # qnorm gives the inverse Gaussian CDF
lower = beta_1_hat - critical_z * se_beta_1_hat; lower
upper = beta_1_hat + critical_z * se_beta_1_hat; upper

## away CLE
x = as.vector(BettingData[1:n,59])
# Construct a level 1-alpha confidence interval
beta_2_hat = sum((x - mean(x))*y) / sum((x - mean(x))^2)
se_beta_2_hat = sigma / sqrt(sum((x - mean(x))^2))
critical_z = qnorm(1 - alpha/2)  # qnorm gives the inverse Gaussian CDF
lower = beta_2_hat - critical_z * se_beta_2_hat; lower
upper = beta_2_hat + critical_z * se_beta_2_hat; upper
GD = function( gradient, f, eta=.01, x0, epsilon=10^-6, max_iter=10^7){
  
  t = 2
  trace = matrix( NA, ncol=length(x0), nrow=max_iter)
  trace[1:2,] =	rbind( x0, x0 + 2*epsilon)
  neg_l = f(trace[t,])
  # Begin the gradient descent algorithm
  while( sum((trace[t,] - trace[t-1,])^2)^.5 >= epsilon & t < max_iter ){
    
    # Update the parameters
    trace[t+1,] = trace[t,] - eta * gradient(trace[t,])
    #print(trace[t+1,])		
    t = t +1
    
    # If negative log-likelihood increases, then reduce learning rate
    neg_l_new = f(trace[t,]) 
    #cat(neg_l_new, "  ", neg_l, "\n")
    if(neg_l_new >= neg_l) eta = eta * .9 
    neg_l = neg_l_new
  }
  cat("final learning rate = ", eta, "\n")
  
  if(t == max_iter){
    print("Warning: max iterations exceeded")
    trace[max_iter,] = NA
  }
  
  return(trace[1:t,])
}



# Generate synthetic training data from a logistic regression model
## First try with homeCover... determined I could get a better prediction with an away model


n = 1000
p = 5
X = cbind( 1,as.matrix(BettingData[1:n,c(13,17,55,59)]))
y = BettingData[1:n,54]


# Train the model
df = function(b){
  sigmoid = 1 / (1 + exp(-c(X %*% b)))
  return( colSums((sigmoid - y) * X) )
}

neg_l = function(b){
  sigmoid = 1 / (1 + exp(-c(X %*% b)) +10^-6) # 10^-6 for numerical stability
  return( -sum(y * log(sigmoid) + (1 - y) * log(1 - sigmoid)) )
}

b0 = runif(p) * 2*sqrt(6/p) + (-sqrt(6/p))
beta_MLE = c(tail( GD( gradient=df, f=neg_l, x0=b0), 1))
N = 100  # Number of simulated data sets
sigma = sd(y)
alpha = .01
critical_z = qnorm(1 - alpha/2)
coverage = 0
for(k in 1:N){
  set.seed(k)
  py = 1 / ( 1 + exp(-X %*% beta_MLE))
  Y_test = c(runif(n) < py)
  beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y
  ##	x_test = cbind( 1, matrix( rnorm(1*(p-1)), ncol=p-1))
  ##Y_test = x_test %*% beta + rnorm( 1, sd=sigma)
  
  se_y_pred = sigma * sqrt(X[n,] %*% solve(t(X) %*% X) %*% t(t(X[n,])) + 1)
  lower = X[n,] %*% beta_hat - critical_z * se_y_pred
  upper = X[n,] %*% beta_hat + critical_z * se_y_pred
  
  if( lower < Y_test[n] & Y_test[n] < upper)  coverage = coverage +1
}
coverage = coverage / N; coverage

num_sims = 100
sampling_dist = matrix( NA, num_sims, p);
n = 200
X = X[1:n,]
for(k in 1:num_sims){
  set.seed(k)
  # Generate a synthetic data set
  py = 1 / ( 1 + exp(-X %*% beta_MLE))
  y = c(runif(n) < py)
  
  df = function(b){
    sigmoid = 1 / (1 + exp(-c(X %*% b)))
    return( colSums((sigmoid - y) * X) )
  }
  
  neg_l = function(b){
    sigmoid = 1 / (1 + exp(-c(X %*% b)) +10^-6) # 10^-6 for numerical stability
    return( -sum(y * log(sigmoid) + (1 - y) * log(1 - sigmoid)) )
  }
  
  b0 = runif(p) * 2*sqrt(6/p) + (-sqrt(6/p))
  trace = GD( gradient=df, f=neg_l, x0=b0)
  sampling_dist[k,] = tail( trace, 1)
  
  print(k)
}


# Plot the sampling distribution of the components of the MLEs
par(mfrow=c(2,2))
for(j in 1:p){
  hist( sampling_dist[,j], breaks=floor(sqrt(num_sims)), main=paste0("beta_",j),
        xlab=NA, freq=F)
  abline( v=beta_MLE[j], col="green", lwd=3)
} 
X = cbind( 1,as.matrix(BettingData[1:1000,c(13,17,55,59)]))
py_hat = 1 / ( 1 + exp(-X %*% beta_MLE))

y_hat = (py_hat > .5) # Using a threshold of .5

# False positive rate
FPR = sum(y_hat[y == 0] == 1) / sum(y == 0); FPR

# True postive rate (i.e., sensitivity)
TPR = sum(y_hat[y == 1] == 1) / sum(y == 1); TPR

# True negative rate (i.e., specificity)
TNR = sum(y_hat[y == 0] == 0) / sum(y == 0); TNR
# How do these values change for different thresholds?
grid = seq( 0, 1, by=.001)
FPR = rep( NA, length(grid))
TPR = rep( NA, length(grid))
for(k in 1:length(grid)){
  y_hat = (py_hat > grid[k])
  FPR[k] = sum(y_hat[y == 0] == 1) / sum(y == 0)
  TPR[k] = sum(y_hat[y == 1] == 1) / sum(y == 1)
}

# Plot the ROC curve


# This ROC curve shows how well the logistic regression is able to classify the
# training data.  How well does it generalize to data it was not trained on?
# Generate synthetic testing data from the logistic regression model
py = 1 / ( 1 + exp(-X %*% beta_MLE))
set.seed(1)
y_test = c(runif(n) < py)


FPR_test = rep( NA, length(grid))
TPR_test = rep( NA, length(grid))
for(k in 1:length(grid)){
  y_test_hat = (py > grid[k])
  FPR_test[k] = sum(y_test_hat[y_test == 0] == 1) / sum(y_test == 0)
  TPR_test[k] = sum(y_test_hat[y_test == 1] == 1) / sum(y_test == 1)
}

# Add this ROC curve to compare with the ROC on the training data
plot( FPR, TPR, lwd=2, type="l", main= "ROC curve, Maximum Likelihood Estimators")
lines( grid, grid, lty="dotted", lwd=2)
lines( FPR_test, TPR_test, lwd=2, type="l", col="blue")

