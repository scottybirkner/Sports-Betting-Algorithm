i = 1
  title = paste("Bootstrapped Sampling Distribution for\n y intercept", sep = "")
  # Histogram of the bootstrapped sampling distribution for the estimated beta_i
  hist( bootstrap_beta[,i], breaks=floor(sqrt(num_boot_samples)), freq=F,
      main=title, ylim = c(0,6))
  abline( v=beta_MLE[i], col="green", lwd=3)

  # Since we generated the regression data with Gaussian errors, we know the true
  # sampling distribution.  So overlay the true density on the histogram
  grid = seq( beta_MLE[i]-3, beta_MLE[i]+3, by=.01)
  lines( grid, dnorm( grid, mean=beta_MLE[i], 
                    sd=sqrt(solve(t(X)%*%X)[1,1])*sd(y)), 
       lwd=3)

  i = i + 1
  title = paste("Bootstrapped Sampling Distribution for\n Away Odds Estimate", sep = "")
  # Histogram of the bootstrapped sampling distribution for the estimated beta_i
  hist( bootstrap_beta[,i], breaks=floor(sqrt(num_boot_samples)), freq=F,
        main=title)
  abline( v=beta_MLE[i], col="green", lwd=3)
  
  # Since we generated the regression data with Gaussian errors, we know the true
  # sampling distribution.  So overlay the true density on the histogram
  grid = seq( beta_MLE[i]-3, beta_MLE[i]+3, by=.01)
  lines( grid, dnorm( grid, mean=beta_MLE[i], 
                      sd=sqrt(solve(t(X)%*%X)[1,1])*sd(y)), 
         lwd=3)
  
  
  i = i + 1
    title = paste("Bootstrapped Sampling Distribution for\n Opening Home Line Estimate", sep = "")
  # Histogram of the bootstrapped sampling distribution for the estimated beta_i
  hist( bootstrap_beta[,i], breaks=floor(sqrt(num_boot_samples)), freq=F,
        main=title)
  abline( v=beta_MLE[i], col="green", lwd=3)
  
  # Since we generated the regression data with Gaussian errors, we know the true
  # sampling distribution.  So overlay the true density on the histogram
  grid = seq( beta_MLE[i]-3, beta_MLE[i]+3, by=.01)
  lines( grid, dnorm( grid, mean=beta_MLE[i], 
                      sd=sqrt(solve(t(X)%*%X)[1,1])*sd(y)), 
         lwd=3)
  i = i + 1
    title = paste("Bootstrapped Sampling Distribution for\n New England Home Games", sep = "")
  # Histogram of the bootstrapped sampling distribution for the estimated beta_i
  hist( bootstrap_beta[,i], breaks=floor(sqrt(num_boot_samples)), freq=F,
        main=title)
  abline( v=beta_MLE[i], col="green", lwd=3)
  
  # Since we generated the regression data with Gaussian errors, we know the true
  # sampling distribution.  So overlay the true density on the histogram
  grid = seq( beta_MLE[i]-3, beta_MLE[i]+3, by=.01)
  lines( grid, dnorm( grid, mean=beta_MLE[i], 
                      sd=sqrt(solve(t(X)%*%X)[1,1])*sd(y)), 
         lwd=3)
  i = i + 1
    title = paste("Bootstrapped Sampling Distribution for\n Cleveland Browns Away Games", sep = "")
  # Histogram of the bootstrapped sampling distribution for the estimated beta_i
  hist( bootstrap_beta[,i], breaks=floor(sqrt(num_boot_samples)), freq=F,
        main=title)
  abline( v=beta_MLE[i], col="green", lwd=3)
  
  # Since we generated the regression data with Gaussian errors, we know the true
  # sampling distribution.  So overlay the true density on the histogram
  grid = seq( beta_MLE[i]-3, beta_MLE[i]+3, by=.01)
  lines( grid, dnorm( grid, mean=beta_MLE[i], 
                      sd=sqrt(solve(t(X)%*%X)[1,1])*sd(y)), 
         lwd=3)
  ## FPR/TPR plot
  # Add this ROC curve to compare with the ROC on the training data
  plot( FPR, TPR, lwd=2, type="l", main = "ROC Curve, Bootstrapped Estimators")
  lines( grid, grid, lty="dotted", lwd=2)
  lines( FPR_test, TPR_test, lwd=2, type="l", col="blue")
  