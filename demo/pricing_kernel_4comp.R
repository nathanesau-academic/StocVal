marketData <- readRDS("~/Dropbox/Research/StocVal/data/Canada/varinput_canada.Rda")
varData <- data.frame(onemonth=marketData$onemonth,inflation=marketData$inflation,
  tenyear=marketData$tenyear,stock=marketData$stock)
histYields <- data.frame(twoyear=marketData$twoyear, threeyear=marketData$threeyear,
  fiveyear=marketData$fiveyear, tenyear=marketData$tenyear)

cat("Fit VAR OLS model (calibration step 1)
---------------------------\n")
mu <- matrix(c(mean(varData$onemonth), mean(varData$inflation),
  mean(varData$tenyear), mean(varData$stock)), 4, 1)
varData <- data.frame(onemonth=varData$onemonth - mu[1], 
  inflation=varData$inflation - mu[2], tenyear=varData$tenyear-mu[3],
  stock=varData$stock - mu[4])
var_ols.out <- var_ols(varData)

cat("OLS inputs (from calibration step 1) \n")
Phi <- var_ols.out$Phi
Sigma <- var_ols.out$Sigma

cat("\nPhi
---------------------------\n")
print(Phi)

cat("\nSigma
---------------------------\n")
print(Sigma)

N <- c(2,3,5,10)
varData <- data.frame(onemonth=marketData$onemonth,inflation=marketData$inflation,
  tenyear=marketData$tenyear,stock=marketData$stock)
calibrate_VAR.out <- calibrate_VAR(varData, histYields, Phi, Sigma, N);

cat("\nPricing kernel inputs (from calibration step 2) \n")
cat("\nSigmaL0
---------------------------\n")
print(calibrate_VAR.out$SigmaL0)

cat("\nSigmaL1
---------------------------\n")
print(calibrate_VAR.out$SigmaL1)

cat("see plots")
compare_yield_fit(varData, histYields, calibrate_VAR.out, Phi, Sigma, N)
