#' @title VAR model (least squares estimates)
#' @description First step of VAR model calibration. Number of state variables 
#'  is equal to length(varData[1,]) - however, currently 4 is required since the pricingKernel
#'  is currently implemented to accept a 4 component VAR. Note that the MASS
#'  and vars packages are required.
#' @param varData The VAR variables to use in the model
#' @param marketData The data containing the VAR variables and term structure historical
#'  yields.
#' @return A list containing Phi (the coefficients) and Sigma (the covariance matrix)
#' @details When calibrating the VAR model, the mean is subtracted and we 
#'  estimate (zt-u) not zt. Also note that a VAR(1) model is estimated. For example,
#'  marketData <- readRDS("~/Dropbox/Research/StocVal/data/Canada/varinput_canada.Rda");
#'  varData <- data.frame(onemonth=market_data$onemonth, inflation=market_data$inflation,
#'    tenyear=market_data$tenyear, stock=market_data$stock);
#'  mu <- matrix(c(mean(varData$onemonth), mean(varData$inflation),
#'    mean(varData$tenyear), mean(varData$stock)), 4, 1);
#'  varData <- data.frame(onemonth=varData$onemonth - mu[1], 
#'    inflation=varData$inflation - mu[2], tenyear=varData$tenyear-mu[3],
#'    stock=varData$stock - mu[4]);
#'  var_ols.out <- var_ols(varData)
#' @export 
var_ols <- function(varData) {
  library(vars)
  library(MASS) # packages
  
  var_model <- VAR(varData, p=1, type="none") # fit var(1)
  Phi <- coef(var_model)
  Phi <- matrix( c(Phi$onemonth[,1], Phi$inflation[,1], Phi$tenyear[,1], 
    Phi$stock[,1]), 4, 4, byrow=TRUE)
  Sigma <- summary(var_model)$covres
  print(summary(var_model)) # prints var model
  list(Phi=Phi, Sigma=Sigma) # return estimated OLS parameters
}