#' @title An (Pricing kernel helper)
#' @description Log price of a bond is An + Bn' zt where zt is VAR state vector (Step 1)
#' @param An previous value of An (i.e. An-1)
#' @param Bnt previous value of Bn' (i.e. Bn-1')
#' @param SigmaL0 pricing kernel parameter estimated in VAR Calibration (Step 2)
#' @param Sigma covariance matrix from OLS
#' @return An
#' @export 
A <- function(An, Bnt, SigmaL0, Sigma) { # An
  An - Bnt %*% SigmaL0 + 0.5 * Bnt %*% Sigma %*% t(Sigma) %*% t(Bnt)
}

#' @title Bn' (Pricing kernel helper)
#' @description Log price of a bond is An + Bn' zt where zt is VAR state vector (Step 1)
#' @param Bnt previous value of Bn' (i.e. Bn-1')
#' @param Phi coefficients from OLS
#' @param delta1 vector of constants 
#' @return Bn'
#' @export 
B <- function(Bnt, Phi, delta1, SigmaL1) { # Bnt
  -delta1 + Bnt %*% Phi - Bnt %*% SigmaL1 
}

#' @title VAR Calibration (Step 2)
#' @description Finds SigmaL0 and SigmaL1 for the pricing kernel
#'  given VAR parameters Phi and Sigma (which were calculated using OLS). 
#'  To make a different number of VAR components (than 4 work), modify SigmaL1 
#'  and SigmaL0 inside the function. NOTE that SigmaL0 and SigmaL1 
#'  should be used to generate a term structure where the mean is NOT SUBTRACTED
#'  from the marketData (note that mean was subtracted from marketData during step 1,
#'  i.e. in the var_ols() function).
#' @param varData VAR columns used in OLS (i.e. onemonth, inflation, tenyear, stock)
#' @param histYields histYields to calibrate term structure with (i.e. twoyear, threeyear,
#'  fiveyear, tenyear)
#' @param Phi Coefficients from OLS
#' @param Sigma Covariance matrix from OLS
#' @param N specifies the terms used in histYields (i.e. 2, 3, 5, 10)
#' @return A list containing SigmaL0 and SigmaL1
#' @details For example,
#'  marketData <- readRDS("~/Dropbox/Research/StocVal/data/Canada/varinput_canada.Rda");
#'  varData <- data.frame(onemonth=marketData$onemonth, inflation=marketData$inflation,
#'    tenyear=marketData$tenyear, stock=marketData$stock);
#'  histYields <- data.frame(twoyear=marketData$twoyear, threeyear=marketData$threeyear,
#'    fiveyear=marketData$fiveyear, tenyear=marketData$tenyear);
#'  N <- c(2,3,5,10)
#'  calibrate_VAR.out <- calibrate_VAR(varData, histYields, Phi, Sigma, N);
#' @export 
calibrate_VAR <- function(varData, histYields, Phi, Sigma,
  N=c(2,3,5,10)) {
  delta1 <- c(1,0,0,0) # 1 corresponds to 1 month short rate
  nrows <- length(histYields[,1]) # varData and histYields are same length
  
  # y has 11 components
  ytn <- function(y) { 
    # Solve for SigmaL1 and SigmaL0
    SigmaL1 <- matrix(c(y[1], y[2], y[3], y[4], 0, 0, 0, 0,   
      y[5], y[6], y[7], y[8], Phi[4,1], Phi[4,2], Phi[4,3], Phi[4,4]), 4,4,byrow=TRUE)
    SigmaL0 <- matrix(c(y[9], 0, y[10], y[11]), 4, 1)
    
    An <- c(0) # A0
    Bnt <- matrix(c(0,0,0,0),1,4) # B0
    
    nsum_index <- 1
    fittedYields <- vector("list", 4)
    for(n in 1:10) {
      An <- A(An, Bnt, SigmaL0, Sigma)
      Bnt <- B(Bnt, Phi, delta1, SigmaL1)
      if(is.element(n,N)) { # loop over t
        fittedYields[[nsum_index]] <- numeric(245) # store fitted yields
        for(t in 1:nrows) {
          zt <- matrix(as.numeric(varData[t,]), 4,1)
          pt <- An + Bnt %*% zt
          yt_fit <- -pt/n # fitted yield
          fittedYields[[nsum_index]][t] <- yt_fit
        }
        nsum_index <- nsum_index+1
      }
    }
    
    total <- 0
    for(i in 1:4) {
      total <- total + sum( (histYields[[i]] - fittedYields[[i]])^2)
    }
    total
  }
  
  y0 <- c(0.00,0.00,0.00,0.00, 0.00,0.00,0.00,0.00, 0.00,0.00,0.00)
  y <- optim(y0, ytn)$par # minimize ytn
  
  SigmaL1 <- matrix(c(y[1], y[2], y[3], y[4], 0, 0, 0, 0,
    y[5], y[6], y[7], y[8], Phi[4,1], Phi[4,2], Phi[4,3], Phi[4,4]),
    4,4,byrow=TRUE)
  SigmaL0 <- matrix(c(y[9], 0, y[10], y[11]), 4, 1)
  list(SigmaL1 = SigmaL1, SigmaL0 = SigmaL0) # return SigmaL1 and SigmaL0
}

#' @title Compare fitted yields to historical yields (PLOTS)
#' @description Plots historical vs fitted yields. Note that this function
#'  WILL work for any number of VAR components as long as calibrate_VAR.out
#'  input is correct (however currently calibrate_VAR() only takes 4 VAR components).
#' @param varData VAR columns used in OLS (i.e. onemonth, inflation, tenyear, stock)
#' @param histYields histYields to calibrate term structure with (i.e. twoyear, threeyear,
#'  fiveyear, tenyear)
#' @param calibrate_VAR.out output of calibrate_VAR
#' @param Phi coefficients of OLS
#' @param Sigma covariance matrix of OLS
#' @param N specifies the terms used in histYields (i.e. 2, 3, 5, 10). 
#'  Should be the same as parameter than was passed to calibrate_VAR().
#' @return see plots
#' @details For example, 
#'  marketData <- readRDS("~/Dropbox/Research/StocVal/data/Canada/varinput_canada.Rda");
#'  varData <- data.frame(onemonth=marketData$onemonth, inflation=marketData$inflation,
#'    tenyear=marketData$tenyear, stock=marketData$stock);
#'  histYields <- data.frame(twoyear=marketData$twoyear, threeyear=marketData$threeyear,
#'    fiveyear=marketData$fiveyear, tenyear=marketData$tenyear);
#'  N <- c(2,3,5,10)
#'  calibrate_VAR.out <- calibrate_VAR(varData, histYields, Phi, Sigma, N);
#'  compare_yield_fit(varData, histYields, calibrate_VAR.out, Phi, Sigma, N)
#' @export 
compare_yield_fit <- function(varData, histYields, calibrate_VAR.out, Phi, Sigma, 
  N=c(2,3,5,10)) {
  SigmaL0 <- calibrate_VAR.out$SigmaL0
  SigmaL1 <- calibrate_VAR.out$SigmaL1 
  ncomponents <- length(Phi[,1]) # number of VAR components
  
  delta1 <- c(1,0,0,0) # 1 corresponds to 1 month short rate
  nrows <- length(varData[,1]) # varData and histYields are same length
  An <- c(0)
  Bnt <- matrix(rep(0,ncomponents),1,ncomponents)
  total <- 0
  nsum_index <- 1
  
  fittedYields <- vector("list", ncomponents)
  for(n in 1:(max(N))) {
    An <- A(An, Bnt, SigmaL0, Sigma)
    Bnt <- B(Bnt, Phi, delta1, SigmaL1)
    if(is.element(n,N)) { # loop over t
      fittedYields[[nsum_index]] <- numeric(nrows)
      yields <- histYields[,nsum_index]
      for(t in 1:nrows) {
        zt <- matrix(as.numeric(varData[t,]), ncomponents, 1)
        pt <- An + Bnt %*% zt
        yt_fit <- -pt/n # fitted yield
        fittedYields[[nsum_index]][t] <- yt_fit
      }
      nsum_index <- nsum_index + 1 # next column of histYields
    }
  }
  
  # plot the fitted yields
  ytitles <- paste0(N,"-year")
  for(i in 1:4) {
    plot(histYields[[i]], ylab=ytitles[i], type='l', col='blue')
    lines(fittedYields[[i]], type='l', col='red')
  }
}

#' @title Generate VAR scenarios
#' @description Forward iterate a VAR model 
#' @param nscenarios The number of scenarios to generate
#' @param nyears The number of years to project for each scenario
#' @param params A list containing the VAR parameters Phi, Sigma, L0 and L1, z0
#'  and mu. By default the list var_params is used.
#' @param seed The random number seed to use
#' @return A list of nscenarios matrices. Each matrix is nyears*12 columns and
#'  the number of rows is the number of VAR components (in this case 4)
#' @details The MASS package is used to generate random numbers from a multivariate
#'  normal distribution. Note that zt is calculated using only Phi and Sigma
#'  since mean was subtracted - we add back the mean before returning the scenarios.
#' @examples gen_var_scenarios.out <- gen_var_scenarios()
#' @export 
gen_var_scenarios <- function(nscenarios=1, nyears=25, 
  params=var_params, seed=137531) {
  library(MASS)
  scenarios <- vector("list", nscenarios) # store the scenario matrices in a list
    
  # initialize some values outside loop
  Phi <- params$Phi
  Sigma <- params$Sigma
  z0 <- params$z0
  mu <- params$mu
  zt <- z0
  nmonths <- nyears * 12

  set.seed(seed)
  for(i in 1:nscenarios) {
    scenarios[[i]] <- matrix(0, 4, nmonths) # each column stores zt
    rand <- mvrnorm(n = nmonths, mu=rep(0,4), Sigma=Sigma)
    for(j in 1:nmonths) { # TO DO: vectorize this part of the code
      zt <- Phi %*% zt + rand[j,]
      scenarios[[i]][,j] <- zt + mu
    }
  }
  scenarios # return zt
}

#' @title Generate pricing kernels
#' @param gen_var_scenarios.out output of gen_var_scenarios()
#' @param nscenarios The number of scenarios to generate
#' @param nyears The number of years to project for each scenario
#' @param params A list containing the VAR parameters Phi, Sigma, L0 and L1, z0
#'  and mu. By default the list var_params is used.
#' @param seed the random number seed to use
#' @return A list of nscenarios vectors. Each vector is nyears*12 entries.
#' @details The MASS package is used to generate random numbers from a multivariate
#'  normal distribution. The same seed should be provided to gen_pricing_scenarios()
#'  that was provided to gen_var_scenarios().
#' @examples gen_var_scenarios.out <- gen_var_scenarios(); 
#'  gen_pricing_scenarios.out <- gen_pricing_scenarios(gen_var_scenarios.out)
#' @export 
gen_pricing_scenarios <- function(gen_var_scenarios.out, nscenarios=1, nyears=25, 
  params=var_params, seed=137531) {
  library(MASS)
  scenarios <- vector("list", nscenarios) # store the scenario matrices in a list
  
  # initialize some values outside loop
  Phi <- params$Phi
  Sigma <- params$Sigma
  L0 <- params$L0
  L1 <- params$L1
  mu <- params$mu 
  z0 <- params$z0 + mu # add back mean
  zt <- z0
  delta1 <- matrix(c(1,0,0,0), 1, 4)
  nmonths <- nyears * 12
  
  # calculate LT = L0 + L1 %*% zt -> 4x1
  LT <- L0 + L1 %*% zt 
  
  set.seed(seed)
  for(i in 1:nscenarios) {
    scenarios[[i]] <- numeric(nmonths)
    zscenarios <- gen_var_scenarios.out[[i]] # use zscenarios[,j] not [j,]
    rand <- mvrnorm(n = nmonths, mu=rep(0,4), Sigma=Sigma)
    for(j in 1:nmonths) { # TO DO: vectorize this part of the code
      mt <- delta1 %*% zt + 0.5%*%t(LT)%*%LT + t(LT)%*%rand[j,] # [j,] not [,j]
      zt <- zscenarios[,j] # change zt after calculating -mt+1
      LT <- L0 + L1 %*% zt # LT = L0 + L1 %*% zt
      
      # mt represents -mt+1. -mt+1 = -lnMt+1 -> mt+1 = lnMt+1 
      # -> Mt+1 = exp(mt+1), the stochastic discount factor
      scenarios[[i]][j] <- mt
    }
  }
  scenarios # return Mt
}