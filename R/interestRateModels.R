#' @title Cubic Spline
#' @description Computes the cubic spline linear interpolation for unknown yield terms
#' @param termStruct Data frame containing term and USD risk free rate. Maximum term is 30.
#' @return A data frame containing the interpolated USD risk free rates.
#' @details This function must be called to perform the stochastic valuation 
#' @examples termStruct.out <- cubic_spline(termStruct)
#' @export
cubic_spline <- function(termStruct) {
  nrows <- length(termStruct[,1]) 
  ncols <- length(termStruct) 
  
  X <- termStruct[,1] 
  Y <- termStruct[,2] 
  H <- numeric(nrows) 
  D <- numeric(nrows) 
  a <- numeric(nrows)
  b <- numeric(nrows)
  C <- numeric(nrows) # C not c
  V <- numeric(nrows)
  M <- numeric(nrows)
  Z <- 0 
  W <- 0 
  Q <- 0 
  T <- 0 # overwrites T in .GlobalEnv
  S <- matrix(0,nrow=nrows,ncol=4) 
  termStruct.out <- matrix(0,nrow=30,ncol=2)
  
  H[1] <- X[2] - X[1]
  D[1] <- (Y[2] - Y[1])/ H[1]
  for(k in 2:(nrows-1)) { # calculates using recursion
    H[k] = X[k+1] - X[k] 
    D[k] = (Y[k+1] - Y[k])/H[k]
    a[k] = H[k]
    b[k] = 2*(H[k-1] + H[k])
    C[k] = H[k]
  }
  
  # second loop to calculate V
  for(k in 2:(nrows-1)) { 
    V[k] = 6*(D[k]-D[k-1])
  }
  
  b[2] = b[2] + H[1] + H[1]*H[1]/H[2]
  C[2] = C[2] - H[1] * H[1] / H[2]
  b[nrows-1] = b[nrows-1] + H[nrows-1] + H[nrows-1] * H[nrows -1]/H[nrows-2]
  a[nrows-2] = a[nrows-2] - H[nrows-1] * H[nrows-1] / H[nrows-2]
  
  for(k in 3:(nrows-1)) { 
    T =  a[k-1]/b[k-1]
    b[k] = b[k] - T*C[k-1]
    V[k] = V[k] - T*V[k-1]
  }
  
  M[nrows-1] = V[nrows-1]/b[nrows-1] 
  for(k in (nrows-2):2) {
    M[k] = (V[k] - C[k]*M[k+1])/b[k]
  }
  
  M[1] = M[2] - H[1] * (M[3] - M[2])/H[2]
  M[nrows] = M[nrows-1] + H[nrows-1] * (M[nrows-1] - M[nrows-2])/H[nrows-2]
  
  for(k in 1:(nrows-1)) { 
    S[k,1] = Y[k]
    S[k,2] = D[k] - H[k]*(2*M[k]+M[k+1])/6
    S[k,3] = M[k]/2
    S[k,4] = (M[k+1] - M[k])/(6*H[k])
  }
  
  Q = X[nrows]
  
  k = 0 # declare k outside loop
  for(i in 1:Q) {    
    for(j in 2:nrows) {
      if(X[j-1] <= i && i <= X[j]) {
        k = j-1
        j = nrows
      }
    }
    
    if(i==X[1]) {
      k = 1
    }
    
    W = i - X[k]
    Z = ((S[k,4]*W + S[k,3])*W + S[k,2])*W + S[k,1]
    
    for(j in 1:ncols) {
      if(j == 1) {
        termStruct.out[i,j] = i
      } else {
        termStruct.out[i,j] = Z 
      }
    }
  }
  termStruct.out = as.data.frame(termStruct.out)
  names(termStruct.out) = c("Term", "RFR")
  termStruct.out
}

#' @title Curve Extension (Swap, constant forward)
#' @description Calculates the curve extension until time 100 using swap (constant forward)
#' @param termStruct.out Contains the linearly interpolated risk free rates 
#'  from cubic_spline()
#' @return The curve extension for an additional 70 years (effective annual zero rate)
#' @examples termStruct.out <- cubic_spline(termStruct)
#'  termStruct.extension <- curve_extensionCF(termStruct.out)
#' @export
curve_extensionCF <- function(termStruct.out) {
  zeroRate <- termStruct.out[,2] 
  contZeroRate <- log(1+zeroRate)
  zcb <- exp(-(1:30)*contZeroRate)
  symZcb <- cumsum(zcb)
   
  last2 <- c(20,30) # last two liquid terms
  spotLast2 <- c(zeroRate[last2[1]], zeroRate[last2[2]])
  longFR <- ((1+spotLast2[2])^last2[2]/(1+spotLast2[1])^last2[1])^(1/(last2[2]-last2[1])) - 1
             
  longZcb <- c(zcb, numeric(70)) # additional 70 years
  longZeroRate <- c(zeroRate, numeric(70)) 
  for(i in 31:100) {
    longZcb[i] = longZcb[i-1]/(1+longFR)
    longZeroRate[i] = longZcb[i]^(-1/i) - 1
  }
  longZeroRate
}
   
#' @title Curve Extension (Swap, Cubic Spline and N-S)
#' @description Calculates the curve extension until time 100 using swap (Cubic Spline and N-S)
#' @param termStruct.out Contains the linearly interpolated risk free rates 
#'  from cubic_spline()
#' @param params parameters for the Neilson and Siegel extrapolation method
#' @return The curve extension for an additional 70 years (continuous zero rate)
#' @examples termStruct.out <- cubic_spline(termStruct)
#'  termStruct.extension <- curve_extensionNS(termStruct.out)
#' @details N-S refers to the Nelson & Siegel Extrapolation for Spot Rates. The formula is: 
#'  rtm <- BO + B1(1 - exp(-m/tau1))/(m/tau1) + B2((1-exp(-m/tau2))/(m/tau2) - exp(-m/tau2)),
#'  where rtm is the spot rate in term m.
#'  
#' The implied forward rate could be calculated by placing the following code into the 
#' function:
#'  
#'  impliedFR <- numeric(100)
#'  
#'  for(i in 0:99) impliedFR[i+1] = longZcb[i+1]/longZcb[i+2] - 1
#'
#'  impliedFR <- c(1/longZcb[1]-1, head(impliedFR,-1))
#' @export
curve_extensionNS <- function(termStruct.out, parameters=NS_params) {
  zeroRate <- termStruct.out[,2] 
  contZeroRate <- log(1+zeroRate)
  zcb <- exp(-(1:30)*contZeroRate)
  symZcb <- cumsum(zcb)
  
  # hard coded parameter values 
  m <- parameters[1]
  B0 <- parameters[2]
  B1 <- parameters[3]
  B2 <- parameters[4]
  tau1 <- parameters[5]
  tau2 <- parameters[6]
  
  t <- 1:100
  contSR <- B0 + B1*((1-exp(-t/tau1))/(t/tau1)) + B2*((1-exp(-t/tau2))/(t/tau2) - exp(-t/tau2))
  annualSR <- exp(contSR) - 1
  longZcb <- c(zcb, numeric(70)) # additional 70 years
  longZeroRate <- c(zeroRate, numeric(70)) # additional 70 years
  
  for(i in 31:100) {
    longZeroRate[i] <- annualSR[i]
    longZcb[i] <- (1+annualSR[i])^(-i)
  }

  contZR <- log(1/longZcb[1:99])/(1:99)
  c(contZR,0) # c with 0 to be make length 100
}

#' @title 1-factor Hull White
#' @param srinput Hull White model parameters. c(b, tau, scen, prjy, rns)
#' @param termStruct.extension output of curve_extensionNS() 
#' @param termofyield should be 1 or function won't work 
#' @description Short rate interest model used to generate stochastic scenarios
#' @return Stochastic short rate scenarios
#' @examples termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  hw.out <- hull_white(srinput, termStruct.extension, termofyield=1)
#' @details In srinput, scen represents the number of scenarios to generate, 
#'  prjy represents the number of projected years, and rns represents the 
#'  random number seed. It should also be pointed out that hull_white() can
#'  also be used to perform credit spread scenario generation not just short rate
#'  scenario generation (see interestModelsDemo)
#' @export
hull_white <- function(srinput, termStruct.extension, termofyield=1) {
  ncols <- 1 # srinput is 1D vector
  nrows <- length(srinput) 
  ncols2 <- 1 # termStruct.extension is 1D vector
  nrows2 <- length(termStruct.extension)
  
  # input parameters
  b <- srinput[1]
  tau <- srinput[2]
  scen <- srinput[3]
  prjy <- srinput[4]
  rns <- srinput[5]
  
  # vectors and matrices
  T <- numeric(100) # overwrites T in .GlobalEnv
  RR <- matrix(0,scen,prjy)
  R <- numeric(100)
  X1 <- termStruct.extension # copy vector to X1
  
  # solve theta
  T[1] <- X1[2]*2 + 0.5*tau^2 - (1+b)*X1[1]
  for(j in 4:nrows2) {
    tmp <- 0
    tj <- j-1 # in R indexes start at 1. However, I want lower index in calculation
    for(l in 1:(j-3)) {
      tl <- l - 1
      tmp <- tmp - T[l] * (1 - b^(tj-tl-1))/(1-b)
    }        
    T[j-2] <- X1[j] * tj + tmp + 0.5*tau^2*(tj-1-2*b*(1-b^(tj-1))/(1-b) +
                  b^2*(1-b^(2*tj-2)) / (1-b^2))/(1-b)^2 - X1[1]*(1-b^tj)/(1-b)
  }
  
  # scenario generation (random numbers) -> TO DO: vectorized will be faster
  # possibly set seed before this loop since call is made to rnorm
  set.seed(rns)    
  for(p in 1:scen) {
    R[1] <- X1[1]
    RR[p,1] <- X1[1]
    for(q in 2:prjy) {
      n <- rnorm(1) # random number
      R[q] <- b*R[q-1] + T[q] + tau * n
      RR[p,q] <- R[q]
    }
  }
  
  sroutput <- matrix(0, 1000, 100+1)
  for(i in 1:scen) {
    for(j in 1:prjy) {
      if(termofyield==1) {
        sroutput[i,j] <- max(0.001,RR[i,j]) # ensure positive
      } else {
        sroutput[i,j] <- 0 # TO DO: needs further improvement
      }
    }
  }
  sroutput <- as.data.frame(sroutput)
  names(sroutput) <- 0:100
  sroutput
}

#' @title 1-factor Hull White (bond prices)
#' @description Transform hull white short rates into bond prices -> P = 1/(1+r)
#' @param hw.out Output of hull_white() containing short rate scenarios
#' @return Bond prices corresponding to hull white short rate scenarios
#' @examples termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  hw.out <- hull_white(srinput, termStruct.extension, termofyield=1);
#'  hw.bond <- hull_whiteBond(hw.out) 
#' @export
hull_whiteBond <- function(hw.out) {
  nrow <- length(hw.out[,1])
  hw.bond <- matrix(0, nrow, 100)
  hw.bond[,1] <- 1
  for(i in 2:100) { # assume 100 scenarios for now
    hw.bond[,i] <- hw.bond[,i-1]/(1+hw.out[,(i-1)])
  }
  hw.bond <- data.frame(hw.bond)
  names(hw.bond)=0:99
  hw.bond
}

#' @title 1-factor Hull White Theta
#' @description Calculates theta for 1-factor hull white short rate model
#' @param srinput Hull White model parameters. c(b, tau, scen, prjy, rns)
#' @param termStruct.extension output of curve_extensionNS()
#' @return A theta vector
#' @examples termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  hw.theta <- hw_theta(srinput, termStruct.extension)
#' @export 
hw_theta <- function(srinput, termStruct.extension) {
  ncols <- 1 # srinput is 1D vector
  nrows <- length(srinput) 
  ncols2 <- 1 # termStruct.extension is 1D vector
  nrows2 <- length(termStruct.extension)
  
  # input parameters
  b <- srinput[1]
  tau <- srinput[2]
  scen <- srinput[3]
  prjy <- srinput[4]
  rns <- srinput[5]
  
  # vectors and matrices
  T <- numeric(100) # overwrites T in .GlobalEnv
  RR <- matrix(0,scen,prjy)
  R <- numeric(100)
  X1 <- termStruct.extension # copy vector to X1
  
  # solve theta
  T[1] <- X1[2]*2 + 0.5*tau^2 - (1+b)*X1[1]
  for(j in 4:100) {
    tmp <- 0
    tj <- j-1
    for(l in 1:(j-3)) {
      tl <- l - 1
      tmp <- tmp - T[l] * (1 - b^(tj-tl-1))/(1-b)
    }        
    T[j-2] <- X1[j] * tj + tmp + 0.5*tau^2*(tj-1-2*b*(1-b^(tj-1))/(1-b) +
          b^2*(1-b^(2*tj-2)) / (1-b^2))/(1-b)^2 - X1[1]*(1-b^tj)/(1-b)
  }
  T 
}

#' @title N-year zero rates 
#' @description Derives N-year zero rates from 1-factor hull white 
#' @param hw.out output of hull_white()
#' @param termStruct.extension output of curve_extensionNS()
#' @param srinput Hull-white model parameters
#' @return A data frame containing n-year zero rates for each scenario
#' @examples termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  hw.out <- hull_white(srinput, termStruct.extension, termofyield=1);
#'  nyearZero.out <- nyearZero(hw.out, termStruct.extension, srinput, term=10)
#' @details Due to the way in which theta was calculated, the last three columns 
#'  of hw.out are not accurate. Therefore, the last four columns of nyearZero(..., term=2)
#'  are not accurate, and so on. Also note that ,
#'  nyearZero(,...term=1) and hw.out are equivalent
#' @export 
nyearZero <- function(hw.out, termStruct.extension, srinput, term=10) {  
  nyear <- termStruct.extension[term]
  nrow <- length(hw.out[,1])
  nyearZero_out <- matrix(0,nrow,100+1)
  nyearZero_out[,1] <- rep(nyear, nrow)
 
  AA <- function(theta,b,tau,T,TT) {
    total <- 0
    if((TT-2) >= T) {
      for(l in T:(TT-2)) {
        total <- total - BB(b,TT-l-1) *theta[l+1+1]
      }
    }
    total + 0.5*tau^2 * CC(b,TT-T-1)
  }
  
  BB <- function(b,N) {
    (1-b^N)/(1-b)
  }
  
  CC <- function(b,N) {
    (N-2*b*BB(b,N) + b^2*BB(b^2,N))/(1-b^2)
  }
  
  b <- srinput[1]
  tau <- srinput[2]
  theta <- hw_theta(srinput, termStruct.extension) # TO DO: cache theta 
  
  for(i in 2:100) {
    tmp <- BB(b, term)/term 
    tmp2 <- AA(theta, b, tau, i-1, i-1+term)
    nyearZero_out[,i] <- tmp * hw.out[,i] - tmp2/term
  }
  nyearZero_out <- data.frame(nyearZero_out)
  names(nyearZero_out) <- seq(0,100,1)
  nyearZero_out
}

#' @title Par Yield
#' @description Calculates par yield using results of Hull White scenario generation
#' @param hw.out output of hull_white()
#' @param termStruct.extension output of curve_extensionNS()
#' @param srinput Hull-white model parameters
#' @param term The term of the par yield
#' @return A data frame containing par yield rates for each scenario
#' @examples termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  hw.out <- hull_white(srinput, termStruct.extension, termofyield=1);
#'  parYield.out <- parYield(hw.out, termStruct.extension, srinput, term=5)
#' @details hw.out uses a term of 10 years. This function allows for a different
#'  term (i.e. 5 years). However, it has three nested loops, so can take a while.
#' @export 
parYield <- function(hw.out, termStruct.extension, srinput, term=5) { 
  # input parameters
  b <- srinput[1]
  tau <- srinput[2]
  scen <- srinput[3] # no. of simulations
  prjy <- srinput[4]
  rns <- srinput[5]
  
  AA <- function(theta,b,tau,T,TT) {
    total <- 0
    for(l in T:(TT-2)) {
      total <- total - BB(b,TT-l-1) *theta[l+1+1]
    }
    total + 0.5*tau^2 * CC(b,TT-T-1)
  }
  
  BB <- function(b,N) {
    (1-b^N)/(1-b)
  }
  
  CC <- function(b,N) {
    (N-2*b*BB(b,N) + b^2*BB(b^2,N))/(1-b^2)
  }
  
  # vectors and matrices
  R <- numeric(100) 
  disc_fact <- numeric(100) 
  Z <- matrix(0, 100, 100) 
  par_yield <- matrix(0, 100, 100)
  disc_factor <- matrix(0, 100, 100) 
  ann_factor <- matrix(0, 100, 100) 
  theta <- hw_theta(srinput,termStruct.extension) 
  nrow <- length(hw.out[,1])
  sroutput <- matrix(0, nrow, 100)
  
  for(i in 1:scen) { # iterate over all scenarios
    R <- as.numeric(hw.out[i,])
    for(t in 1:(100-term-1)) { # 1:93 
      ann_factor[t, 1] = 0
      for(j in 1:term) { # 1:5
        if(j==1) {
          Z[t,t+j] <- R[t]
        } else {
          Z[t,t+j] <- BB(b,j)/j * R[t] - AA(theta, b, tau, t-1, t+j-1)/j
        }
        
        disc_factor[t,j] <- exp(-j*Z[t,t+j])
        
        if(j==1) {
          ann_factor[t,j] <- disc_factor[t,j]
        } else {
          ann_factor[t,j] <- ann_factor[t,j-1] + disc_factor[t,j]
        }
        
        if(j==1) {
          par_yield[t,j] <- exp(Z[t,t+j]) - 1
        } else {
          par_yield[t,j] <- (1-disc_factor[t,j])/(ann_factor[t,j-1] + disc_factor[t,j])
        }
      }
    }
    sroutput[i,] <- par_yield[,term]
  }
  sroutput <- data.frame(sroutput)
  names(sroutput) <- seq(0,99,1)
  sroutput
}

#' @title Equity return scenario generation
#' @description Generates stochastic scenarios for the excess equity return
#' @param rpi Equity-risk premium input parameters
#' @param volterm Volatility for bonds with terms 1 to 100 length inclusive (equity)
#' @param termStruct.extension output of curve_extensionNS()
#' @return The equity excess return for each scenario
#' @examples termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  ers.out <- ers(rpi, volterm, termStruct.extension)
#' @export
ers <- function(rpi, volterm, termStruct.extension) {
  ncols <- 1 # rpi is 1D vector
  nrows <- length(rpi)
  ncols2 <- 1 # volterm is 1D vector
  nrows2 <- length(volterm)
  
  # input parameters
  rp <- rpi[1] # risk premium
  scen <- rpi[2] # number of scenarios
  prjy <- rpi[3] # projection years
  rns <- rpi[4] # random number seed
  
  X1 <- volterm # copy to X1
  X2 <- termStruct.extension # copy to X2
  Z <- matrix(0, scen, prjy) # init matrix
  
  set.seed(rns)
  for(p in 1:scen) { # random number generation
    for(q in 1:prjy) {
      n <- rnorm(1)
      Z[p,q] <- exp(rp+X2[q]*0 - X1[q] * X1[q] / 2 + X1[q] * n) - 1
    }
  }
  Z <- data.frame(Z)
  names(Z) <- 1:100
  Z
}

#' @title Total equity return for scenarios
#' @description Derives the equity total return from the excess equity return. 
#'  Note this method simply adds two data frames.
#' @param hw.out output of hull_white()
#' @param ers.out output of ers()
#' @return A data frame containing the equity total return for each scenario
#' @examples termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  ers.out <- ers(rpi, volterm, termStruct.extension) 
#'  ersTotal.out <- ersTotal(ers.out)
#' @export 
ersTotal <- function(hw.out, ers.out) {
  nrow <- length(hw.out[,1])
  ersTotal.out <- matrix(0,nrow,100)
  for(i in 1:nrow) { # row
    for(j in 1:100) { # col
      ersTotal.out[i,j] <- hw.out[i,j] + ers.out[i,j] # TO DO: add df's directly
    }
  }
  ersTotal.out
}

#' @title Inflation scenario generation
#' @description Generate inflation scenarios using calibrated inflation mean and 
#'  volatility information
#' @param inflation_in Parameters for the simulation scenario generation
#' @param inflation_mean Inflation mean for each term
#' @param volatility.inflation Implied volatility term structure
#' @return A data frame containing inflation scenarios for each term
#' @examples termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  inflationrs.out <- inflationrs(inflation_in, inflation_mean, 
#'    volatility.inflation)
#' @export
inflationrs <- function(inflation_in, inflation_mean, volatility.inflation) {  
  # input parameters
  scen <- inflation_in[1]
  prjy <- inflation_in[2]
  rns <- inflation_in[3]
  
  X1 <- inflation_mean # copy to X1
  Y1 <- volatility.inflation # copy implied volatility term structure to Y1
  set.seed(rns)
  Z <- matrix(0, scen, prjy)
  
  # generation of random numbers
  for(p in 1:scen) {
    for(q in 1:prjy) {
      n <- rnorm(1)
      Z[p,q] <- exp(X1[q] - Y1[q] * Y1[q] / 2 + Y1[q] * n) - 1
    }
  }
  Z <- data.frame(Z)
  names(Z) <- 1:100
  Z
}

#' @title Cholesky Decomposition
#' @description Computes the Cholesky Decomposition of a correlation matrix
#' @param cholinput a 4x4 correlation matrix for the short rate, credit spread,
#'  excess return and inflation rate
#' @return a 4x4 matrix representing the Cholesky decomposition
#' @examples cholesky.out <- cholesky(cholinput)
#' @export
cholesky <- function(cholinput) {
  ncols <- length(cholinput[1,])
  nrows <- length(cholinput[,1]) 
  a <- cholinput # copy cholinput to a
  P <- numeric(nrows)
  
  total <- 0 # declare total outside the loop
  for(i in 1:nrows) {
    for(j in i:ncols) {
      total <- a[i,j]
      if((i-1)>=1) {
        for(k in (i-1):1) {
          total <- total - a[i,k] * a[j,k]
        }
      }
      if(i==j) {
        if(length(total) >0 )
          P[i] <- sqrt(total)
      } else {
        if(length(total) > 0)
          a[j,i] <- total/P[i]
      }
    }
  }
  a # return is a matrix (not data frame)
}

#' @title ESGA
#' @description Holistic ESG for short rate, credit spread, equity 
#'              return and inflation with correlation
#' @param esgin ESG scenario generation input
#' @param credit_spread Volatility for credit spreads 
#' @param volterm.equity Volatility for bonds with terms 1 to 100 length inclusive
#' @param inflation_in Inputs for inflation scenario generation
#' @param cholesky.out output of cholesky()
#' @param termStruct.extension output of curve_extensionNS()
#' @return A list of data frames with values for scenario 1 to 1000 and terms 0 to 99
#' @examples termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  credit_spread <- rep(0.01,100);
#'  cholesky.out <- cholesky(cholinput);
#'  esga.out <- esga(esgin, credit_spread, volterm.equity, inflation_mean,
#'  volterm.inflation , cholesky.out, termStruct.extension)
#' @details List indexes: Index 1 is short rate, Index 2 is credit spread, Index 3 
#'  is total equity return and Index 4 is inflation rate. An important feature
#'  of this function is that it uses the Cholesky decomposition of the 
#'  variance-covariance matrix between these varaibles.
#' @export
esga <- function(esgin, credit_spread, volterm.equity, inflation_mean, 
  volterm.inflation, cholesky.out, termStruct.extension) {
  # input parameters
  b <- esgin[1] # from srinput
  tau <- esgin[2] # from srinput
  rp <- esgin[3] # from rpi
  bc <- esgin[4] # from crspinput
  tauc <- esgin[5] # from crspinput
  scen <- esgin[6] # from srinput
  prjy <- esgin[7] # from srinput
  rns <- esgin[8] # from srinput
  
  X <- termStruct.extension 
  XC <- credit_spread 
  XE <- volterm.equity
  XI <- inflation_mean
  YI <- volterm.inflation
  CORR <- cholesky.out  
  
  # dim
  ncols <- 1 # 1D vector
  nrows <- length(esgin)
  ncols2 <- 1 # 1D vector
  nrows2 <- length(termStruct.extension)
  ncols3 <- 1 # 1D vector
  nrows3 <- length(credit_spread)
  ncols4 <- 1 # 1D vector
  nrows4 <- length(volterm.equity)
  ncols5 <- 2 # inflation_mean and volterm.inflation
  nrows5 <- 100 # length(inflation_mean)
  ncols6 <- length(cholesky.out) # matrix
  nrows6 <- length(cholesky.out[,1]) # matrix
  
  # theta vectors
  T <- numeric(nrows2) # overwrites T in .GlobalEnv 
  TC <- numeric(nrows3) # theta prime
  
  # solve theta
  T[1] <- X[2]*2 + 0.5*tau^2 - (1+b)*X[1]
  for(j in 4:nrows2) {
    tmp <- 0
    tj <- j-1
    for(l in 1:(j-3)) {
      tl <- l - 1
      tmp <- tmp - T[l] * (1 - b^(tj-tl-1))/(1-b)
    }        
    T[j-2] <- X[j] * tj + tmp + 0.5*tau^2*(tj-1-2*b*(1-b^(tj-1))/(1-b) +
      b^2*(1-b^(2*tj-2)) / (1-b^2))/(1-b)^2 - X[1]*(1-b^tj)/(1-b)
  }
  
  # solve theta prime
  TC[1] <- XC[2]*2 + 0.5*tauc^2 - (1+bc)*XC[1] 
  for(j in 4:nrows3) {
    tmp <- 0
    tj <- j-1
    for(l in 1:(j-3)) {
      tl <- l - 1
      tmp <- tmp - TC[l] * (1 - bc^(tj-tl-1))/(1-bc)
    }        
    TC[j-2] <- XC[j] * tj + tmp + 0.5*tauc^2*(tj-1-2*bc*(1-bc^(tj-1))/(1-bc) +
      bc^2*(1-bc^(2*tj-2)) / (1-bc^2))/(1-bc)^2 - XC[1]*(1-bc^tj)/(1-bc)
  }
  
  R <- numeric(100)
  RC <- numeric(100) 
  Z <- list(matrix(0, scen, prjy),
            matrix(0, scen, prjy),
            matrix(0, scen, prjy),
            matrix(0, scen, prjy)) # holds scenario generation for 4 indexes
  randcorr <- numeric(4)
  
  # scenario generation (random numbers)
  set.seed(rns)
  for(p in 1:scen) {
    R[1] <- X[1]
    Z[[1]][p,1] <- R[1]
    RC[1] <- XC[1]
    Z[[2]][p,1] <- RC[1]
    for(q in 1:prjy) {
      n <- rnorm(4) # vector with 4 random numbers
      for(j in 1:4) {
        tmp <- 0
        for(k in 1:4) {
          tmp <- tmp + CORR[j,k] * n[k]
        }
        randcorr[j] <- tmp
      }
      if(q!=1) {
        R[q] <- b*R[q-1] + T[q-1] + tau * randcorr[1]
        Z[[1]][p,q] <- max(0.001, R[q]) # makes sure R[q] is positive
        RC[q] <- bc*RC[q-1] + TC[q-1] + tauc * randcorr[2]
        Z[[2]][p,q] <- RC[q]
      }
      Z[[3]][p,q] <- exp(rp - XE[q] * XE[q] /2 + XE[q] * randcorr[3] + max(0.001, R[q])) - 1
      Z[[4]][p,q] <- exp(XI[q] - YI[q]*YI[q] / 2 + YI[q] * randcorr[4]) - 1
    }
  }
  for(i in 1:4) {
    Z[[i]] <- data.frame(Z[[i]])
    names(Z[[i]]) <- seq(0,99,1)
  }
  Z
}

#' @title Generate stochastic scenarios 
#' @description Convert esga() output into a form that can be read by 
#'  the valuation functions
#' @param hw.out output of hull_white()
#' @param termStruct.extension output of curve_extensionNS()
#' @param srinput hull white input parameters
#' @param esga.out output of esga() 
#' @return A list containing interest rates for various terms, inflation rate,
#'  credit rate, total yield, equity index and bond index
#' @examples termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  credit_spread <- rep(0.01,100);
#'  hw.out <- hull_white(srinput, termStruct.extension, termofyield=1);
#'  cholesky.out <- cholesky(cholinput);
#'  esga.out <- esga(esgin, credit_spread, volterm.equity, inflation_mean,
#'  volterm.inflation , cholesky.out, termStruct.extension);
#'  stochasticScenarios.out <- stochasticScenarios(hw.out, termStruct.extension,
#'  srinput, esga.out)
#' @export
stochasticScenarios <- function(hw.out,termStruct.extension,srinput,esga.out) {
  scenarios <- list()
  terms <- c(1,2,3,5,7,10,15,20,30)
  numTerms <- length(terms)
  for(i in 1:numTerms) {
    scenarios[[i]] <- nyearZero(hw.out,termStruct.extension,srinput,terms[i])
  }
  
  scenarios[[numTerms+1]] <- esga.out[[4]] # inflation rate
  scenarios[[numTerms+2]] <- esga.out[[2]] # credit rate
  scenarios[[numTerms+3]] <- ersTotal(scenarios[[1]], scenarios[[numTerms+2]]) # tot. yield
  
  nrow <- length(hw.out[,1])
  equityMatrix <- matrix(0, nrow, 100)
  for(i in 1:nrow) {
    tmp <- as.numeric(esga.out[[3]][i,])
    equityIndex <- numeric(100)
    equityIndex[1] <- 1
    for(j in 2:100) {
      equityIndex[j] <- equityIndex[j-1]*(1+tmp[j-1])
    }
    equityMatrix[i,] <- equityIndex
  }
  
  scenarios[[numTerms+3+1]] <- equityMatrix # equity index for each scenario
  scenarios[[numTerms+4+1]] <- 1/hull_whiteBond(hw.out) # bond index
  
  names(scenarios) <- c("term1", "term2", "term3", "term5", "term7", 
    "term10","term15","term20","term30", "inflation", "credit", "total", 
    "equity", "bond")
  scenarios
}

#' @title Generate a deterministic scenario
#' @description Generates deterministic scenario for variables
#'  interest rate term, inflation rate, credit spread, total yield, equity
#'  index, and bond index
#' @param termStruct.out output of cubic_spline()
#' @param params Neilson and Siegel parameters for calculating the curve extension
#' @return A list containing interest rates for various terms, inflation rate,
#'  credit rate, total yield, equity index and bond index
#' @details output is similar to stochastic scenarios except that 
#'  only one scenario is returned that the scenario is not random.
#'  Note that time goes from 0 to 99 for this scenario.
#' @examples termStruct.out <- cubic_spline(termStruct)
#'  determScenario.out <- determScenario(termStruct.out)
#' @export 
determScenario <- function(termStruct.out, params=NS_params) {
  # this part of code is copied from curve_extensionNS
  zeroRate <- termStruct.out[,2] 
  contZeroRate <- log(1+zeroRate)
  zcb <- exp(-(1:30)*contZeroRate)
  symZcb <- cumsum(zcb)
  
  # hard coded parameter values 
  m <- params[1]
  B0 <- params[2]
  B1 <- params[3]
  B2 <- params[4]
  tau1 <- params[5]
  tau2 <- params[6]  
  
  t <- 1:100
  contSR <- B0 + B1*((1-exp(-t/tau1))/(t/tau1)) + B2*((1-exp(-t/tau2))/(t/tau2) - exp(-t/tau2))
  annualSR <- exp(contSR) - 1
  longZcb <- c(zcb, numeric(70)) # additional 70 years
  longZeroRate <- c(zeroRate, numeric(70)) # additional 70 years
  
  for(i in 31:100) {
    longZeroRate[i] <- annualSR[i]
    longZcb[i] <- (1+annualSR[i])^(-i)
  }
  longZcb <- c(1, longZcb)
  
  scenario <- list()
  terms <- c(1,2,3,5,7,10,15,20,30)
  numTerms <- length(terms)
  for(i in 1:numTerms) {
    impliedFR <- numeric(100)
    for(j in 0:99) {
      impliedFR[j+1] <- (longZcb[j+1]/longZcb[j+terms[i]+1])^(1/terms[i]) - 1
    }
    scenario[[i]] <- impliedFR
  }

  equityIndex <- numeric(100)
  equityIndex[1] <- 1
  bondIndex <- equityIndex # copy to bondIndex
  rp <- 0 # risk premium = 0 since risk neutral
  for(j in 2:100) {
    equityIndex[j] <- equityIndex[j-1]*(1+scenario[[1]][j-1]+rp)
    bondIndex[j] <- bondIndex[j-1]*(1+scenario[[1]][j-1])
  } # if rp = 0 then bondIndex == equityIndex

  scenario[[numTerms+1]] <- rep(0.023, 100) # inflation rate
  scenario[[numTerms+2]] <- rep(0.01,100) # credit rate
  scenario[[numTerms+3]] <- scenario[[numTerms+2]] + scenario[[1]]
  scenario[[numTerms+3+1]] <- equityIndex
  scenario[[numTerms+4+1]] <- bondIndex
  names(scenario) <- c("term1", "term2", "term3", "term5", "term7", 
    "term10","term15","term20","term30", "inflation", "credit", "total", 
    "equity", "bond")
  scenario
}