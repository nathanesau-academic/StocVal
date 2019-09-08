#' @title Cost of European Call Option
#' @description Calculates the cost of a European call option under the 
#'  black scholes model.
#' @param S The current price of the underlying asset (i.e. stock)
#' @param K The strike / exercise price of the option
#' @param r The risk free interest rate
#' @param sigma The volatility of the option
#' @param d The continuously compounded dividend rate
#' @param term The time to expiration
#' @return The cost of a European call option
#' @examples eur_call_bsm(40,40,0.05,0.20,0.02,1)
#' @details bsm stands for the black scholes model.
#' @export  
eur_call_bsm <- function(S,K,r,sigma,d,term) {
  term_sqrt <- sqrt(term) # cache sqrt
  d1 <- (log(S/K) + (r-d)*term)/(sigma*term_sqrt) + 0.5*sigma*term_sqrt
  d2 <- d1 - (sigma*term_sqrt)
  S*exp(-d*term) * pnorm(d1) - K*exp(-r*term)*pnorm(d2)
}

#' @title Helper function for floorOptionPBO
#' @description Used to calculate PBO for closed form valuation
#' @param x Actual age
#' @param r Retirement age
#' @param survFactor Survival factor for the current year
#' @param discFactor Discount factor for the current year
#' @param bondIndex The bond index for the current year
#' @param av The account value at the beginning of the year
#' @param vol The volatility on a long term treasury bond
#' @param floor The floor rate
#' @return The PBO for the given input parameters
#' @export
CBP3CFV <- function(x, r, survFactor, discFactor,
  bondIndex, av, vol, floor) {
  # input
  surv2 <- survFactor[1]
  maxTerm <- r - x
  
  total <- 0
  for(i in 1:maxTerm) {
    surv <- surv2 
    surv2 <- survFactor[i+1]
    decr <- surv - surv2
    disc <- discFactor[i+1]/discFactor[1]
    bond <- bondIndex[i+1]/bondIndex[1]
    temp_r <- -log(disc)/i
    tmp_r2 <- -log(bond)/i
    P <- av[1]
    C <- eur_call_bsm(S=1,
      K=2+floor-(disc*bond)^(1/i), 
      r=temp_r, 
      sigma=vol, 
      d=0, 
      term=1)
    
    mytmp <- ((1+floor)*exp(-temp_r) + C)^i *  P - P * bond * disc
    if(i!=maxTerm) {
      total <- total + max(0, mytmp)*decr
    } else {
      total <- total + max(0, mytmp)*surv
    }
  }
  total
}