library(vars)
library(MASS)
marketData <- read.csv("~/Dropbox/Research/Data/VAR_DATA.csv")

# means 
mu <- matrix(c(mean(marketData$Salary),
               mean(marketData$Inflation),
               mean(marketData$Investment),
               mean(marketData$Treasury)), 4, 1)

# modify data by subtracting mean (new mean is 0)
varInput <- marketData[,2:5]
varInput$Salary <- varInput$Salary - mu[1,1]
varInput$Inflation <- varInput$Inflation - mu[2,1]
varInput$Investment <- varInput$Investment - mu[3,1]
varInput$Treasury <- varInput$Treasury - mu[4,1]
X0 <- matrix(as.numeric(tail(varInput,1)),4,1) # X0 - mu

varModel <- VAR(varInput, p=1, type="none")
Phi <- coef(varModel)
Phi <- matrix(c(Phi$Salary[,1], Phi$Inflation[,1],
                Phi$Investment[,1], Phi$Treasury[,1]), 4, 4, byrow=TRUE)
a <- summary(varModel)$covres

#' @param n number of scenarios
#' @param nyears number of years (since quarterly multiply by 4)
gen_var_scenarios <- function(X0, Phi, a, n=100, nyears=80) {
    paths <- vector("list", n) # store all n paths
    
    m <- nyears*4 # number of quarters
    path <- matrix(0, 4, m+1) # 4 state variables so 4 rows. m+1 to store initial value
    path[,1] <- X0
    set.seed(137531)
    for(i in 1:n) {
        #rand <- matrix(as.numeric(mvrnorm(m,mu=rep(0,4),Sigma=a)), 4, m) # generate all random numbers
        Xt <- X0
        for(j in 1:m) {
            rand <- matrix(as.numeric(mvrnorm(1,mu = rep(0,4), Sigma=a)), 4, 1)
            #Xt <- Phi%*%Xt + rand[,j] 
            Xt <- Phi%*%Xt + rand
            path[,j+1] <- Xt
        }
        paths[[i]] <- path
    }
    paths
}

gen_var_scenarios.out <- gen_var_scenarios(X0, Phi, a, n=1, nyears=25)
path <- gen_var_scenarios.out[[1]] # first scenario
path[1,] <- path[1,] + mu[1,1] # add mu back to path and analyze
path[2,] <- path[2,] + mu[2,1]
path[3,] <- path[3,] + mu[3,1]
path[4,] <- path[4,] + mu[4,1]

plot(ts(path[1,], start=2008, end=2032, frequency=4), ylab="", main="Simulated continuous real wage increase")
plot(ts(path[2,], start=2008, end=2032, frequency=4), ylab="", main="Simulated continuous inflation rate")
plot(ts(path[4,], start=2008, end=2032, frequency=4), ylab="", main="Simulated continuous 10-year treasury bond return")
plot(ts(path[3,], start=2008, end=2032, frequency=4), ylab="", main="Simulated continuous investment return (low risk)")