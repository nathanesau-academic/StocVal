library(vars)
library(MASS)
marketData <- read.csv("~/Dropbox/Research/Data_2015/VAR_DATA.csv")

par(mfrow=c(4,2))

plot(ts(marketData$Salary, start=1987, end=2015, frequency = 4),
     main="Continuous Real Wage Increase", ylab="")
plot(ts(marketData$Inflation, start=1987, end=2015, frequency=4),
     main="Continuous Inflation Rate", ylab="")
plot(ts(marketData$Treasury, start=1987, end=2015, frequency=4),
     main="Continuous 10-year treasury bond return", ylab="")
plot(ts(marketData$Investment, start=1987, end=2015, frequency=4),
     main="Continuous investment return (low risk)", ylab="")

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

# generating a VAR scenario
path <- matrix(0, 4, 101)
Xt <- X0
path[,1] <- Xt

delta1 <- matrix(c(1,0,0,0), 4, 1)
delta0 <- 0


set.seed(137531)
for(i in 1:100) {
    m <- matrix(as.numeric(mvrnorm(1,mu = rep(0,4), Sigma=a)), 4, 1)
    Xt <- Phi %*% Xt + m
    path[,i+1] <- Xt
}

# add mu back to path and analyze
path[1,] <- path[1,] + mu[1,1]
path[2,] <- path[2,] + mu[2,1]
path[3,] <- path[3,] + mu[3,1]
path[4,] <- path[4,] + mu[4,1]

plot(ts(path[1,], start=2008, end=2032, frequency=4), ylab="", main="Simulated continuous real wage increase")
plot(ts(path[2,], start=2008, end=2032, frequency=4), ylab="", main="Simulated continuous inflation rate")
plot(ts(path[4,], start=2008, end=2032, frequency=4), ylab="", main="Simulated continuous 10-year treasury bond return")
plot(ts(path[3,], start=2008, end=2032, frequency=4), ylab="", main="Simulated continuous investment return (low risk)")