library(vars)
library(MASS)

# READ IN MARKET DATA 
market_data <- 
    readRDS("~/Dropbox/Research/StocVal/data/Canada/varinput_canada.Rda")
yield_dataTS <- ts(market_data[,2:8],start = 1995, end=2015, freq=12)
stock_TS <- ts(market_data[,9], start=1995, end=2015, freq=12)
inflation_TS <- ts(market_data[,10], start=1995, end=2015, freq=12)

# PLOTS
plot(yield_dataTS, col=1:7, plot.type="single", ylab="Yield", 
    main="Historical continuously compounded monthly yields")
legend("topright", legend=colnames(yield_dataTS), ncol=2, lty=1, col=1:8)
plot(stock_TS, ylab="Stock return", main="Historical continuously compounded
     monthly stock returns")
plot(inflation_TS, ylab="Inflation", main="Historical continuously compounded
     monthly inflation rates")

# VAR DATA
var_data <- data.frame(
    inflation=market_data$inflation,
    tenyear=market_data$tenyear,
    stock=market_data$stock)

# subtract mean from VAR DATA
mu <- matrix(c(    mean(var_data$inflation),
                   mean(var_data$tenyear),
                   mean(var_data$stock)), 3, 1)

var_input <- data.frame(
    inflation=var_data$inflation - mu[1],
    tenyear=var_data$tenyear - mu[2],
    stock=var_data$stock - mu[3])

# fit var(1)
var_model <- VAR(var_input, p=1, type = "none")
Phi <- coef(var_model)
Phi <- matrix(c(Phi$inflation[,1],
    Phi$tenyear[,1],
    Phi$stock[,1]), 3, 3, byrow=TRUE)

Sigma <- summary(var_model)$covres 
print(summary(var_model))

# simulate VAR path
Xt <- matrix(as.numeric(tail(var_input, 1)), 3, 1)
path <- matrix(0, 3, 301)
path[,1] <- Xt
for(i in 1:300) {
    rand <- mvrnorm(n=1, mu=rep(0,3), Sigma=Sigma)
    Xt <- Phi %*% Xt + rand
    path[,i+1] <- Xt 
}

# plot extension
inflation_TS_combined <- ts(c(var_input$inflation, path[1,]),
    start=1995, end=2040, freq=12)
plot(inflation_TS_combined, ylab="Inflation", main="Historical continuously compounded
     monthly inflation rates", type='l')
abline(v=c(2015,6), lty=2)

stock_TS_combined <- ts(c(var_input$stock, path[3,]),
    start=1995, end=2040, freq=12)
plot(stock_TS_combined, ylab="Stock Price", main="Historical continuously compounded
    stock returns", type='l')
abline(v=c(2015,6), lty=2)