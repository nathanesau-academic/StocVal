cat("SOA scenarios 
-------------------------------\n")
soaScenarios <- bmarkScenarios("~/Dropbox/Research/StocVal/data/SOA/soascenario.csv")
soaterm1.mean <- meanScenarios(soaScenarios$term1)
soaterm10.mean <- meanScenarios(soaScenarios$term10)
cat("term 1 mean, 500 scenarios\n")
print(soaterm1.mean)

cat("Comparison with 1000 other stochastic scenarios
-------------------------------\n")
termStruct.out <- cubic_spline(termStruct);
termStruct.extension <- curve_extensionNS(termStruct.out);
credit_spread <- rep(0.01,100);
hw.out <- hull_white(srinput, termStruct.extension, termofyield=1);
cholesky.out <- cholesky(cholinput);
esga.out <- esga(esgin, credit_spread, volterm.equity, inflation_mean,
                 volterm.inflation , cholesky.out, termStruct.extension);
stochasticScenarios.out <- stochasticScenarios(hw.out, termStruct.extension,
                                               srinput, esga.out)
stochasticterm1.mean <- meanScenarios(stochasticScenarios.out$term1)
stochasticterm10.mean <- meanScenarios(stochasticScenarios.out$term10)
cat("see plots: SOA scenario mean is blue, 1000 other scenarios mean is red\n")
plot(x=seq(0,96,1), y=soaterm1.mean[1:97], main="SOA 500 scenarios vs 1000 other scenarios", type='l', col='blue',
  xlab="time", ylab="term1")
lines(x=seq(0,96,1), y=stochasticterm1.mean[1:97], type='l', col='red')
plot(x=seq(0,87,1), y=soaterm10.mean[1:88], main="SOA 500 scenarios vs 1000 other scenarios", type='l', col='blue',
     xlab="time", ylab="term10")
lines(x=seq(0,87,1), y=stochasticterm10.mean[1:88], type='l', col='red')

cat("Comparison with deterministic scenario (black line)
-------------------------------\n")
determScenario.out <- determScenario(termStruct.out)
deterterm1 <- determScenario.out$term1
deterterm10 <- determScenario.out$term10
cat("see plots\n")
plot(x=seq(0,96,1), y=soaterm1.mean[1:97], main="SOA 500 scenarios vs 1000 other scenarios", type='l', col='blue',
     xlab="time", ylab="term1")
lines(x=seq(0,96,1), y=stochasticterm1.mean[1:97], type='l', col='red')
lines(x=seq(0,96,1), y=deterterm1[1:97], type='l')
plot(x=seq(0,87,1), y=soaterm10.mean[1:88], main="SOA 500 scenarios vs 1000 other scenarios", type='l', col='blue',
     xlab="time", ylab="term10")
lines(x=seq(0,87,1), y=stochasticterm10.mean[1:88], type='l', col='red')
lines(x=seq(0,87,1), y=deterterm10[1:88], type='l')