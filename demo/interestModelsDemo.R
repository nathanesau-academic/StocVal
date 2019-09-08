cat("Term Structure of interest rates
-------------------------------\n")
print(termStruct, row.names=F)

cat("\nCubic Spline Interpolation
-------------------------------\n")
termStruct.out <- cubic_spline(termStruct)
print(termStruct.out, row.names=F)

cat("\nCurve Extension (Neilson and Siegel Extrapolation)
-------------------------------\n")
termStruct.extension <- curve_extensionNS(termStruct.out)
print(termStruct.extension, row.names=F)

cat("\nHull White model Parameters
-------------------------------\n")
cat("b:                ", srinput[1], "\n")
cat("tau:              ", srinput[2], "\n")
cat("# of scenarios:   ", srinput[3], "\n")
cat("projection years: ", srinput[4], "\n")
cat("Random no. seed:  ", srinput[5], "\n")

cat("\nHull White: scenarios 1-5
-------------------------------\n")
cat("see plots (note that all scenarios end at y=0)\n")
# note that last 3 columns are not "accurate" in the model due to hw_theta method
# -> this is how it is supposed to be
hw.out <- hull_white(srinput, termStruct.extension, termofyield=1)
plot(x=seq(0,100-4,1),y=hw.out[1,1:(100-3)],xlab="Time", ylab="Interest Rate", type='l', col=1, 
     ylim=c(0,0.20), main="Hull White")
for(i in 2:5)
   lines(x=seq(0,100-4,1),y=hw.out[i,1:(100-3)],xlab="Time", ylab="Interest Rate", type='l', col=i)

cat("\n Hull White Bond Prices: scenarios 1-5
-------------------------------\n")
cat("see plots\n")
hw.bond <- hull_whiteBond(hw.out)
plot(x=seq(0,99,1),y=hw.bond[1,],xlab="Time", ylab="Bond Price", type='l', col=1, 
     ylim=c(0,1), main="Hull White Bond")
for(i in 2:5)
  lines(x=seq(0,99,1),y=hw.bond[i,],xlab="Time", ylab="Bond Price", type='l', col=i)

cat("N-year zero rates with N=5
-------------------------------\n")
cat("see plots\n")
nyearZero.out <- nyearZero(hw.out,termStruct.extension,srinput,term = 5)
plot(x=seq(0,(100-5-3),1),y=nyearZero.out[1,1:(100-5-2)],xlab="Time", ylab="N-year zero rate", type='l', col=1, 
     ylim=c(0,0.20), main="N-year zero rates (term=5)")
for(i in 2:5)
  lines(x=seq(0,(100-5-3),1),y=nyearZero.out[i,1:(100-5-2)],xlab="Time", ylab="N-year zero rate", type='l', col=i)

# EDIT: COMMENTED OUT BECAUSE THIS FUNCTION TAKES A LONG TIME TO RUN
# cat("\n Par Yield: scenarios 1-5
# -------------------------------\n")
# cat("see plots\n")
# parYield.out <- parYield(hw.out, termStruct.extension, srinput, term=5) # takes long time to run
# plot(x=seq(0,(100-5-3),1),y=parYield.out[1,1:(100-5-2)],xlab="Time", ylab="Par Yield", type='l', col=1, 
#      ylim=c(0,0.20), main="Par Yield (term=5)")
# for(i in 2:5)
#   lines(x=seq(0,(100-5-3),1),y=parYield.out[i,1:(100-5-2)],xlab="Time", ylab="Par Yield", type='l', col=i)

cat("\n Credit Rate: scenarios 1-5
-------------------------------\n")
cat("see plots\n")
credit_spread <- rep(0.01,100)
hw.out2 <- hull_white(crspinput, credit_spread, termofyield=1) # similar to short rate scenario generation
plot(x=seq(0,100-3,1),y=hw.out2[1,1:(100-2)],xlab="Time", ylab="Credit Rate", type='l', col=1, 
     ylim=c(0,0.05), main="Credit Rate")
for(i in 2:5)
  lines(x=seq(0,100-3,1),y=hw.out2[i,1:(100-2)],xlab="Time", ylab="Credit Rate", type='l', col=i)

cat("\n Excess Equity Return: scenarios 1-5
-------------------------------\n")    
cat("see plots\n")
ers.out <- ers(rpi, volterm.equity, termStruct.extension)
plot(x=seq(1,100,1),y=ers.out[1,],xlab="Time", ylab="Excess Equity Return", type='l', col=1, 
     ylim=c(-0.5,1.2), main="Excess Equity Return")
for(i in 2:5)
  lines(x=seq(1,100,1),y=ers.out[i,],xlab="Time", ylab="Excess Equity Return", type='l', col=i)

cat("\n Equity Total Return: scenarios 1-5
-------------------------------\n")    
cat("see plots\n")
ersTotal.out <- ersTotal(hw.out, ers.out)
plot(x=seq(1,100,1),y=ersTotal.out[1,],xlab="Time", ylab="Equity Total Return", type='l', col=1, 
     ylim=c(-0.5,1.2), main="Equity Total Return")
for(i in 2:5)
  lines(x=seq(1,100,1),y=ersTotal.out[i,],xlab="Time", ylab="Equity Total Return", type='l', col=i)

cat("\nInflation Rate: scenarios 1-5
-------------------------------\n")    
cat("see plots\n")
inflationrs.out <- inflationrs(inflation_in, inflation_mean, volterm.inflation)
plot(x=seq(1,100,1),y=inflationrs.out[1,],xlab="Time", ylab="Inflation Rate", type='l', col=1, 
     ylim=c(-0.35,0.5), main="Inflation")
for(i in 2:5)
  lines(x=seq(1,100,1),y=inflationrs.out[i,],xlab="Time", ylab="Inflation Rate", type='l', col=i)

cat("\nCholesky Decomposition of Correlation Matrix
-------------------------------\n")  
cat("see plots\n")
cholesky.out <- cholesky(cholinput)
print(cholesky.out)

cat("\nESG Stochastic Rates: scenarios 1-5
-------------------------------\n")  
cat("see plots\n")
esga.out <- esga(esgin, credit_spread, volterm.equity, inflation_mean,
  volterm.inflation , cholesky.out, termStruct.extension)
indexes <- c("Short Rate", "Credit Spread", "Total Equity Return", "Inflation Rate")
xremove <- c(3,0,0,0)
ylim <- list(sr=c(0,0.25), cs=c(-0.02,0.05), ter=c(-0.5,1.9), ir=c(-0.35,0.6))
for(j in 1:4) {
  plot(x=seq(1,100-xremove[j],1),y=esga.out[[j]][1,1:(100-xremove[j])],xlab="Time", 
       ylab=indexes[j], type='l', col=1, ylim=ylim[[j]], main=paste("ESG: ", indexes[j]))
  for(i in 2:5)
    lines(x=seq(1,100-xremove[j],1),y=esga.out[[j]][i,1:(100-xremove[j])],xlab="Time", 
    ylab=indexes[j], type='l', col=i)
}

cat("\n Stochastic scenarios summary: scenarios 1-5
-------------------------------\n")  
cat("see plots\n")
stochasticScenarios.out <- stochasticScenarios(hw.out,termStruct.extension,
  srinput, esga.out)
plotNames <- names(stochasticScenarios.out)
ylim <- list(term1=c(0,0.18), term2=c(0,0.18), term3=c(0,0.18), term5=c(0,0.17), 
  term7=c(0.01,0.16), term10=c(0.01,0.15), term15=c(0.01,0.15), term20=c(0.01,0.15), term30=c(0.02,0.15),
  inflation=c(-0.35,0.6), credit=c(-0.04,0.05), total=c(0,0.20), equity=c(0,50),
  bond=c(0,2500))
xremove <- c(3,3+(2-1),3+(3-1),3+(5-1),3+(7-1),3+(10-1),3+(15-1),3+(20-1),3+(30-1),0,
  0,3,0,0)
for(j in 1:14) {
  plot(x=seq(1,100-xremove[j],1),y=stochasticScenarios.out[[j]][1,1:(100-xremove[j])],
    xlab="Time", ylab=plotNames[j], type='l', col=1, ylim=ylim[[j]], 
    main=paste("Stochastic summary: ", plotNames[j]))
  for(i in 2:3)
    lines(x=seq(1,100-xremove[j],1),y=stochasticScenarios.out[[j]][i,1:(100-xremove[j])],
    xlab="Time", ylab=plotNames[j], type='l', col=i)
}

cat("\n Deterministic scenario summary
-------------------------------\n")  
cat("see plots\n")
determScenario.out <- determScenario(termStruct.out)
plotNames <- names(determScenario.out)
ylim <- list(term1=c(0,0.18), term2=c(0,0.18), term3=c(0,0.18), term5=c(0,0.17), 
  term7=c(0.01,0.16), term10=c(0.01,0.15), term15=c(0.01,0.15), term20=c(0.01,0.15), term30=c(0.02,0.15),
  inflation=c(-0.35,0.6), credit=c(-0.04,0.05), total=c(0,0.20), equity=c(-0.5,150),
  bond=c(0,150))
xremove <- c(3,3+(2-1),3+(3-1),3+(5-1),3+(7-1),3+(10-1),3+(15-1),3+(20-1),3+(30-1),0,
  0,0,0,0)
for(j in 1:14) {
  plot(x=seq(1,100-xremove[j],1),y=determScenario.out[[j]][1:(100-xremove[j])],
       xlab="Time", ylab=plotNames[j], type='l', col=1, ylim=ylim[[j]], 
       main=paste("Deterministic summary: ", plotNames[j]))
}