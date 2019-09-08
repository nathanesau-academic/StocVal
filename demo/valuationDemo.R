cat("Employee #5 Information
-------------------------------\n")
employee <- demoInfo[5,] # to do this for each employee, loop over each row
print(employee, row.names=F)

cat("\nSurvival Info
-------------------------------\n")
surviveInfo.out <- surviveInfo(employee)
print(surviveInfo.out, row.names=F)

cat("\n... Deterministic Scenario
Account Value (no floor)
-------------------------------\n")
accountValueNF.out <- accountValueNF(employee, surviveInfo.out)
print(accountValueNF.out, row.names=F)

cat("\nAccount Value (with floor)
-------------------------------\n")
accountValueWF.out <- accountValueWF(employee, surviveInfo.out)
print(accountValueWF.out, row.names=F)

cat("\nFloor option cost
-------------------------------\n")
floorOption.cost <- floorOption(accountValueNF.out, accountValueWF.out)
print(floorOption.cost, row.names=F)

cat("\nFloor option cost (PBO)
-------------------------------\n")
floorOptionPBO.cost <- floorOptionPBO(demoInfo[5,], accountValueNF.out, 
  accountValueWF.out, surviveInfo.out)
print(floorOptionPBO.cost, row.names=F)

cat("\nPlan summary
-------------------------------\n")
planSummaryDeterm.out <- planSummaryDeterm(demoInfo,planVariables)
print(planSummaryDeterm.out, row.names=F)

cat("\n... Stochastic Scenarios
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

cat("\nScenario #1
-------------------------------\n")
term1 <- as.numeric(stochasticScenarios.out$term1[1,])
term10 <- as.numeric(stochasticScenarios.out$term10[1,])

cat("\nTerm 1 yields\n")
print(term1)

cat("\nTerm 10 yields\n")
print(term10)
accountValueNF.out <- accountValueNF(employee, surviveInfo.out, 
  oneyear=term1, tenyear=term10)
accountValueWF.out <- accountValueWF(employee, surviveInfo.out,
  oneyear=term1, tenyear=term10)
floorOption.cost <- floorOption(accountValueNF.out, accountValueWF.out,
  oneyear=term1, tenyear=term10)
floorOptionPBO.cost <- floorOptionPBO(employee, accountValueNF.out,
  accountValueWF.out, surviveInfo.out, oneyear=term1, tenyear=term10)

cat("\nFloor option cost\n")
print(floorOption.cost)

cat("\nFloor option cost (PBO)\n")
print(floorOptionPBO.cost)

cat("\n1000 Scenarios
-------------------------------\n")
stocfloorOption.cost <- stocfloorOption(employee, surviveInfo.out, 
  stochasticScenarios.out)
cat("\nMean of costs\n")
print(stocfloorOption.cost$mean_cost)
cat("\nVariance of costs\n")
print(stocfloorOption.cost$var_cost)

cat("\nsee plots\n")
hist(stocfloorOption.cost$costs, main="Distribution of floor option costs",
     ylab="Frequency", xlab="Floor option cost")

stocfloorOptionPBO.cost <- stocfloorOptionPBO(employee, surviveInfo.out,
  stochasticScenarios.out)
cat("\nMean of costs (PBO)\n")
print(stocfloorOptionPBO.cost$mean_cost)
cat("\nVariance of costs (PBO)\n")
print(stocfloorOptionPBO.cost$var_cost)

cat("\nsee plots\n")
hist(stocfloorOptionPBO.cost$costs, main="Distribution of floor option 
     costs (PBO)", ylab="Frequency", xlab="Floor option cost")

cat("\nEntire plan
-------------------------------\n")
planSummaryStoc.out <- planSummaryStoc(demoInfo, stochasticScenarios.out,
  planVariables)
print(planSummaryStoc.out$planSummary, row.names=F)

cat("\nsee plots\n")
hist(planSummaryStoc.out$floor_option_costs, main="Distribution of total
  floor option costs", ylab="Frequency", xlab="Floor option cost")
hist(planSummaryStoc.out$floor_option_costs_pbo, main="Distribution of total
  floor option costs (PBO)", ylab="Frequency", xlab="Floor option cost")