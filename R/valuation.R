#' @title Termination Assumption for an employee
#' @param employee A row from the demoInfo dataset
#' @param r The maximum retirement age
#' @return A data frame containing the termination assumption
#'  for the employee from age x to age r inclusive
#' @examples employee <- demoInfo[5,];
#'  termAssumption.out <- termAssumption(employee)
#' @export
termAssumption <- function(employee,r=65) {
  x = employee$age_valuation
  ageSeq = seq(x, r)
  termRate = numeric()
  
  for(age in ageSeq) {
    termRate = c(termRate, 
      ifelse(age==r,1,
        ifelse(age>=(r-5), 0.1, 0.03)))
  }
  
  # TR: termination Rate
  data.frame(Age=ageSeq, TR=termRate)
}

#' @title Survivorship Information for employee
#' @description Creates a data frame containing the decrements for the 
#'  employee.
#' @param employee A row from the demoInfo dataset
#' @param assumptions Plan features and assumptions - by default 
#'  the variables in planVariables are used. 
#' @param mortTable The mortality table to use containing qxm and qxf 
#'  columns for age 0 to 110. By default the columns in the mortalityInfo
#'  dataset are used.
#' @return The mortality rate, termination rate, probability of decrement
#'  during year and survival factor at the beginning of the year for 
#'  the employee.
#' @examples surviveInfo.out <- surviveInfo(demoInfo[5,])
#' @details The mortality table used can be found in the mortalityInfo
#'  dataset.
#' @export 
surviveInfo <- function(employee, assumptions=planVariables, 
  mortTable=mortalityInfo) {
  r <- employee$age_valuation + employee$remaining
  termRate <- termAssumption(employee,r)$TR
  mortRows <- seq(employee$age_valuation, r)
  nrows <- length(mortRows)
  
  mortRate <- numeric(nrows)
  if(employee$gender=="M") 
    mortRate <- mortTable$qxm[mortRows+1]
  else # female
    mortRate <- mortTable$qxf[mortRows+1]

  probDecr <- pmin(mortRate+termRate - termRate*mortRate, 1)
  survBoy <- numeric(nrows)
  survBoy[1] <- 1
  
  # calculate survival probability at boy
  for(i in 2:length(mortRows)) {
    survBoy[i] <- ifelse(survBoy[i-1]<=0, 0,
      survBoy[i-1]*(1-probDecr[i-1]))
  }
  
  data.frame(Age=mortRows, mortRate, termRate, probDecr, survBoy)  
}

#' @title Account Value without floor on return
#' @param employee A row from the demoInfo dataset
#' @param surviveInfo.out output of surviveInfo()
#' @param r The maximum retirement age
#' @param assumptions Plan features and assumptions - by default 
#'  the variables in planVariables are used. 
#' @param oneyear 1 year treasury bond yields. By default, deterministic scenario
#'  output is used.
#' @param tenyear 10 year treasury bond yields. By default, deterministic scenario
#'  output is used.
#' @return A data frame containing the account value boy, account value
#'  after growth, salary, contribution (eoy), account value (eoy), 
#'  benefit payments (boy) and discount benefit payment for each year 
#'  until retirement
#' @examples surviveInfo.out <- surviveInfo(demoInfo[5,]);
#'  accountValueNF.out <- accountValueNF(demoInfo[5,], surviveInfo.out)
#' @details NF stands for no floor
#' @export 
accountValueNF <- function(employee, surviveInfo.out, assumptions=planVariables,
  oneyear=term1, tenyear=term10) {
  nrows <- length(surviveInfo.out[,1])
  yields1 <- oneyear[1:nrows]
  yields10 <- tenyear[1:nrows]

  # derive discount factor BOY and bond index
  discFactor <- numeric(nrows)
  discFactor[1] <- 1
  bondIndex <- discFactor # copy to bond index before loop
  for(i in 2:nrows) {
    discFactor[i] <- discFactor[i-1]/(1+yields1[i-1]) # uses 1 year 
    bondIndex[i] <- bondIndex[i-1]*(1+yields10[i-1]) # uses 10 year 
  }
  
  # inputs
  x <- employee$age_valuation
  s <- planVariables$s
  e <- employee$age_entry 
  r <- employee$age_valuation + employee$remaining
  CR <- assumptions$CR
  vest <- assumptions$vesting 
  
  av_boy <- c(employee$account_value) # length nrows 
  av_growth <- av_boy * (1+yields10[1])
  salary <- employee$current_salary
  contrib_eoy <- ifelse(x<=r, salary*CR, 0)
  av_eoy <- av_growth + contrib_eoy
  ben_boy <- ifelse((x-e) < vest, 0, av_boy*surviveInfo.out$probDecr[1]*surviveInfo.out$survBoy[1])
  dis_ben <- ben_boy*discFactor[1]
  
  if(nrows<2) { # exit before loop if needed
    return(data.frame(Age=x,av_boy,av_growth,salary,contrib_eoy,av_eoy,ben_boy,dis_ben))
  }
  
  for(i in 2:nrows) {
    tmp <- (x+i-1) # age
    tmp2 <- (x+i-1)-e # time since entry
    
    av_boy[i] <- max(0, av_eoy[i-1])
    av_growth[i] <- av_boy[i]*(1+yields10[i])
    salary[i] <- ifelse(tmp<r, salary[i-1]*(1+s), 0)
    contrib_eoy[i] <- ifelse(tmp<=r, salary[i]*CR, 0) # constant CR
    av_eoy[i] <- av_growth[i] + contrib_eoy[i]
    ben_boy[i] <- ifelse(tmp2 < vest, 0, av_boy[i]*surviveInfo.out$survBoy[i]*surviveInfo.out$probDecr[i])
    dis_ben[i] <- ben_boy[i]*discFactor[i]
  }
  
  data.frame(Age=x:r,av_boy,av_growth,salary,contrib_eoy,av_eoy,ben_boy,dis_ben)
}

#' @title Account Value without floor on return
#' @param employee A row from the demoInfo dataset
#' @param surviveInfo.out output of surviveInfo()
#' @param r The maximum retirement age
#' @param assumptions Plan features and assumptions - by default 
#'  the variables in planVariables are used. 
#' @param oneyear 1 year treasury bond yields. By default, deterministic scenario
#'  output is used.
#' @param tenyear 10 year treasury bond yields. By default, deterministic scenario
#'  output is used.
#' @return A data frame containing the account value boy, account value
#'  after growth, salary, contribution (eoy), account value (eoy), 
#'  benefit payments (boy) and discount benefit payment for each year 
#'  until retirement
#' @examples surviveInfo.out <- surviveInfo(demoInfo[5,]);
#'  accountValueWF.out <- accountValueWF(demoInfo[5,], surviveInfo.out)
#' @details WF stands for with floor. For the most part, this function
#'  is very similar to accountValueNF()
#' @export 
accountValueWF <- function(employee, surviveInfo.out, assumptions=planVariables,
  oneyear=term1, tenyear=term10) {
  nrows <- length(surviveInfo.out[,1])
  yields1 <- oneyear[1:nrows]
  yields10 <- tenyear[1:nrows]
  
  # derive discount factor BOY and bond index
  discFactor <- numeric(nrows)
  discFactor[1] <- 1
  bondIndex <- discFactor # copy to bond index before loop
  for(i in 2:nrows) {
    discFactor[i] <- discFactor[i-1]/(1+yields1[i-1]) # uses 1 year 
    bondIndex[i] <- bondIndex[i-1]*(1+yields10[i-1]) # uses 10 year 
  }
  
  # inputs
  x <- employee$age_valuation
  s <- planVariables$s
  e <- employee$age_entry 
  r <- employee$age_valuation + employee$remaining
  CR <- assumptions$CR
  vest <- assumptions$vesting 
  floor <- assumptions$floor
  
  av_boy <- c(employee$account_value) # length nrows 
  av_growth <- av_boy * (1+yields10[1])
  option_adjust <- ifelse(x<=r, av_boy*max(floor-yields10[1], 0), 0)
  av_growth_adjust <- ifelse(x<=r, option_adjust, 0) + av_growth
  salary <- employee$current_salary
  contrib_eoy <- ifelse(x<=r, salary*CR, 0)
  av_eoy <- av_growth_adjust + contrib_eoy
  ben_boy <- ifelse((x-e) < vest, 0, av_boy*surviveInfo.out$probDecr[1]*surviveInfo.out$survBoy[1])
  dis_ben <- ben_boy*discFactor[1]
  
  if(nrows<2) { # exit before loop if needed
    return(data.frame(Age=x,av_boy,av_growth,option_adjust,av_growth_adjust,
        salary,contrib_eoy,av_eoy,ben_boy,dis_ben))
  }
  
  for(i in 2:nrows) {
    tmp <- (x+i-1) # age
    tmp2 <- (x+i-1)-e # time since entry
    
    av_boy[i] <- max(0, av_eoy[i-1])
    av_growth[i] <- av_boy[i]*(1+yields10[i])
    option_adjust[i] <- ifelse(tmp <= r, av_boy[i]*max(floor-yields10[i],0))
    av_growth_adjust[i] <- ifelse(tmp <= r, option_adjust[i], 0) + av_growth[i]
    salary[i] <- ifelse(tmp<r, salary[i-1]*(1+s), 0)
    contrib_eoy[i] <- ifelse(tmp<=r, salary[i]*CR, 0) # constant CR
    av_eoy[i] <- av_growth_adjust[i] + contrib_eoy[i]
    ben_boy[i] <- ifelse(tmp2 < vest, 0, av_boy[i]*surviveInfo.out$survBoy[i]*surviveInfo.out$probDecr[i])
    dis_ben[i] <- ben_boy[i]*discFactor[i]
  }
  
  data.frame(Age=x:r,av_boy,av_growth,option_adjust,av_growth_adjust,
    salary,contrib_eoy,av_eoy,ben_boy,dis_ben)
}

#' @title Cost of floor option 
#' @description Returns the cost of the embedded floor option, calculated
#'  by taking the difference in the payment with the option vs 
#'  without the option. 
#' @param accountValueNF.out output of accountValueNF()
#' @param accountValueWF.out output of accountValueWF()
#' @param oneyear 1 year treasury bond yields. By default, deterministic scenario
#'  output is used.
#' @param tenyear 10 year treasury bond yields. By default, deterministic scenario
#'  output is used.
#' @return The cost of the embedded option
#' @examples surviveInfo.out <- surviveInfo(demoInfo[5,]);
#'  accountValueNF.out <- accountValueNF(demoInfo[5,], surviveInfo.out);
#'  accountValueWF.out <- accountValueWF(demoInfo[5,], surviveInfo.out);
#'  floorOption.cost <- floorOption(accountValueNF.out, accountValueWF.out)
#' @details The option is a floor on the minimum return, so it increases
#'  the value of the benefit for the employee.
#' @export
floorOption <- function(accountValueNF.out, accountValueWF.out,
  oneyear=term1, tenyear=term10) {
  payoff <- accountValueWF.out$ben_boy - accountValueNF.out$ben_boy
  
  nrows <- length(accountValueNF.out[,1])
  yields1 <- oneyear[1:nrows]
  yields10 <- tenyear[1:nrows]
  
  # derive discount factor BOY and bond index
  discFactor <- numeric(nrows)
  discFactor[1] <- 1
  bondIndex <- discFactor # copy to bond index before loop
  for(i in 2:nrows) {
    discFactor[i] <- discFactor[i-1]/(1+yields1[i-1]) # uses 1 year 
    bondIndex[i] <- bondIndex[i-1]*(1+yields10[i-1]) # uses 10 year 
  }
  sum(payoff*discFactor)
}

#' @title Cost of floor option (pbo)
#' @param employee A row from the demoInfo dataset
#' @param accountValueNF.out output of accountValueNF()
#' @param accountValueWF.out output of accountValueWF()
#' @param surviveInfo.out output of surviveInfo()
#' @param assumptions Plan features and assumptions - by default 
#'  the variables in planVariables are used. 
#' @param oneyear 1 year treasury bond yields. By default, deterministic scenario
#'  output is used.
#' @param tenyear 10 year treasury bond yields. By default, deterministic scenario
#'  output is used.
#' @return The cost of the embedded option
#' @examples surviveInfo.out <- surviveInfo(demoInfo[5,]);
#'  accountValueNF.out <- accountValueNF(demoInfo[5,], surviveInfo.out);
#'  accountValueWF.out <- accountValueWF(demoInfo[5,], surviveInfo.out);
#'  floorOptionPBO.cost <- floorOptionPBO(demoInfo[5,], accountValueNF.out, 
#'  accountValueWF.out, surviveInfo.out)
#' @details The option cost will be different when a stochastic valuation is
#'  used. This is a different way of calculating the cost of the floor option.
#' @export 
floorOptionPBO <- function(employee, accountValueNF.out, accountValueWF.out,
  surviveInfo.out, assumptions=planVariables, oneyear=term1, tenyear=term10) {
  # input variables
  x <- employee$age_valuation
  r <- employee$age_valuation + employee$remaining
  nrows <- (r-x)+1
  yields1 <- oneyear[1:nrows]
  yields10 <- tenyear[1:nrows]
  vol <- assumptions$vol
  floor <- assumptions$floor
  
  # derive discount factor BOY and bond index
  discFactor <- numeric(nrows)
  discFactor[1] <- 1
  bondIndex <- discFactor # copy to bond index before loop
  for(i in 2:nrows) {
    discFactor[i] <- discFactor[i-1]/(1+yields1[i-1]) # uses 1 year 
    bondIndex[i] <- bondIndex[i-1]*(1+yields10[i-1]) # uses 10 year 
  }
  
  # closed form valuation
  closedForm <- data.frame(age=x:r,abo=numeric(nrows),
    pbo=numeric(nrows))
  
  for(i in 2:nrows) {
    tmp <- (x+i-1) # age 
    tmp2 <- tmp - x # diff in age
    probDecr <- ifelse(x<r, surviveInfo.out$probDecr[i], 1)
    survBoy <- surviveInfo.out$survBoy[i-1]
    av_boy <- accountValueWF.out$av_boy[1] # note [1] not [i] 
    
    # risk neutral
    C <- eur_call_bsm(S=1,
      K=1+floor-((bondIndex[i]*discFactor[i])^(1/tmp2)-1),
      r=-log(discFactor[i])/(tmp2),
      sigma=vol,
      d=0,
      term=1)
    tmp_abo <- ( ((1+floor)*discFactor[i]^(1/tmp2) + C)^(tmp2)*av_boy - 
      av_boy*bondIndex[i]*discFactor[i])*probDecr*survBoy
    closedForm$abo[i] <- ifelse(x<=r,tmp_abo,0) # abo
    
    if(i==2) {
      closedForm$pbo[i] <- CBP3CFV(x, r, surviveInfo.out$survBoy, discFactor,
        bondIndex, accountValueWF.out$av_boy, 
        vol, floor) * discFactor[1] # [1] not [i]
    } else {
      survFactor_tmp <- tail(surviveInfo.out$survBoy, -i+2)
      discFactor_tmp <- tail(discFactor, -i+2)
      bondIndex_tmp <- tail(bondIndex, -i+2)
      avBoy_tmp <- accountValueWF.out$contrib_eoy[i-2]
    
      closedForm$pbo[i] <- CBP3CFV(x+i-2, r, survFactor_tmp, discFactor_tmp,
        bondIndex_tmp, avBoy_tmp, vol, floor) * discFactor[i-1]
    }
  }  
  sum(closedForm$pbo)
}

#' @title Embedded option prices for each employee (Deterministic)
#' @description Calculates the embedded option prices for each employee using the deterministic
#'  scenario output from floorOption() and floorOptionPBO()
#' @param demoInfo Demographic info for plan
#' @param assumptions Plan features and assumptions - by default 
#'  the variables in planVariables are used. 
#' @return Option costs for each employee
#' @examples planSummaryDeterm.out <- planSummaryDeterm(demoInfo,planVariables)
#' @export
planSummaryDeterm <- function(demoInfo, assumptions=planVariables) {
    numEmployees <- length(demoInfo[,1])
    planSummary.out <- matrix(0,numEmployees,1+1+2)
    for(i in 1:numEmployees) {
        employee <- demoInfo[i,]
        surviveInfo.out <- surviveInfo(employee,assumptions)
        accountValueNF.out <- accountValueNF(employee,surviveInfo.out,
          assumptions)
        accountValueWF.out <- accountValueWF(employee,surviveInfo.out,
          assumptions)
        floorOption.cost <- floorOption(accountValueNF.out,accountValueWF.out)
        floorOptionPBO.cost <- floorOptionPBO(employee, accountValueNF.out,
          accountValueWF.out, surviveInfo.out, assumptions)
        
        planSummary.out[i,1] <- employee$employee_id
        planSummary.out[i,2] <- sum(accountValueNF.out$dis_ben)
        planSummary.out[i,3] <- floorOption.cost 
        planSummary.out[i,4] <- floorOptionPBO.cost 
    }
    
    planSummary.out <- data.frame(planSummary.out)
    names(planSummary.out) <- c("employee_id", "pv_benefit_no", "floor_option",
      "floor_option_pbo")
    planSummary.out["total", ] <- colSums(planSummary.out) # column totals
    planSummary.out["total",1] <- ""
    planSummary.out 
}

#' @title Stochastic cost of floor option
#' @description Returns a list containing the costs of the floor option under 
#'  each stochastic scenario, the expected value and variance.
#' @param employee A row from the demoInfo dataset
#' @param surviveInfo.out output of surviveInfo()
#' @param stochasticScenarios.out output of stochasticScenarios()
#' @return A list containing the cost of option under each scenario, mean and 
#'  variance.
#' @examples surviveInfo.out <- surviveInfo(demoInfo[5,]);
#'  termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  credit_spread <- rep(0.01,100);
#'  hw.out <- hull_white(srinput, termStruct.extension, termofyield=1);
#'  cholesky.out <- cholesky(cholinput);
#'  esga.out <- esga(esgin, credit_spread, volterm.equity, inflation_mean,
#'  volterm.inflation , cholesky.out, termStruct.extension);
#'  stochasticScenarios.out <- stochasticScenarios(hw.out, termStruct.extension,
#'  srinput, esga.out)
#'  stocfloorOption.cost <- stocfloorOption(demoInfo[5,], surviveInfo.out, 
#'  stochasticScenarios.out)
#' @export 
stocfloorOption <- function(employee, surviveInfo.out, stochasticScenarios.out,
  assumptions=planVariables) {
  numScenarios <- length(stochasticScenarios.out$term1[,1])
  floorOption.cost <- numeric(numScenarios)
  for(i in 1:numScenarios) {
    term1 <- as.numeric(stochasticScenarios.out$term1[i,])
    term10 <- as.numeric(stochasticScenarios.out$term10[i,])
    accountValueNF.out <- accountValueNF(employee, surviveInfo.out,
      assumptions, oneyear=term1, tenyear=term10)
    accountValueWF.out <- accountValueWF(employee, surviveInfo.out,
      assumptions, oneyear=term1, tenyear=term10)
    floorOption.cost[i] <- floorOption(accountValueNF.out, accountValueWF.out,
      oneyear=term1, tenyear=term10)
  }
  list(costs=floorOption.cost, mean_cost=mean(floorOption.cost),
    var_cost=var(floorOption.cost))
}

#' @title Stochastic cost of floor option (pbo)
#' @description Returns a list containing the costs of the floor option
#'  under each stochastic scenario, the expected value and variance
#' @param employee A row from the demoInfo dataset
#' @param surviveInfo.out output of surviveInfo()
#' @param stochasticScenarios.out output of stochasticScenarios()
#' @return A list containing the cost of the option under each scenario, mean
#'  and variance.
#' @examples surviveInfo.out <- surviveInfo(demoInfo[5,]);
#'  termStruct.out <- cubic_spline(termStruct);
#'  termStruct.extension <- curve_extensionNS(termStruct.out);
#'  credit_spread <- rep(0.01,100);
#'  hw.out <- hull_white(srinput, termStruct.extension, termofyield=1);
#'  cholesky.out <- cholesky(cholinput);
#'  esga.out <- esga(esgin, credit_spread, volterm.equity, inflation_mean,
#'  volterm.inflation , cholesky.out, termStruct.extension);
#'  stochasticScenarios.out <- stochasticScenarios(hw.out, termStruct.extension,
#'  srinput, esga.out)
#'  stocfloorOptionPBO.cost <- stocfloorOptionPBO(demoInfo[5,], surviveInfo.out, 
#'  stochasticScenarios.out)
#' @export 
stocfloorOptionPBO <- function(employee, surviveInfo.out, stochasticScenarios.out,
  assumptions=planVariables) {
  numScenarios <- length(stochasticScenarios.out$term1[,1])
  floorOptionPBO.cost <- numeric(numScenarios)
  for(i in 1:numScenarios) {
    term1 <- as.numeric(stochasticScenarios.out$term1[i,])
    term10 <- as.numeric(stochasticScenarios.out$term10[i,])
    accountValueNF.out <- accountValueNF(employee, surviveInfo.out,
      assumptions, oneyear=term1, tenyear=term10)
    accountValueWF.out <- accountValueWF(employee, surviveInfo.out,
      assumptions, oneyear=term1, tenyear=term10)
    floorOptionPBO.cost[i] <- floorOptionPBO(employee, accountValueNF.out, 
      accountValueWF.out, surviveInfo.out, oneyear=term1, tenyear=term10)
  }
  list(costs=floorOptionPBO.cost, mean_cost=mean(floorOptionPBO.cost),
    var_cost=var(floorOptionPBO.cost))
}

#' @title Embedded option prices for each employee (Stochastic)
#' @description Calculates the embedded option prices for each employee using
#'  the stochastic scenario output from stocfloorOption() and stocfloorOptionPBO()
#' @param demoInfo Demographic info for plan
#' @param stochasticScenarios.out output of stochasticScenarios()
#' @param assumptions Plan features and assumptions - by default 
#'  the variables in planVariables are used. 
#' @return A list containing the total floor option cost for each scenario,
#'  the total floor option (PBO) cost for each scenario and a data frame
#'  containing the expected value and mean costs for each scenario
#' @examples planSummaryStoc.out <- planSummaryStoc(demoInfo, stochasticScenarios.out,
#'  planVariables)
#' @export 
planSummaryStoc <- function(demoInfo, stochasticScenarios.out,
                            assumptions=planVariables) {
  numEmployees <- length(demoInfo[,1])
  numScenarios <- length(stochasticScenarios.out$term1[,1])
  planSummary.out <- matrix(0,numEmployees,1+1+2+2)
  totfloorOption.cost <- numeric(numScenarios)
  totfloorOptionPBO.cost <- numeric(numScenarios)
  
  for(i in 1:numEmployees) {
    employee <- demoInfo[i,]
    surviveInfo.out <- surviveInfo(employee,assumptions)
    
    floorOption.cost <- numeric(numScenarios)
    floorOptionPBO.cost <- numeric(numScenarios)
    
    for(j in 1:numScenarios) {
      term1 <- as.numeric(stochasticScenarios.out$term1[j,])
      term10 <- as.numeric(stochasticScenarios.out$term10[j,])
      accountValueNF.out <- accountValueNF(employee,surviveInfo.out,
                                           assumptions, oneyear=term1, tenyear=term10)
      accountValueWF.out <- accountValueWF(employee,surviveInfo.out,
                                           assumptions, oneyear=term1, tenyear=term10)
      floorOption.cost[j] <- floorOption(accountValueNF.out, accountValueWF.out,
                                             oneyear=term1, tenyear=term10)
      floorOptionPBO.cost[j] <- floorOptionPBO(employee, accountValueNF.out,
                                                 accountValueWF.out, surviveInfo.out, assumptions, oneyear=term1, 
                                                 tenyear=term10)
    }
    totfloorOption.cost <- totfloorOption.cost + floorOption.cost
    totfloorOptionPBO.cost <- totfloorOptionPBO.cost + floorOptionPBO.cost
    
    planSummary.out[i,1] <- employee$employee_id
    planSummary.out[i,2] <- sum(accountValueNF.out$dis_ben)
    planSummary.out[i,3] <- mean(floorOption.cost)
    planSummary.out[i,4] <- mean(floorOptionPBO.cost)
    planSummary.out[i,5] <- var(floorOption.cost)
    planSummary.out[i,6] <- var(floorOptionPBO.cost)
  }
  
  planSummary.out <- data.frame(planSummary.out)
  names(planSummary.out) <- c("employee_id", "pv_benefit_no", "mean_floor_option",
                              "mean_floor_option_pbo", "var_floor_option", "var_floor_option_pbo")
  planSummary.out["total", ] <- colSums(planSummary.out) # column totals
  planSummary.out["total",1] <- ""
  list(floor_option_costs=totfloorOption.cost, 
       floor_option_costs_pbo=totfloorOptionPBO.cost,
       planSummary=planSummary.out)
}