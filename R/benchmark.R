#' @title Read SOA stochastic scenarios 
#' @description Imports the SOA stochastic scenarios into a list structure
#'  containing data frames for term1 and term10 interest rates
#' @param file The file path to the CSV file containing the stochastic scenarios
#' @return A list containing data frames for the term1 and term10 interest rates
#' @examples soaScenarios <- bmarkScenarios("~/Desktop/StocVal/Data/soascenario.csv")
#' @details file argument depends on where the CSV  file is stored
#' @export
bmarkScenarios <- function(file) {
  scenarioCSV <- read.csv(file, header=F)
  term1 <- matrix(0, 500, 100)
  term10 <- matrix(0, 500, 100)
  for(i in 1:500) {
    lower <- (i-1)*100 + 1
    upper <- (i*100)
    term1[i,1:100] <- 
      scenarioCSV[,1][lower:upper]
    term10[i,1:100] <-
      scenarioCSV[,6][lower:upper]
  }
  list(term1=term1, term10=term10)
}

#' @title Calculate the average stochastic scenario
#' @description Calculates the mean of the stochastic scenarios at each time
#' @param scenarios A data frame containing the scenarios, in format of the output
#'  of hull_white() 
#' @return A vector containing the mean of the stochastic scenarios at each
#'  time
#' @examples soaScenarios <- bmarkScenarios("~/Desktop/StocVal/Data/soascenario.csv");
#'  meanScenarios.out <- meanScenarios(soaScenarios$term1)
#' @export 
meanScenarios <- function(scenarios) {
  numScenarios <- length(scenarios[,1])
  scenarioMean <- numeric(100)
  for(i in 1:100) {
    scenarioMean[i] <-mean(as.numeric(scenarios[,i]))
  }
  scenarioMean
}
