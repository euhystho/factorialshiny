two_factors_calc <- function(factor1, factor2){
  # For 2 factors
  levels <- c(2, 2)
  factorNames <- c(first_factor, second_factor)
  factorLabels <- labelFactors(input$factors, factor_labels)
  
  firstFactorLow <- factorLabels[[1]][[1]]
  firstFactorHigh <- factorLabels[[1]][[2]]
  
  secondFactorLow <- factorLabels[[2]][[1]]
  secondFactorHigh <- factorLabels[[2]][[2]]
  
  factorA <- c(
    rep(firstFactorLow, 15),
    rep(firstFactorHigh, 15),
    rep(firstFactorLow, 15),
    rep(firstFactorHigh, 15)
  )
  
  factorB <- c(
    rep(secondFactorLow, 15),
    rep(secondFactorLow, 15),
    rep(secondFactorHigh, 15),
    rep(secondFactorHigh, 15)
  )
  
  # Generate simulated health data
  health <- c(rnorm(15, 2.5, 2.5),
              rnorm(15, 3.0, 2.5),
              rnorm(15, 3.5, 2.5),
              rnorm(15, 4.0, 2.5))
  
  # Create data frame with two factors
  data <- data.frame(factorA, factorB, health)
  
  # Generate the full factorial design
  design <- gen.factorial(levels, nVars = numSelectedFactors, varNames = factorNames)
  
  # Extract variables for analysis
  A <- data$factorA
  B <- data$factorB
  H <- data$health
  
  # Run ANOVA with two factors
  f$result <- aov(H ~ A * B)
}