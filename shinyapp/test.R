factors <- c("Nutrition", "Exercise")
labelFactors <- function(user_input, num_factors) {
  #NOTE: Change the Low/High based on the Factors:
  sleepLabels <- list("Insufficient", "Sufficient")
  exerciseLabels <- list("Moderate", "Intense")
  nutritionLabels <- list("Mindless", "Purposeful")
  
  factorALabels <- NULL
  factorBLabels <- NULL
  factorCLabels <- NULL
  
  sleepIndex <- match("Sleep", user_input)
  exerciseIndex <- match("Exercise", user_input)
  nutritionIndex <- match("Nutrition", user_input)
  
  if (!is.na(sleepIndex) && num_factors >= 2) {
    if (sleepIndex == 1) {
      factorALabels = sleepLabels
    } else if (sleepIndex == 2) {
      factorBLabels = sleepLabels
    } else if (sleepIndex == 3 && num_factors == 3) {
      factorCLabels = sleepLabels
    }
  }
  
  if (!is.na(exerciseIndex && num_factors >= 2)) {
    if (exerciseIndex == 1) {
      factorALabels = exerciseLabels
    } else if (exerciseIndex == 2) {
      factorBLabels = exerciseLabels
    } else if (exerciseIndex == 3 && num_factors == 3) {
      factorCLabels = exerciseLabels
    }
  }
  
  if (!is.na(nutritionIndex && num_factors >= 2)) {
    if (nutritionIndex == 1) {
      factorALabels = nutritionLabels
    } else if (nutritionIndex == 2) {
      factorBLabels = nutritionLabels
    } else if (nutritionIndex == 3 && num_factors == 3) {
      factorCLabels = nutritionLabels
    }
  }
  
  if (num_factors == 3) {
    return(list(factorALabels, factorBLabels, factorCLabels))
  } else if (num_factors == 2) {
    return(list(factorALabels, factorBLabels))
  }
}

# Create a vector of the number of levels for each factor
# Also, put the corresponding levels based on the number of factors
# NOTE: Extend the if else depending on how many additional factors to add

numSelectedFactors = length(factors)
first_factor <- if (numSelectedFactors >= 1)
  factors[1] else NULL
second_factor <- if (numSelectedFactors >= 2)
  factors[2] else NULL


if (numSelectedFactors == 3) {
  levels <- c(2, 2, 2)
  third_factor <- factors[3]
  factorNames <- c(first_factor, second_factor, third_factor)
  factorLabels <- labelFactors(factorNames, numSelectedFactors)
  
  firstFactorLow <- factorLabels[[1]][[1]]
  firstFactorHigh <- factorLabels[[1]][[2]]
  
  secondFactorLow <- factorLabels[[2]][[1]]
  secondFactorHigh <- factorLabels[[2]][[2]]
  
  thirdFactorLow <- factorLabels[[3]][[1]]
  thirdFactorHigh <- factorLabels[[3]][[2]]
  
} else if (numSelectedFactors == 2) {
  levels <- c(2, 2)
  factorNames <- c(first_factor, second_factor)
  factorLabels <- labelFactors(factorNames, numSelectedFactors)
  
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
}




# Generate the full factorial design
design <- gen.factorial(levels, nVars = factorNum, varNames = factorNames)

health <- c(rnorm(15, 2.5, 2.5),
            rnorm(15, 3, 2.5),
            rnorm(15, 3.5, 2.5),
            rnorm(15, 2, 2.5))


data <- data.frame(factorA, factorB, health)

A <- data$factorA
B <- data$factorB
H <- data$health

design <- design
data <- data
result <- aov(H ~ A * B)