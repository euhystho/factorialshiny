library(stringr)

insertFactorsToTable <- function(df, selected_factors){
    if (length(selected_factors) == 2) {
      row.names(df)[1:3] = c(selected_factors[1], 
      selected_factors[2], 
      paste0(selected_factors[1], ":", selected_factors[2]))
    } else if (length(selected_factors) == 3) {
      row.names(df)[1:7] = c(
        selected_factors[1],
        selected_factors[2],
        selected_factors[3],
        paste0(selected_factors[1], ":", selected_factors[2]),
        paste0(selected_factors[1], ":", selected_factors[3]),
        paste0(selected_factors[2], ":", selected_factors[3]),
        paste0(
          selected_factors[1],
          ":",
          selected_factors[2],
          ":",
          selected_factors[3]
        )
      )
    }
    return(df)
}

formatSigEffect <- function(name, selected_factors){
# Abbreviate Nutrition as Nutr, Exercise as Ex, and Sleep is Sleep
  return(name)
}