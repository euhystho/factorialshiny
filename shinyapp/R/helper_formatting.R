library(stringr)

individuals <- readRDS("Data/ind_data.rds")
weights <- readRDS("Data/ind_weights.rds")

#Just modify this to change factors for everything :)
factor_labels <- list(
  "Sleep" = list("Insufficient", "Sufficient"),
  "Exercise" = list("Light", "Intense"),
  "Nutrition" = list("Mindless", "Purposeful"),
  "Spirituality" = list("Infrequent", "Regular"),
  "Socialization" = list("Isolated", "Outgoing"),
  "Wellbeing" = list("Neglect", "Nurture")
)
factor_names <- names(factor_labels)

#Change these to Change Interaction Plot Specific Factors
  factors_plot_labels <- list(
    "Sleep" = "Sleep Hours",
    "Exercise" = "Exercise",
    "Nutrition" = "Nutrition",
    "Spirituality" = "Spiritual Engagement",
    "Socialization" = "Social Connection",
    "Wellbeing" = "Wellbeing"
  )
  
  lookupFactors <- function(){
    # Dynamic factors_lookup
    factors_lookup <- list()
    
    # Generate all possible two-factor combinations
    for (i in seq_along(factor_names)) {
      for (j in seq(i + 1, length(factor_names))) {
        factor1 <- factor_names[i]
        factor2 <- factor_names[j]
        
        # Create sorted combination key
        combo_key <- paste(sort(c(factor1, factor2)), collapse = "_")
        
        # Define low_low and high_high
        factors_lookup[[combo_key]] <- list(
          low_low = c(factor_labels[[factor1]][[1]], factor_labels[[factor2]][[1]]),
          high_high = c(factor_labels[[factor1]][[2]], factor_labels[[factor2]][[2]])
        )
      }
    }
    return(factors_lookup)
    
  }
  

#Check if the user is mobile :)
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

labelFactors <- function(selected_factors, label_type) {
  num_factors <- length(selected_factors)
  # Extract labels for the first num_factors elements in user_input
  labels <- lapply(selected_factors[1:num_factors], function(factor) {
    if (factor %in% names(label_type)) {
      return(label_type[[factor]])
    } 
  })
  return(labels)
}

labelTableFactors <- function(df, selected_factors) {
  # Get all possible combinations of factors
  all_combinations <- list()
  
  # Generate all possible combinations of the selected factors
  for (i in 1:length(selected_factors)) {
    # Get all combinations of length i
    combinations <- combn(selected_factors, i, simplify = FALSE)
    
    # Add each combination to the list
    all_combinations <- c(all_combinations, combinations)
  }
  
  # Create row names by joining the factors in each combination with ":"
  row_names <- sapply(all_combinations, function(combo) {
    paste(combo, collapse = ":")
  })
  
  # Check if the number of combinations matches the expected rows
  numCombinations <- (2^length(selected_factors)) - 1
  if (length(row_names) != numCombinations) {
    warning("Number of generated row names does not match expected number of combinations")
  }
  
  # Assign row names to the dataframe
  if (nrow(df) >= numCombinations) {
    row.names(df)[1:numCombinations] <- row_names
  } else {
    warning("Dataframe has fewer rows than the number of factor combinations")
    row.names(df)[1:nrow(df)] <- row_names[1:nrow(df)]
  }
  
  return(df)
}

#Assign the Selected Factors to the Effect Plot to make it User-Friendly
formatSigEffect <- function(labels, selected_factors){
# Abbreviate Nutrition as Nutr, Exercise as Ex, and Sleep is Sleep
  labels <- gsub("A1", substr(selected_factors[1], 1, 2), labels)
  labels <- gsub("B1", substr(selected_factors[2], 1, 2), labels)
  labels <- gsub("C1", substr(selected_factors[3], 1, 2), labels)
  labels <- gsub("D1", substr(selected_factors[4], 1, 2), labels)
  labels <- gsub("E1", substr(selected_factors[5], 1, 2), labels)
  labels <- gsub("F1", substr(selected_factors[6], 1, 2), labels)
  return(labels)
}

displayTable <- function(f, selected_factors, tableType){
  
  isAnova <- tableType == "anova"
  
  data <- if (isAnova) as.data.frame(anova(f$result)) else as.data.frame(f$design)
  
  if (isAnova){
    anova_results <- anova(f$result)
    # Round numeric columns to 3 decimal places
    numeric_cols <- sapply(data, is.numeric)
    data[, numeric_cols] <- round(data[, numeric_cols], 3)
    
    # Format p-values with significance stars
    if ("Pr(>F)" %in% colnames(data)) {
      # Create a new column for p-value display
      data$Significance <- ""
      
      # Add significance stars
      data$Significance[data$`Pr(>F)` < 0.001] <- "***"
      data$Significance[data$`Pr(>F)` >= 0.001 &
                          data$`Pr(>F)` < 0.01] <- "**"
      data$Significance[data$`Pr(>F)` >= 0.01 &
                          data$`Pr(>F)` < 0.05] <- "*"
      data$Significance[data$`Pr(>F)` >= 0.05 &
                          data$`Pr(>F)` < 0.1] <- "."
      
      # Format p-values for better readability
      data$`Pr(>F)` <- sprintf("%.3f %s", data$`Pr(>F)`, data$Significance)
      
      # Remove the separate significance column
      data$Significance <- NULL
    }
    data <- labelTableFactors(data, selected_factors)
    
  }
  
  table <- kable(data, row.names = TRUE) %>%
    kable_styling(
      bootstrap_options = c("striped", "responsive"),
      full_width = FALSE, position = if (isAnova) "left" else "center"
    )
  
  if (isAnova){
    table <- table %>% row_spec(
      which(anova_results[, "Pr(>F)"] < 0.05),
      bold = TRUE,
      background = "#e67763",
      color = "#FFF"
    ) %>%
      # Add footer with significance key
      add_footnote(
        c("Significance codes: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1"),
        notation = "none"
      )
    
  } else {
    # Check if each factor exists and apply appropriate styling
    for (i in seq_along(selected_factors)) {
      factor_name <- selected_factors[i]
      col_index <- i + 1  # +1 because column 1 is row names
      
      # Only apply spec_color if the column exists and contains numeric or factor data
      if (factor_name %in% colnames(data)) {
        # Check if column is numeric or can be converted to numeric
        col_data <- data[[factor_name]]
        
        # For factors, convert to numeric 1/2 for Low/High
        if (is.numeric(col_data)) {
          # If it's already numeric, use it directly
          table <- table %>%
            column_spec(
              col_index,
              color = 'white',
              background = spec_color(col_data, end = 0.7, option = 'A')
            )
        }
      }
    }
  }
  return(HTML(table))
}