library(stringr)

individuals <- readRDS("Data/ind_data.rds")

#Just modify this to change factors for everything :)
factor_labels <- list(
  "Sleep" = list("Insufficient", "Sufficient"),
  "Exercise" = list("Light", "Intense"),
  "Nutrition" = list("Mindless", "Purposeful")
)

factor_names <- names(factor_labels)

#Change these to Change Interaction Plot Specific Factors
  factors_plot_labels <- list(
    "Sleep" = "Sleep Hours",
    "Exercise" = "Exercise Intensity",
    "Nutrition" = "Nutrition Intention"
  )
  
  
  # Create a standardized lookup system for factor combinations
  factors_lookup <- list(
    # Nutrition and Sleep combination
    "Nutrition_Sleep" = list(
      low_low = c(factor_labels[[factor_names[3]]][[1]], 
                  factor_labels[[factor_names[1]]][[1]]),
      high_high = c(factor_labels[[factor_names[3]]][[2]], 
                    factor_labels[[factor_names[1]]][[2]])
    ),
    # Nutrition and Exercise combination
    "Nutrition_Exercise" = list(
      low_low = c(factor_labels[[factor_names[3]]][[1]], 
                  factor_labels[[factor_names[2]]][[1]]),
      high_high = c(factor_labels[[factor_names[3]]][[2]], 
                    factor_labels[[factor_names[2]]][[2]])
    ),
    # Sleep and Exercise combination
    "Sleep_Exercise" = list(
      low_low = c(factor_labels[[factor_names[1]]][[1]], 
                  factor_labels[[factor_names[2]]][[1]]),
      high_high = c(factor_labels[[factor_names[1]]][[2]], 
                    factor_labels[[factor_names[2]]][[2]])
    )
  )

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

#Assign the Selected Factors to the Table to make it User-Friendly
labelTableFactors <- function(df, selected_factors){
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


#Assign the Selected Factors to the Effect Plot to make it User-Friendly
formatSigEffect <- function(labels, selected_factors){
# Abbreviate Nutrition as Nutr, Exercise as Ex, and Sleep is Sleep
  labels <- gsub("A1", selected_factors[1], labels)
  labels <- gsub("B1", selected_factors[2], labels)
  labels <- gsub("C1", selected_factors[3], labels)
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