#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

# Shiny-related UI libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(paletteer)

#Plot related libraries
library(plotly)
library(kableExtra)
library(tidyverse)
library(dae)

#Factorial Design related libraries
library(AlgDesign)
library(DoE.base)
library(daewr)
options(shiny.reactlog=TRUE) 


# Define UI for application
ui <- page_sidebar(
  useShinyjs(),
  #TODO: Application Bootswatch Theme
  theme = bs_theme(bootswatch = "journal"),
  # Application title
  title = "Effects of Self-Efficacy on Post-Menopausal Women's Health",
  window_title = "2^k Factorial Design of Healthy Habits Circle Simulations",
  
  # Mobile Friendly:
  fillable_mobile = TRUE,
  mobileDetect('isMobile'),
  
  # Sidebar with a slider input for number of bins
  sidebar = sidebar(
    title = "Health Dimensions",
    width = 300,
    fillable = TRUE,
    accordion_filters <- accordion(
      id = "dims",
      checkboxGroupButtons(
        "phys1",
        "Select up to three of the following factors:",
        choices = c("Nutrition", "Sleep", "Exercise","Spirituality", "Cognition", "Community"),
        selected = c("Nutrition","Sleep", "Exercise"),
        size = 'sm'
      )
     ),
      accordion_panel(
        "Physical Health",
        icon = bs_icon("heart-pulse-fill"),
        conditionalPanel(
          condition = "input.phys.includes('Nutrition')",
          card(
            card_header("Nutrition"),
            sliderTextInput(
              "calorie_slider",
              "Caloric Intake:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Below Target", "At Target", "Above Target")
            ),
            sliderTextInput(
              "protein_slider",
              "Protein Intake:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Below Target", "At Target", "Above Target")
            ),
            sliderTextInput(
              "fat_slider",
              "Fat Intake:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Below Target", "At Target", "Above Target")
            ),
            sliderTextInput(
              "carb_slider",
              "Carbohydrate Intake:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Below Target", "At Target", "Above Target")
            ),
            sliderTextInput(
              "fasting_slider",
              "Choose the type of fasting:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Dry", "Water", "Intermittent")
            ),
            sliderTextInput(
              "other_nutr_slider",
              "Choose how much of any other nutritional factor:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Below Target", "At Target", "Above Target")
            ),
          )
        ),
        conditionalPanel(
          condition = "input.phys.includes('Sleep')",
          card(
            card_header("Sleep"),
            sliderInput(
              "sleep_quan_slider",
              "Choose how many hours of Sleep:",
              min = 0,
              max = 10,
              value = c(7, 9)
            ),
            sliderTextInput(
              "sleep_qual_slider",
              "Choose the quality of Sleep:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Terrible", "Excellent")
            ),
            sliderInput(
              "sleep_eat_slider",
              "Choose how many hours since eating before bed:",
              min = 0,
              max = 8,
              value = 3,
            ),
            sliderInput(
              "screentime_slider",
              "Choose how much screentime before bed:",
              min = 0,
              max = 5,
              value = 3,
            ),
            sliderInput(
              "melatonin_slider",
              "Choose how many mgs of Melatonin are taken before bed:",
              min = 0,
              max = 10,
              value = 0,
            ),
          ),
          # Added missing comma here
        ),
        conditionalPanel(
          condition = "input.phys.includes('Exercise')",
          card(
            card_header("Exercise"),
            sliderTextInput(
              "swim_slider",
              "Choose the intensity of Swimming:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("None","Light", "Intense")
            ),
            sliderTextInput(
              "run_slider",
              "Choose the intensity of Running:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("None","Light", "Intense")
            ),
            sliderTextInput(
              "walk_slider",
              "Choose the intensity of Walking:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("None","Light", "Intense")
            ),
            sliderTextInput(
              "hike_slider",
              "Choose the intensity of Hiking:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("None","Light", "Intense")
            ),
            sliderTextInput(
              "bike_slider",
              "Choose the intensity of Biking:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("None","Light", "Intense")
            ),
            sliderTextInput(
              "team_slider",
              "Choose the intensity of Team Sport:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("None","Light", "Intense")
            ),
            sliderTextInput(
              "lifting_slider",
              "Choose the intensity of Weightlifting:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("None","Light", "Intense")
            ),
            sliderTextInput(
              "other_exerc_slider",
              "Choose the intensity of Other Exercise:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("None","Light", "Intense")
            )
          )
        )
      ),
      accordion_panel(
        "Social Health",
        icon = bs_icon("people-fill"),
        card (
          sliderInput(
            "friends_slider",
            "Specify the amount of hours socializing with friends per week:",
            min = 0,
            max = 25,
            value = 10
          ),
          sliderInput(
            "strangers_slider",
            "Specify amount of hours socializing with strangers per week:",
            min = 0,
            max = 20,
            value = 10
          ),
          sliderInput(
            "vol_slider",
            "Specify the amount of time a week spent doing community service:",
            min = 0,
            max = 30,
            value = 10
          )
        )
      ),
      accordion_panel(
        "Mental Health",
        icon = bs_icon("person-circle"),
        card(
          sliderInput(
            "read_slider",
            "Choose how many hours spent reading:",
            min = 0,
            max = 15,
            value = 5
          ),
          sliderInput(
            "journal_slider",
            "Choose how many hours spent journaling:",
            min = 0,
            max = 5,
            value = 3
          ),
          sliderInput(
            "mental_slider",
            "Choose how many hours promoting mental health through an activity (e.g., therapy, support group):",
            min = 0,
            max = 20,
            value = 2
          ),
          sliderInput(
            "wellbeing_slider",
            "Choose how many hours engaging in an activity that promotes wellbeing:",
            min = 0,
            max = 20,
            value = 2
          )
        )
        
      ),
      accordion_panel(
        "Spiritual Health",
        icon = bs_icon("yin-yang"),
        card(
          sliderInput(
            "meditate_slider",
            "Choose how many hours a week spent meditating:",
            min = 0,
            max = 5,
            value = 2
          ),
          sliderInput(
            "mindful_slider",
            "Choose how many hours a week are spent practicing mindfulness:",
            min = 0,
            max = 5,
            value = 1
          ),
          sliderInput(
            "religious_slider",
            "Choose how many hours a week spent at a religious institution:",
            min = 0,
            max = 10,
            value = 3
          ),
          sliderInput(
            "pray_slider",
            "Choose how many hours a week spent praying:",
            min = 0,
            max = 5,
            value = 1
          ),
          sliderInput(
            "yoga_slider",
            "Choose how many hours a week spent doing yoga:",
            min = 0,
            max = 5,
            value = 3
          ),
          sliderInput(
            "sacred_slider",
            "Choose how many hours a week are spent studying sacred texts:",
            min = 0,
            max = 10,
            value = 2
          ),
          sliderInput(
            "spirit_slider",
            "Choose how many hours a week are spent on other spiritual activities:",
            min = 0,
            max = 10,
            value = 2
          )
        )
    )
  ),
  tabsetPanel(
    id = "tabs",
    nav_panel(title = "Introduction", value = "intro", intro_card),
    nav_panel(title = "Design Cube(s)", value = "cube", plot_card[[2]]),
    nav_panel(title = "Assumptions", value = "assumption",
              navset_card_underline(
                nav_panel(title = "Independence", text_card[[1]]),
                nav_panel(title = "Residual vs Fitted", plot_card[[4]]),
                nav_panel(title = "Normal QQ Plot", plot_card[[5]]),
                nav_panel(title = "Outliers", text_card[[2]])
              )
    ),
    nav_panel(title = "Interaction Plot", value = "interaction", plot_card[[1]]),
    nav_panel(title = "Effects", value = "effect", plot_card[[3]]),
    footer = tagList(
      conditionalPanel(
        condition = "input.tabs != 'intro' && input.tabs != 'effect'",
        layout_columns(table_card[[2]], table_card[[1]], col_widths = c(4, 8))
      ),
      conditionalPanel(
        condition = "input.tabs == 'effect'",
        table_card[[3]]
      )
    )
  ),
  
  
)

# Define server logic
server <- function(input, output) {
  
  f <- reactiveValues(
    data = NULL,
    design = NULL,
    result = NULL,
    std_dev = NULL,
    health_value = NULL
  )
  
  factorCalc <- reactive({
    # Create a vector of the number of levels for each factor
    numSelectedFactors = length(input$phys)
    
    # Get selected factors
    first_factor <- if (numSelectedFactors >= 1)
      input$phys[1]
    else
      NULL
    second_factor <- if (numSelectedFactors >= 2)
      input$phys[2]
    else
      NULL
    third_factor <- if (numSelectedFactors >= 3)
      input$phys[3]
    else
      NULL
    
    # Set default factor labels
    firstFactorLabels <- c("Low", "High") 
    secondFactorLabels <- c("Low", "High") 
    thirdFactorLabels <- c("Low", "High")
    
    # Handle different numbers of factors
    if (numSelectedFactors == 3) {
      # For 3 factors
      levels <- c(2, 2, 2)
      factorNames <- c(first_factor, second_factor, third_factor)
      factorLabels <- labelFactors(input$phys, factor_labels)
      
      firstFactorLow <- factorLabels[[1]][[1]]
      firstFactorHigh <- factorLabels[[1]][[2]]
      
      secondFactorLow <- factorLabels[[2]][[1]]
      secondFactorHigh <- factorLabels[[2]][[2]]
      
      thirdFactorLow <- factorLabels[[3]][[1]]
      thirdFactorHigh <- factorLabels[[3]][[2]]
      
      # Create the factor vectors for a 3-factor design
      # Each combination needs 15 repetitions
      factorA <- c(
        rep(firstFactorLow, 15),
        # A low, B low, C low
        rep(firstFactorHigh, 15),
        # A high, B low, C low
        rep(firstFactorLow, 15),
        # A low, B high, C low
        rep(firstFactorHigh, 15),
        # A high, B high, C low
        rep(firstFactorLow, 15),
        # A low, B low, C high
        rep(firstFactorHigh, 15),
        # A high, B low, C high
        rep(firstFactorLow, 15),
        # A low, B high, C high
        rep(firstFactorHigh, 15)   # A high, B high, C high
      )
      
      factorB <- c(
        rep(secondFactorLow, 15),
        # A low, B low, C low
        rep(secondFactorLow, 15),
        # A high, B low, C low
        rep(secondFactorHigh, 15),
        # A low, B high, C low
        rep(secondFactorHigh, 15),
        # A high, B high, C low
        rep(secondFactorLow, 15),
        # A low, B low, C high
        rep(secondFactorLow, 15),
        # A high, B low, C high
        rep(secondFactorHigh, 15),
        # A low, B high, C high
        rep(secondFactorHigh, 15)  # A high, B high, C high
      )
      
      factorC <- c(
        rep(thirdFactorLow, 15),
        # A low, B low, C low
        rep(thirdFactorLow, 15),
        # A high, B low, C low
        rep(thirdFactorLow, 15),
        # A low, B high, C low
        rep(thirdFactorLow, 15),
        # A high, B high, C low
        rep(thirdFactorHigh, 15),
        # A low, B low, C high
        rep(thirdFactorHigh, 15),
        # A high, B low, C high
        rep(thirdFactorHigh, 15),
        # A low, B high, C high
        rep(thirdFactorHigh, 15)   # A high, B high, C high
      )
      
      # Generate simulated health data for a 3-factor design
      # Adjust means for different combinations to simulate various effects
      health <- c(
        rnorm(15, 2.5, 2.5),
        # A low, B low, C low
        rnorm(15, 3.0, 2.5),
        # A high, B low, C low
        rnorm(15, 3.5, 2.5),
        # A low, B high, C low
        rnorm(15, 4.0, 2.5),
        # A high, B high, C low
        rnorm(15, 3.2, 2.5),
        # A low, B low, C high
        rnorm(15, 3.7, 2.5),
        # A high, B low, C high
        rnorm(15, 4.2, 2.5),
        # A low, B high, C high
        rnorm(15, 4.7, 2.5)   # A high, B high, C high
      )
      
      # Create data frame with all three factors
      data <- data.frame(factorA, factorB, factorC, health)
      
      # Generate the full factorial design
      design <- gen.factorial(levels, nVars = numSelectedFactors, varNames = factorNames)
      
      # Extract variables for analysis
      A <- data$factorA
      B <- data$factorB
      C <- data$factorC
      H <- data$health
      
      # Run ANOVA with all three factors and their interactions
      f$result <- aov(H ~ A * B * C)
      
    } else if (numSelectedFactors == 2) {
      # For 2 factors
      levels <- c(2, 2)
      factorNames <- c(first_factor, second_factor)
      factorLabels <- labelFactors(input$phys, factor_labels)
      
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
    # Store the data in the reactiveValues
    f$design <- design
    f$data <- data
  })
  
  getModel <- reactive({
    numSelectedFactors = length(input$phys)
    factorCalc()
    df <- f$data
    A <- df$factorA
    B <- df$factorB
    C <- df$factorC
    H <- df$health
    
    if (numSelectedFactors == 2){
      model <- lm(H ~ A*B, data = df, 
                  contrasts = list(A = contr.FrF2, B = contr.FrF2))
    } else if (numSelectedFactors == 3){
      model <- lm(H ~ A*B*C, data = df, 
                  contrasts = list(A = contr.FrF2, B = contr.FrF2, C = contr.FrF2))
    }
    
    
    return(model)
  })
  
  # Function to generate summary without creating plots
  generate_summary <- reactive({
    # Ensure at least 2 health dimensions are selected
    if (length(input$phys) < 2) {
      print("Please select at least 2 subfactors for analysis.")
      return(NULL)
    }
    
    factorCalc()
    
    data <- f$data
    
    A <- data$factorA
    B <- data$factorB
    H <- data$health
    
    # Extract the factors based on how many are selected
    if (length(input$phys) == 2) {
      
      # Get the actual selected dimensions in their original order
      selected_dimensions <- input$phys
      
      # Create a standardized lookup system for factor combinations
      factors_lookup <- list(
        # Nutrition and Sleep combination
        "Nutrition_Sleep" = list(
          low_low = c("Mindless", "Insufficient"),
          high_high = c("Purposeful", "Sufficient")
        ),
        # Nutrition and Exercise combination
        "Nutrition_Exercise" = list(
          low_low = c("Mindless", "Light"),
          high_high = c("Purposeful", "Intense")
        ),
        # Sleep and Exercise combination
        "Sleep_Exercise" = list(
          low_low = c("Insufficient", "Light"),
          high_high = c("Sufficient", "Intense")
        )
      )
      
      # Determine which combination we're dealing with
      combination <- paste(selected_dimensions, collapse = "_")
      
      # Calculate means and standard deviations
      means <- tapply(H, list(A, B), mean)
      sd <- tapply(H, list(A, B), sd)
      
      # Get the appropriate factor level names based on the combination
      if (combination %in% names(factors_lookup) ||
          paste(rev(selected_dimensions), collapse = "_") %in% names(factors_lookup)) {
        # Get correct lookup key (handle reversed order)
        lookup_key <- ifelse(
          combination %in% names(factors_lookup),
          combination,
          paste(rev(selected_dimensions), collapse = "_")
        )
        
        # Get the factor level combinations
        low_low_levels <- factors_lookup[[lookup_key]]$low_low
        high_high_levels <- factors_lookup[[lookup_key]]$high_high
        
        # Extract specific scenarios for the summary (worst vs best case)
        if (combination %in% names(factors_lookup)) {
          # Standard order
          mod_insuf <- means[low_low_levels[1], low_low_levels[2]]
          int_suf <- means[high_high_levels[1], high_high_levels[2]]
          
          # Extract standard deviations for these scenarios
          mod_insuf_sd <- sd[low_low_levels[1], low_low_levels[2]]
        } else {
          # Reversed order (B, A instead of A, B)
          mod_insuf <- means[low_low_levels[2], low_low_levels[1]]
          int_suf <- means[high_high_levels[2], high_high_levels[1]]
          
          # Extract standard deviations for these scenarios
          mod_insuf_sd <- sd[low_low_levels[2], low_low_levels[1]]
        }
        
        # Calculate differences and approximate percentage improvement
        diff_health <- int_suf - mod_insuf
        pct_improve <- (diff_health / mod_insuf) * 100
        
        # Standard deviation-based health improvement
        diff_health_sd <- (int_suf - mod_insuf) / mod_insuf_sd
        
        f$std_dev <- round(diff_health_sd, 2)
        f$health_value <- round(pct_improve, 1)
        
      } 
    } else if (length(input$phys) == 3) {
      # If all three factors are selected, we can do a more comprehensive analysis
      C <- data$factorC
      
      # Calculate means for all combinations
      means <- tapply(H, list(A, B, C), mean)
      sd <- tapply(H, list(A, B, C), mean)
      
      # Get worst case (all low) vs best case (all high)
      worst_case <- means["Mindless", "Insufficient", "Moderate"]
      best_case <- means["Purposeful", "Sufficient", "Intense"]
      
      # Calculate standard deviation for worst case
      worst_case_sd <- sd["Mindless", "Insufficient", "Moderate"]
      
      # Calculate differences and approximate percentage improvement
      diff_health <- best_case - worst_case
      pct_improve <- (diff_health / worst_case) * 100
      
      # Standard deviation-based health improvement
      diff_health_sd <- (best_case - worst_case) / worst_case_sd
      
      f$std_dev <- round(diff_health_sd, 2)
      f$health_value <- round(pct_improve, 1)
    }
    
    return(TRUE) # Return TRUE to indicate success
  })
  
  # Update the output renderers to handle the case when fewer than 2 dimensions are selected
  output$sd <- renderText({
    isTwoFactor = length(input$phys) == 2
    if (!isTwoFactor || is.null(generate_summary())) {
      return("N/A")
    }
    return(f$std_dev)
  })
  
  output$health_change <- renderText({
    isTwoFactor = length(input$phys) == 2
    if (!isTwoFactor || is.null(generate_summary())) {
      return("N/A")
    }
    return(paste0(f$health_value, "%"))
  })
  
  output$interaction_plot <- renderPlot({
    numFactors <- length(input$phys)
    if (numFactors < 2 || numFactors == 3) {
      plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
      text(0, 0, "Please select 2 subfactors under the Physical Health panel!", cex = 1.5)
      return()
    }
    
    factorCalc()
    df <- f$data
    
    # Use the selected dimensions for plot labels
    labels <- labelFactors(input$phys, factors_plot_labels)
    
    interaction.plot(
      df$factorA, 
      df$factorB, 
      df$health,
      xlab = labels[1],
      ylab = "Overall Health Score",
      trace.label = labels[2]
    )
  })
  
  output$factorial <- renderPlotly({
    # Create factorial design grid
    df <- expand.grid(
      factorA = c(0, 1),
      factorB = c(0, 1),
      factorC = c(0, 1)
    )
    
    # Add hover text
    df$factorA_text <- ifelse(df$factorA == 0, "Low", "High")
    df$factorB_text <- ifelse(df$factorB == 0, "Low", "High")
    df$factorC_text <- ifelse(df$factorC == 0, "Low", "High")
    
    # Edge definitions and colors
    edges <- list(
      c(1, 2), c(2, 4), c(4, 3), c(3, 1),  # Bottom square
      c(5, 6), c(6, 8), c(8, 7), c(7, 5),  # Top square
      c(1, 5), c(2, 6), c(3, 7), c(4, 8)   # Vertical edges
    )
    
    #Set up the color palette 
    edge_colors <- paletteer_d("DresdenColor::paired")
    
    # Function to determine changing factor and generate edge label
    get_edge_info <- function(df, edge) {
      v1 <- df[edge[1], ]
      v2 <- df[edge[2], ]
      
      # Find which factor changes
      factor <- if (v1$factorA != v2$factorA) {
        c("sleep", v1$factorA_text, v2$factorA_text)
      } else if (v1$factorB != v2$factorB) {
        c("exercise", v1$factorB_text, v2$factorB_text)
      } else {
        c("nutrition", v1$factorC_text, v2$factorC_text)
      }
      
      # Create label
      label <- paste0(
        toupper(substr(factor[1], 1, 1)),
        substr(factor[1], 2, nchar(factor[1])),
        ": ", factor[2], " → ", factor[3]
      )
      
      return(label)
    }
    
    # Initialize plot
    plot <- plot_ly()
    
    # Add edges
    for (i in seq_along(edges)) {
      edge <- edges[[i]]
      edge_label <- get_edge_info(df, edge)
      
      plot <- plot %>% add_trace(
        x = df$factorA[edge],
        y = df$factorB[edge],
        z = df$factorC[edge],
        type = 'scatter3d',
        mode = 'lines',
        name = edge_label,
        line = list(color = edge_colors[i], width = 6),
        hoverinfo = 'skip',
        showlegend = TRUE
      )
    }
    
    # Add vertices
    hover_template <- paste(
      "Sleep: %{customdata[0]}<br>",
      "Exercise: %{customdata[1]}<br>",
      "Nutrition: %{customdata[2]}",
      "<extra></extra>"
    )
    
    custom_data <- lapply(1:nrow(df), function(i) {
      list(df$factorA_text[i], df$factorB_text[i], df$factorC_text[i])
    })
    
    plot <- plot %>% add_trace(
      x = df$factorA,
      y = df$factorB,
      z = df$factorC,
      type = 'scatter3d',
      mode = 'markers',
      name = "Factor Points",
      marker = list(size = 8, color = "black", symbol = "circle"),
      customdata = custom_data,
      hovertemplate = hover_template
    )
    
    # Layout configuration
    plot %>% layout(
      scene = list(
        xaxis = list(title = 'Sleep', showgrid = FALSE),
        yaxis = list(title = 'Exercise', showgrid = FALSE),
        zaxis = list(title = 'Nutrition', showgrid = FALSE)
      ),
      title = "3-D Representation of the 2ᵏ Factorial Design",
      legend = list(
        title = list(text = "Factors and Levels"),
        orientation = "h",
        visible = ifelse(input$isMobile, FALSE, TRUE)
      )
    )
  })
  
  output$effects_plot <- renderPlot({
    model <- getModel()
    
    # Extract coefficients (half effects)
    coeffs <- coef(model)[-1]  # Remove intercept
    
    # Create half-normal plot of effects
    par(mfrow = c(1, 2))
    
    # Normal plot
    qqnorm(coeffs, main = "Normal Plot of Effects", 
           xlab = "Normal Scores", ylab = "Estimated Effects", 
           pch = 16, col = "blue")
    abline(lm(sort(coeffs) ~ qnorm(ppoints(length(coeffs)))))
    
    # Half-normal plot
    abs_coeffs <- abs(coeffs)
    qqnorm(abs_coeffs, main = "Half-Normal Plot of Effects", 
           xlab = "Half-Normal Scores", ylab = "Absolute Effects", 
           pch = 16, col = "red", plot.it = FALSE)
    plot_data <- qqnorm(abs_coeffs, plot.it = FALSE)
    plot(plot_data$x, plot_data$y, main = "Half-Normal Plot of Effects", 
         xlab = "Half-Normal Scores", ylab = "Absolute Effects", 
         pch = 16, col = "red")
    
    # Add labels for significant effects
    parnames <- names(coeffs)
    labeled <- abs_coeffs > (2 * sd(abs_coeffs))
    if (any(labeled)) {
      factor_labels <- formatSigEffect(parnames[labeled], input$phys)
      text(plot_data$x[labeled], plot_data$y[labeled], 
           labels = factor_labels, pos = 2)
    }
  })
  
  output$independence <- renderPrint({
    cat("When looking at our factors they are orthogonal since they do not depend on one another. Thus this design fufills the condition of independence.")
  })
  
  output$residual <- renderPlot({
    numFactors <- length(input$phys)
    if (numFactors < 2) {
      return()
    }
    model <- getModel()
    plot(fitted(model), residuals(model), 
         xlab = "Fitted Values", ylab = "Residuals",
         main = "Residuals vs Fitted", pch = 19, col = "blue")
    abline(h = 0, lty = 2, col = "red")
  })
  
  output$QQ <- renderPlot({
    numFactors <- length(input$phys)
    if (numFactors < 2) {
      return()
    }
    model <- getModel()
    qqnorm(residuals(model), main = "Normal Q-Q Plot", pch = 19, col = "blue")
    qqline(residuals(model), col = "red", lwd = 2)
  })
  
  output$outlier <- renderPrint({
    numFactors <- length(input$phys)
    if (numFactors < 2) {
      return()
    }
    model <- getModel()
    df <- f$data
    tryCatch({
      cat("Gaptest Analysis for Outlier Detection:\n")
      gap_result <- Gaptest(df)
      
      if (is.null(gap_result)){
        cat("There are no outliers in this dataset!")
      } else {
        print(gap_result)
      }
    }, error = function(e) {
      cat("Error in outlier detection: ", e$message, "\n")
      cat("Cook's Distances for potential outliers:\n")
      cooks_d <- cooks.distance(model)
      print(sort(cooks_d[cooks_d > 4/length(cooks_d)], decreasing = TRUE))
    })
  })
  
  output$effects_output <- renderUI({
    model <- getModel()
    
    # Extract coefficients (half effects)
    coeffs <- coef(model)[-1]  # Remove intercept
    
    # Calculate full effects (2 × coefficients)
    effects <- 2 * coeffs
    
    # Create a data frame of effects
    effects_df <- data.frame(
      Estimate = effects,
      `Half_Effect` = coeffs,
      `Std_Error` = summary(model)$coefficients[-1, "Std. Error"],
      `t_value` = summary(model)$coefficients[-1, "t value"],
      `Pr(>|t|)` = summary(model)$coefficients[-1, "Pr(>|t|)"]
    )
    sig_effects <- which(effects_df[[5]] < 0.05)
    
    effects_df <- labelTableFactors(effects_df,input$phys)
    
    
    HTML(
      kable(effects_df, row.names = TRUE) %>%
        kable_styling(
          bootstrap_options = c("striped", "responsive"),
          full_width = FALSE,
          position = "left"
        ) %>%
        # Highlight significant p-values
        row_spec(
          sig_effects,
          bold = TRUE,
          background = "#e67763",
          color = "#FFF"
        ) %>%
        # Add footer with significance key
        add_footnote(
          c("Significant Effects are highlighted if p < 0.05"),
          notation = "none"
        )
    )
    
  })
  
  output$anova <- renderUI({
    if (length(input$phys) < 2){
      return()
    }
    factorCalc()
    table <- displayTable(f,input$phys, "anova")
  })
  
  output$design <- renderUI({
    if (length(input$phys) < 2){
      return()
    }
    factorCalc()
    table <- displayTable(f, input$phys, "design")
  })
  
}

# Run the application
shinyApp(ui, server)
