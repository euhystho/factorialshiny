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
library(thematic)
library(paletteer)

#Plot related libraries
library(plotly)
library(kableExtra)
library(tidyverse)
library(dae)

#Factorial Design related libraries
library(AlgDesign)


cards <- list(
  card(
    card_header("Interaction Plot"),
    min_height = 625,
    card_body(
      layout_columns(
        value_box(
          title = "Standard Deviation",
          value = textOutput("sd"),
          showcase = bs_icon("badge-sd-fill"),
        ),
        value_box(
          title = "Change in Health",
          value = textOutput("health_change"),
          showcase = bs_icon("heart-pulse-fill"),
        ),
      ),
      plotOutput("interaction_plot")
    ),
    
  ),
  card(
    card_header("ANOVA Table"), 
    min_height = 10, 
    tableOutput("anova")
    ),
  card(
    card_header("Factorial Design Table"),
    min_height = 10,
    uiOutput("design")
  ),
  card(
    card_header("Factorial Design Plot through Plotly"),
    min_height = 625,
    plotlyOutput("factorial")
  ),
  card(
    
    h5("Healthy Habits Circle Simulated Experiment Design", style = "font-weight: bold;"),
    p("2³ factorial design with the following factors:"),
    
    tags$div(
      style = "margin-left: 15px;",
      HTML("<b>Nutrition:</b> <i>Nutrition Intention</i>"),
      tags$ul(
        tags$li(HTML("Mindless: Not planning and practicing mindful eating habits")),
        tags$li("Purposeful: Tracking calories and practicing mindful eating habits")
      ),
      HTML("<b>Sleep:</b> <i>Sleep Hours</i>"),
      tags$ul(
        tags$li("Insufficient: < 7 hours of sleep"),
        tags$li("Sufficient: >= 7 hours of sleep")
      ), 
      
      HTML("<b>Exercise:</b> <i>Exercise Intensity</i>"),
      tags$ul(
        tags$li("Moderate: Engaging in less-strenuous activities"),
        tags$li("Intense: Engaging in more-strenuous activities")
      )
    ),
    
    p(HTML("<b>Response:</b> <i>Health Score</i>"))
  )
)


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
  
  # Sidebar with a slider input for number of bins
  sidebar = sidebar(
    title = "Health Dimensions",
    width = 300,
    fillable = TRUE,
    accordion_filters <- accordion(
      id = "dims",
      accordion_panel(
        "Physical Health",
        icon = bs_icon("heart-pulse-fill"),
        checkboxGroupButtons(
          "phys",
          "Select the following subfactors:",
          choices = c("Nutrition", "Sleep", "Exercise"),
          selected = c("Nutrition", "Sleep"),
          size = 'sm',
        ),
        div(
          id = "nutrition",
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
        div(
          id = "sleep",
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
        div(
          id = "exercise",
          card(
            card_header("Exercise"),
            sliderTextInput(
              "swim_slider",
              "Choose the intensity of Swimming:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Moderate", "Intense")
            ),
            sliderTextInput(
              "run_slider",
              "Choose the intensity of Running:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Moderate", "Intense")
            ),
            sliderTextInput(
              "walk_slider",
              "Choose the intensity of Walking:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Moderate", "Intense")
            ),
            sliderTextInput(
              "hike_slider",
              "Choose the intensity of Hiking:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Moderate", "Intense")
            ),
            sliderTextInput(
              "bike_slider",
              "Choose the intensity of Biking:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Moderate", "Intense")
            ),
            sliderTextInput(
              "team_slider",
              "Choose the intensity of Team Sport:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Moderate", "Intense")
            ),
            sliderTextInput(
              "weight_slider",
              "Choose the intensity of Weightlifting:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Moderate", "Intense")
            ),
            sliderTextInput(
              "other_exerc_slider",
              "Choose the intensity of Other Exercise:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Moderate", "Intense")
            ),
          )
        ),
      ),
      accordion_panel(
        "Social Health",
        icon = bs_icon("people-fill"),
        card (
          sliderInput(
            "socialhours_slider",
            "Specify amount of hours socializing with non-friends per week:",
            min = 0,
            max = 21,
            value = 10
          ),
          sliderInput(
            "vol_slider",
            "Specify the amount of time a week spent doing community service:",
            min = 0,
            max = 30,
            value = 10
          ),
          sliderInput(
            "friends_slider",
            "Specify the amount of hours socializing with friends per week:",
            min = 0,
            max = 25,
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
            "Choose how many hours spent journalling:",
            min = 0,
            max = 5,
            value = 3
          ),
          sliderInput(
            "mental_slider",
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
            max = 3,
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

      ),
    ),
  ),
  tabsetPanel(
    tabPanel(title = "Introduction", cards[[5]]),
    tabPanel(title = "Interaction Plot", cards[[1]]),
    tabPanel(title = "Design Cube(s)", cards[[4]]),
    layout_columns(cards[[3]], cards[[2]], col_widths = c(4,8))
  )
  
)

# Define server logic
server <- function(input, output) {
  #Change some thematics before using this:
  
  # thematic_shiny()
  
  f <- reactiveValues(
    data = NULL,
    design = NULL,
    result = NULL,
    std_dev = NULL,
    health_value = NULL
  )
  
  
  # Hide all the Sliders by default
  observe({
    isolate({
      shinyjs::hide("nutrition")
      shinyjs::hide("sleep")
      shinyjs::hide("exercise")
    })
  })

  observeEvent(input$phys, {
    # Hides all the Sliders
    shinyjs::hide("nutrition")
    shinyjs::hide("sleep")
    shinyjs::hide("exercise")

    # Dynamically shows slider based on which Subfactor is chosen:
    if ("Nutrition" %in% input$phys) {
      shinyjs::show("nutrition")
    }
    if ("Sleep" %in% input$phys) {
      shinyjs::show("sleep")
    }
    if ("Exercise" %in% input$phys) {
      shinyjs::show("exercise")
    }
  })

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
      factorLabels <- labelFactors(factorNames, numSelectedFactors)
      
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
      f$design <- design
      f$data <- data
      f$result <- aov(H ~ A * B * C)
      
    } else if (numSelectedFactors == 2) {
      # For 2 factors
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
      f$design <- design
      f$data <- data
      f$result <- aov(H ~ A * B)
      
    }
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
    
    # Extract the factors based on how many are selected
    if (length(input$phys) == 2) {
      A <- data$factorA
      B <- data$factorB
      H <- data$health
      
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
          low_low = c("Mindless", "Moderate"),
          high_high = c("Purposeful", "Intense")
        ),
        # Sleep and Exercise combination
        "Sleep_Exercise" = list(
          low_low = c("Insufficient", "Moderate"),
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
        
      } else {
        showNotification(
          paste(
            "Invalid combination: ",
            combination,
            ". Please use a combination from: 'Nutrition', 'Sleep', and 'Exercise'."
          )
        )
      }
    } else if (length(input$phys) == 3) {
      # If all three factors are selected, we can do a more comprehensive analysis
      A <- data$factorA
      B <- data$factorB
      C <- data$factorC
      H <- data$health
      
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
    if (length(input$phys) < 2 || is.null(generate_summary())) {
      return("N/A")
    }
    return(f$std_dev)
  })
  
  output$health_change <- renderText({
    if (length(input$phys) < 2 || is.null(generate_summary())) {
      return("N/A")
    }
    return(paste0(f$health_value, "%"))
  })
  
  output$interaction_plot <- renderPlot({
    if (length(input$phys) < 2) {
      plot(0,
           0,
           type = "n",
           axes = FALSE,
           ann = FALSE)
      text(0,
           0,
           "Please select 2 subfactors under the Physical Health panel!",
           cex = 1.5)
      return()
    }
    
    factorCalc()
    df <- f$data
    A <- df$factorA
    B <- df$factorB
    H <- df$health
    
    # Use the selected dimensions for plot labels
    sleepLabel <- "Sleep Hours"
    exerciseLabel <- "Exercise Intensity"
    nutritionLabel <- "Nutrition Intention"
    
    numFactors <- length(input$phys)
    
    sleepIndex <- match("Sleep", input$phys)
    exerciseIndex <- match("Exercise", input$phys)
    nutritionIndex <- match("Nutrition", input$phys)
    
    if (!is.na(sleepIndex)) {
      if (sleepIndex == 1) {
        factorALabel = sleepLabel
      } else if (sleepIndex == 2) {
        factorBLabel = sleepLabel
      } else if (sleepIndex == 3){
        factorCLabel = sleepLabel
      }
    }
    
    if (!is.na(exerciseIndex)) {
      if (exerciseIndex == 1) {
        factorALabel = exerciseLabel
      } else if (exerciseIndex == 2) {
        factorBLabel = exerciseLabel
      } else if (exerciseIndex == 3){
        factorCLabel = exerciseLabel
      }
    }
    
    if (!is.na(nutritionIndex)) {
      if (nutritionIndex == 1) {
        factorALabel = nutritionLabel
      } else if (nutritionIndex == 2) {
        factorBLabel = nutritionLabel
      }else if (nutritionIndex == 3){
        factorCLabel = nutritionLabel
      }
    }
    
    
    if (numFactors == 3){
      plot(0,
           0,
           type = "n",
           axes = FALSE,
           ann = FALSE)
      text(0,
           0,
           "Please select 2 subfactors under the Physical Health panel!",
           cex = 1.5)
      return()
    } else {
      interaction.plot(A,
                       B,
                       H,
                       xlab = factorALabel ,
                       ylab = "Overall Health Score",
                       trace.label = factorBLabel)
    }
    

  })
  
  output$factorial <- renderPlotly({
    # Define the Factors and Levels
    factors = 3
    levels = c(0, 1)
    
    # Create factorial design grid reducing time complexity from O(N^3) to O(1)
    df <- expand.grid(sleep = levels,
                      exercise = levels,
                      nutrition = levels)
    
    # Add hovertext directly to the dataframe and factors
    df$sleep_hovertext <- ifelse(df$sleep == 0, "Low", "High")
    df$exercise_hovertext <- ifelse(df$exercise == 0, "Low", "High")
    df$nutrition_hovertext <- ifelse(df$nutrition == 0, "Low", "High")
    
    # Initialize plot
    plot <- plot_ly()
    
    # Define the edges of the cube as pairs of vertex indices
    edges <- list(
      # Bottom square
      c(1, 2),
      c(2, 4),
      c(4, 3),
      c(3, 1),
      # Top square
      c(5, 6),
      c(6, 8),
      c(8, 7),
      c(7, 5),
      # Vertical edges
      c(1, 5),
      c(2, 6),
      c(3, 7),
      c(4, 8)
    )
    
    # Define colors for different factors
    factor_colors <- list(
      sleep = list(low = "#1f77b4", high = "#17becf"),
      exercise = list(low = "#ff7f0e", high = "#d62728"),
      nutrition = list(low = "#2ca02c", high = "#9467bd")
    )
    
    # Get a color palette for edges
    edge_colors <- paletteer_d("DresdenColor::paired")
    
    # Function to determine which factor changes along an edge
    determine_changing_factor <- function(df, edge_indices) {
      v1 <- df[edge_indices[1], ]
      v2 <- df[edge_indices[2], ]
      
      if (v1$sleep != v2$sleep)
        return("sleep")
      if (v1$exercise != v2$exercise)
        return("exercise")
      if (v1$nutrition != v2$nutrition)
        return("nutrition")
      return("none")  # No change (shouldn't happen in a proper cube)
    }
    
    # Function to generate edge label based on which factor changes
    generate_edge_label <- function(df, edge_indices, factor) {
      v1 <- df[edge_indices[1], ]
      v2 <- df[edge_indices[2], ]
      
      # Determine direction of change
      if (factor == "sleep") {
        from_val <- v1$sleep_hovertext
        to_val <- v2$sleep_hovertext
      } else if (factor == "exercise") {
        from_val <- v1$exercise_hovertext
        to_val <- v2$exercise_hovertext
      } else if (factor == "nutrition") {
        from_val <- v1$nutrition_hovertext
        to_val <- v2$nutrition_hovertext
      } else {
        return("Edge")  # Fallback, shouldn't happen
      }
      
      # Create label with factor name and direction
      return(paste0(
        toupper(substr(factor, 1, 1)),
        substr(factor, 2, nchar(factor)),
        ": ",
        from_val,
        " → ",
        to_val
      ))
    }
    
    
    # Draw all the edges on plotly with colors and labels
    for (i in 1:length(edges)) {
      edge <- edges[[i]]
      
      # Determine what factor changes along this edge
      changing_factor <- determine_changing_factor(df, edge)
      
      # Generate edge label
      edge_label <- generate_edge_label(df, edge, changing_factor)
      
      plot <- plot %>% add_trace(
        x = df$sleep[edge],
        y = df$exercise[edge],
        z = df$nutrition[edge],
        type = 'scatter3d',
        mode = 'lines',
        name = edge_label,
        line = list(color = edge_colors[i], width = 6),
        hoverinfo = 'skip',
        showlegend = TRUE    # Add to legend
      )
    }
    
    # Add all the points on plotly (vertices)
    plot <- plot %>% add_trace(
      x = df$sleep,
      y = df$exercise,
      z = df$nutrition,
      type = 'scatter3d',
      mode = 'markers',
      name = "Factor Points",
      marker = list(
        size = 8,
        color = "black",
        symbol = "circle"
      ),
      hovertemplate = paste(
        "Sleep:",
        df$sleep_hovertext,
        "<br>",
        "Exercise:",
        df$exercise_hovertext,
        "<br>",
        "Nutrition:",
        df$nutrition_hovertext,
        # Extra removes to the trace number
        "<extra></extra>"
      )
    )
    
    # Layout configuration
    plot <- plot %>% layout(
      scene = list(
        xaxis = list(title = 'Sleep', showgrid = FALSE),
        yaxis = list(title = 'Exercise', showgrid = FALSE),
        zaxis = list(title = 'Nutrition', showgrid = FALSE)
      ),
      title = "3-D Representation of the 2^k Factorial Design",
      legend = list(
        title = list(text = "Factors and Levels"),
        orientation = "h",
        yanchor = "bottom",
        y = -0.2
      )
    )
    
    plot
  })
  
  output$anova <- renderUI({
    if (length(input$phys) < 2){
      return()
    }
    factorCalc()
    
    # Get the ANOVA results
    anova_results <- anova(f$result)
    
    # Convert to data frame for better handling
    anova_df <- as.data.frame(anova_results)
    
    # Round numeric columns to 3 decimal places
    numeric_cols <- sapply(anova_df, is.numeric)
    anova_df[, numeric_cols] <- round(anova_df[, numeric_cols], 3)
    
    # Format p-values with significance stars
    if ("Pr(>F)" %in% colnames(anova_df)) {
      # Create a new column for p-value display
      anova_df$Significance <- ""
      
      # Add significance stars
      anova_df$Significance[anova_df$`Pr(>F)` < 0.001] <- "***"
      anova_df$Significance[anova_df$`Pr(>F)` >= 0.001 &
                              anova_df$`Pr(>F)` < 0.01] <- "**"
      anova_df$Significance[anova_df$`Pr(>F)` >= 0.01 &
                              anova_df$`Pr(>F)` < 0.05] <- "*"
      anova_df$Significance[anova_df$`Pr(>F)` >= 0.05 &
                              anova_df$`Pr(>F)` < 0.1] <- "."
      
      # Format p-values for better readability
      anova_df$`Pr(>F)` <- sprintf("%.3f %s", anova_df$`Pr(>F)`, anova_df$Significance)
      
      # Remove the separate significance column
      anova_df$Significance <- NULL
    }
    
    # Get all factor names
    selected_factors <- input$phys
    if (length(selected_factors) == 2) {
      row.names(anova_df)[1:3] = c(selected_factors[1], selected_factors[2], "Interaction")
    } else if (length(selected_factors) == 3) {
      row.names(anova_df)[1:7] = c(
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
    # Create nicely formatted table
    HTML(
      kable(anova_df, row.names = TRUE) %>%
        kable_styling(
          bootstrap_options = c("striped", "responsive"),
          full_width = FALSE,
          position = "left"
        ) %>%
        # Highlight significant p-values
        row_spec(
          which(anova_results[, "Pr(>F)"] < 0.05),
          bold = TRUE,
          background = "#e67763"
        ) %>%
        # Add footer with significance key
        add_footnote(
          c("Significance codes: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1"),
          notation = "none"
        )
    )
  })
  
  output$design <- renderUI({
    if (length(input$phys) < 2){
      return()
    }
    factorCalc()
    data <- as.data.frame(f$design)
    
    # Get the selected factors
    selected_factors <- input$phys
    
    # Create the base table
    base_table <- kable(data, row.names = TRUE) %>%
      kable_styling(bootstrap_options = c("striped", "responsive"),
                    full_width = FALSE)
    
    # Apply column specifications based on which factors are selected
    table_with_styling <- base_table
    
    # Check if each factor exists and apply appropriate styling
    for (i in seq_along(selected_factors)) {
      factor_name <- selected_factors[i]
      col_index <- i + 1  # +1 because column 1 is row names
      
      # Only apply spec_color if the column exists and contains numeric or factor data
      if (factor_name %in% colnames(data)) {
        # Check if column is numeric or can be converted to numeric
        col_data <- data[[factor_name]]
        
        # For factors, convert to numeric 1/2 for Low/High
        if (is.factor(col_data) || is.character(col_data)) {
          # Create a safe numeric representation for coloring
          numeric_values <- as.numeric(as.factor(col_data))
          
          table_with_styling <- table_with_styling %>%
            column_spec(
              col_index,
              color = 'white',
              background = spec_color(numeric_values, end = 0.7, option = 'A')
            )
        } else if (is.numeric(col_data)) {
          # If it's already numeric, use it directly
          table_with_styling <- table_with_styling %>%
            column_spec(
              col_index,
              color = 'white',
              background = spec_color(col_data, end = 0.7, option = 'A')
            )
        }
      }
    }
    
    # Return the HTML
    HTML(table_with_styling)
  })
  
  
}

# Run the application
shinyApp(ui, server)