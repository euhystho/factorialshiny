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
library(echarts4r)
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
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  # Mobile Friendly:
  fillable_mobile = TRUE,
  mobileDetect('isMobile'),
  
  # Sidebar with a slider input for number of bins
  sidebar = sidebar(
    title = "Health Dimensions",
    width = 300,
    fillable = TRUE,
    selectInput(
      inputId = "person",
      label = "Select one of the individuals:",
      choices = unique(individuals$name),
      selected = unique(individuals$name)[1]
    ),
    checkboxGroupButtons(
      "factors",
      "Select the following subfactors:",
      choices = c("Nutrition", "Sleep", "Exercise", "Spirituality", "Socialization", "Wellbeing"),
      selected = c("Nutrition","Sleep"),
      justified = TRUE,
      size = 'sm',
    ),
    accordion_filters <- accordion(
      id = "dims",
      accordion_panel(
        "Participant Biography",
        icon = bs_icon("file-person-fill"),
        uiOutput("bio")
      ),
      accordion_panel(
        "Physical Health",
        icon = bs_icon("heart-pulse-fill"),
        checkboxGroupButtons(
          "phys",
          "Select the following subfactors:",
          choices = c("Nutrition", "Sleep", "Exercise"),
          selected = c("Nutrition","Sleep"),
          size = 'sm',
        ),
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
              value = c(6, 9)
            ),
            sliderTextInput(
              "sleep_qual_slider",
              "Choose the quality of Sleep:",
              grid = TRUE,
              force_edges = TRUE,
              choices = c("Poor", "Good", "Excellent")
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
            ),
          )
        ),
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
        
      ),
    ),
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
server <- function(input, output, session) {
  
  f <- reactiveValues(
    data = NULL,
    design = NULL,
    result = NULL,
    std_dev = NULL,
    health_value = NULL
  )
  
  factorCalc <- reactive({
    # Create a vector of the number of levels for each factor
    numSelectedFactors = length(input$factors)
    
    # Get selected factors
    first_factor <- if (numSelectedFactors >= 1)
      input$factors[1]
    else
      NULL
    second_factor <- if (numSelectedFactors >= 2)
      input$factors[2]
    else
      NULL
    third_factor <- if (numSelectedFactors >= 3)
      input$factors[3]
    else
      NULL
    
    # Set default factor labels
    firstFactorLabels <- c("Low", "High")  
    secondFactorLabels <- c("Low", "High") 
    thirdFactorLabels <- c("Low", "High")
    
    levels <- rep(2,numSelectedFactors)
    
    # Handle different numbers of factors
    if (numSelectedFactors == 3) {
      # For 3 factors
      factorNames <- c(first_factor, second_factor, third_factor)
      factorLabels <- labelFactors(input$factors, factor_labels)
      
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
    
    # Store the data in the reactiveValues
    f$design <- design
    f$data <- data
  })
  
  getModel <- reactive({
    numSelectedFactors = length(input$factors)
    factorCalc()
    df <- f$data
    
    A <- df$factorA
    B <- df$factorB
    C <- df$factorC
    D <- df$factorD
    E <- df$factorE
    H <- df$health
    
    factors <- c("A", "B", "C", "D", "E")  # List all possible factors
    selected_factors <- factors[1:numSelectedFactors]
    if (numSelectedFactors >= 2){
      formula_str <- paste("H ~", paste(selected_factors, collapse="*"))
      contrasts_list <- setNames(
        replicate(numSelectedFactors, contr.FrF2, simplify = FALSE),
        selected_factors
      )
      model <- lm(as.formula(formula_str), data = df, contrasts = contrasts_list)
    }
    return(model)
  })
  
  
  
  # Function to generate summary without creating plots
  generate_summary <- reactive({
    # Ensure at least 2 health dimensions are selected
    if (length(input$factors) < 2) {
      f$std_dev <- NULL
      f$health_value <- NULL
      return(NULL)
    }
    
    factorCalc()
    data <- f$data
    factors_lookup <- lookupFactors()
    
    # Initialize default values
    f$std_dev <- NA
    f$health_value <- NA
    
    # Extract the factors (only handle 2 factors for now)
    if (length(input$factors) == 2) {
      # Get the two selected factors
      selected_factors <- input$factors
      factor1 <- selected_factors[1]
      factor2 <- selected_factors[2]
      
      # Get factor labels from factor_labels
      low1 <- factor_labels[[factor1]][[1]]  # Low level for first factor
      high1 <- factor_labels[[factor1]][[2]] # High level for first factor
      low2 <- factor_labels[[factor2]][[1]]  # Low level for second factor
      high2 <- factor_labels[[factor2]][[2]] # High level for second factor
      
      
      A <- data$factorA
      B <- data$factorB
      H <- data$health
      
      # Map the factor levels to numeric or categorical for tapply
      # Assume factorA and factorB in data are already labeled as "Low"/"High" or similar
      levels_A <- unique(A)
      levels_B <- unique(B)
      
      # Ensure levels match factor_labels
      if (!all(levels_A %in% c(low1, high1)) || !all(levels_B %in% c(low2, high2))) {
        warning("Factor levels in data do not match expected labels. Attempting to map.")
        # Map data levels to low/high
        A_mapped <- ifelse(A %in% c(low1), "Low", "High")
        B_mapped <- ifelse(B %in% c(low2), "Low", "High")
      } else {
        A_mapped <- A
        B_mapped <- B
      }
      
      # Calculate means and standard deviations
      means <- tapply(H, list(A_mapped, B_mapped), mean, na.rm = TRUE)
      sd <- tapply(H, list(A_mapped, B_mapped), sd, na.rm = TRUE)
      
      
      # Define low_low and high_high based on factor_labels (no need for complex lookup)
      low_low <- c(low1, low2)  # Worst case: both factors at low level
      high_high <- c(high1, high2)  # Best case: both factors at high level
      
      # Extract indices for low_low and high_high
      low_low_idx <- c(match(low_low[1], dimnames(means)[[1]]), match(low_low[2], dimnames(means)[[2]]))
      high_high_idx <- c(match(high_high[1], dimnames(means)[[1]]), match(high_high[2], dimnames(means)[[2]]))
      
      
      # Extract values
      mod_insuf <- means[low_low_idx[1], low_low_idx[2]]  # Worst case
      int_suf <- means[high_high_idx[1], high_high_idx[2]]  # Best case
      
      mod_insuf_sd <- sd[low_low_idx[1], low_low_idx[2]]  # SD for worst case

      
      # Calculate differences and approximate percentage improvement
      diff_health <- int_suf - mod_insuf
      pct_improve <- if (mod_insuf != 0) (diff_health / mod_insuf) * 100 else 0  # Avoid division by zero
      
      # Standard deviation-based health improvement
      diff_health_sd <- if (mod_insuf_sd != 0) (int_suf - mod_insuf) / mod_insuf_sd else 0  # Avoid division by zero
      
      # Store results
      f$std_dev <- round(diff_health_sd, 2)
      f$health_value <- round(pct_improve, 1)
    }
    
    return(TRUE)  # Indicate success
  })
  
  selected_data <- reactive({
    req(input$person) 
    row <- which(individuals$name == input$person)
    if (length(row) == 0) return(NULL)
    individuals[row, ]  
  })
  
  observe({
    data <- selected_data()
    if (is.null(data)) return()
    
    for (col in names(data)){
      if (col == "person") next
      
      value <- data[[col]]
      
      if(is.numeric(value) || is.list(value)){
        if (is.list(value)) {
          value <- unlist(value)  # Ensure it's a numeric vector
        }
        updateSliderInput(session, col, value = value)
      } else if (is.character(value)) {
        updateSliderTextInput(session, col, selected = value)
      } 
    }
  })
  
  # Update the output renderers to handle the case when fewer than 2 dimensions are selected
  output$sd <- renderText({
    isTwoFactor = length(input$factors) == 2
    if (!isTwoFactor || is.null(generate_summary())) {
      return("N/A")
    }
    return(f$std_dev)
  })
  
  output$health_change <- renderText({
    isTwoFactor = length(input$factors) == 2
    if (!isTwoFactor || is.null(generate_summary())) {
      return("N/A")
    }
    return(paste0(f$health_value, "%"))
  })
  
  output$interaction_plot <- renderEcharts4r({
    numFactors <- length(input$factors)
    if (numFactors < 2 || numFactors == 3) {
      plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
      text(0, 0, "Please select 2 subfactors under the Physical Health panel!", cex = 1.5)
      return()
    }
    
    factorCalc()
    df <- f$data
    
    # Use the selected dimensions for plot labels
    labels <- labelFactors(input$factors, factors_plot_labels)
    
    # Summarize the data to get means for each combination (similar to interaction.plot)
    sum_df <- df %>%
      group_by(factorA, factorB) %>%
      summarise(Mean_Health = mean(health), .groups = 'keep') %>%
      ungroup()
    
    plot <- sum_df %>%
      group_by(factorB) %>%
      e_charts(x = factorA) %>%  
      e_line(serie = Mean_Health, legend = TRUE) %>%  
      e_aria(enabled = TRUE, decal = list(show = TRUE)) %>%
      e_tooltip(trigger = "axis") %>%  # Show tooltip on hover
      e_title(text = "Interaction Plot") %>%  # Title
      e_x_axis(name = labels[1]) %>%  # X-axis label
      e_y_axis(name = "Health") %>%  # Y-axis label
      e_toolbox_feature("dataZoom") %>%
      e_toolbox_feature(feature = "reset") %>%
      e_legend(formatter = paste0("{name} ", labels[2]))  # Legend title
    plot
    # interaction.plot(
    #   df$factorA, 
    #   df$factorB, 
    #   df$health,
    #   xlab = labels[1],
    #   ylab = "Overall Health Score",
    #   trace.label = labels[2]
    # )
  })
  
  output$factorial <- renderPlotly({
    # Create factorial design grid
    df <- expand.grid(
      factorA = c(0, 1),
      factorB = c(0, 1),
      factorC = c(0, 1)
    )
    
    # Add hover text using dynamic factor_names
    df$factorA_text <- ifelse(df$factorA == 0, 
                              factor_labels[[factor_names[1]]][[1]], 
                              factor_labels[[factor_names[1]]][[2]])
    df$factorB_text <- ifelse(df$factorB == 0, 
                              factor_labels[[factor_names[2]]][[1]], 
                              factor_labels[[factor_names[2]]][[2]])
    df$factorC_text <- ifelse(df$factorC == 0, 
                              factor_labels[[factor_names[3]]][[1]], 
                              factor_labels[[factor_names[3]]][[2]])
    
    # Edge definitions 
    edges <- list(
      c(1, 2), c(2, 4), c(4, 3), c(3, 1),  # Bottom square
      c(5, 6), c(6, 8), c(8, 7), c(7, 5),  # Top square
      c(1, 5), c(2, 6), c(3, 7), c(4, 8)   # Vertical edges
    )
    
    # Set up the color palette
    edge_colors <- paletteer_d("DresdenColor::paired")
    
    # Function to determine changing factor and generate edge label dynamically
    get_edge_info <- function(df, edge, factor_names) {
      v1 <- df[edge[1], ]
      v2 <- df[edge[2], ]
      
      # Identify which factor changes (columns 1:3 are factorA, factorB, factorC)
      diff_factor <- which(v1[1:3] != v2[1:3])
      if (length(diff_factor) != 1) stop
      ("Error: Exactly one factor should change per edge")
      
      # Get the factor name and levels dynamically
      factor_name <- factor_names[diff_factor]
      level1 <- v1[[paste0("factor", LETTERS[diff_factor], "_text")]]
      level2 <- v2[[paste0("factor", LETTERS[diff_factor], "_text")]]
      
      # Create label with proper capitalization
      label <- paste0(
        toupper(substr(factor_name, 1, 1)),
        substr(factor_name, 2, nchar(factor_name)),
        ": ", level1, " → ", level2
      )
      
      return(label)
    }
    
    # Initialize plot 
    plot <- plot_ly()
    
    # Add edges with dynamic labels
    for (i in seq_along(edges)) {
      edge <- edges[[i]]
      edge_label <- get_edge_info(df, edge, factor_names)
      
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
    hover_template <- paste(factor_names[1], ": %{customdata[0]}<br>",
                            factor_names[2], ": %{customdata[1]}<br>",
                            factor_names[3], ": %{customdata[2]}",
                            "<extra></extra>")
    
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
        xaxis = list(title = factor_names[1], showgrid = FALSE),
        yaxis = list(title = factor_names[2], showgrid = FALSE),
        zaxis = list(title = factor_names[3], showgrid = FALSE)
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
      factor_labels <- formatSigEffect(parnames[labeled], input$factors)
      text(plot_data$x[labeled], plot_data$y[labeled], 
           labels = factor_labels, pos = 2)
    }
  })
  
  output$independence <- renderPrint({
    cat("When looking at our factors they are orthogonal since they do not depend on one another. \nThus, this design fufills the condition of independence.")
  })
  
  output$residual <- renderPlot({
    numFactors <- length(input$factors)
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
    numFactors <- length(input$factors)
    if (numFactors < 2) {
      return()
    }
    model <- getModel()
    qqnorm(residuals(model), main = "Normal Q-Q Plot", pch = 19, col = "blue")
    qqline(residuals(model), col = "red", lwd = 2)
  })
  
  output$outlier <- renderPrint({
    numFactors <- length(input$factors)
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
    
    effects_df <- labelTableFactors(effects_df,input$factors)
    
    
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
    if (length(input$factors) < 2){
      return()
    }
    factorCalc()
    table <- displayTable(f,input$factors, "anova")
  })
  
  output$design <- renderUI({
    if (length(input$factors) < 2){
      return()
    }
    factorCalc()
    table <- displayTable(f, input$factors, "design")
  })
  
  output$bio <- renderUI({
    bio <- as.character(individuals[trimws(individuals$name) == input$person,
                                    "biography"][1])
  })
  

  
}

# Run the application
shinyApp(ui, server)
