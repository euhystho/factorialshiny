library(shiny)
library(AlgDesign)
library(dplyr)
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
  
  # Sidebar with a slider input for number of bins
  sidebar = sidebar(
    title = "Health Dimensions",
    width = 300,
    fillable = TRUE,
    checkboxGroupButtons(
      "factors",
      "Select the following categories:",
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
          "Select to display the following subfactors:",
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
server <- function(input, output, session) {
  # Reactive function to calculate factorial design and health scores
  factorCalc <- reactive({
    req(input$factors) # Ensure factors are selected
    selected_factors <- input$factors
    num_factors <- length(selected_factors)
    
    # Validate number of factors
    if (num_factors < 2 || num_factors > 6) {
      return(NULL)
    }
    
    # Define slider mappings and scoring functions
    slider_mappings <- list(
      "Nutrition" = list(
        sliders = c("calorie_slider", "protein_slider", "fat_slider", "carb_slider", "other_nutr_slider"),
        score_func = function(value) {
          ifelse(value == "At Target", 1, 0)
        }
      ),
      "Sleep" = list(
        sliders = c("sleep_quan_slider", "sleep_qual_slider", "sleep_eat_slider", "screentime_slider", "melatonin_slider"),
        score_func = function(slider, value) {
          if (slider == "sleep_quan_slider") {
            # Assume single value; optimal is 8 hours
            value <- mean(value) # Handle range if misinterpreted
            exp(-((value - 8)^2) / 4)
          } else if (slider == "sleep_qual_slider") {
            if (value == "Excellent") 1 else if (value == "Good") 0.5 else 0
          } else if (slider == "sleep_eat_slider") {
            # Optimal: 2-3 hours before bed
            ifelse(value >= 2 & value <= 3, 1, 0.5)
          } else if (slider == "screentime_slider") {
            # Less screentime is better; optimal is 0-1 hours
            1 - (value / 5)
          } else if (slider == "melatonin_slider") {
            # Optimal: 0.5-5 mg for post-menopausal women
            ifelse(value >= 0.5 & value <= 5, 1, 0.5)
          } else {
            0.5 # Default for undefined
          }
        }
      ),
      "Exercise" = list(
        sliders = c("swim_slider", "run_slider", "walk_slider", "hike_slider", "bike_slider", "team_slider", "lifting_slider", "other_exerc_slider"),
        score_func = function(value) {
          if (value == "Intense") 1 else if (value == "Light") 0.5 else 0
        }
      ),
      "Socialization" = list(
        sliders = c("friends_slider", "strangers_slider", "vol_slider"),
        score_func = function(slider, value) {
          # Optimal: 5-15 hours for friends, 2-10 for strangers, 5-15 for volunteering
          if (slider == "friends_slider") {
            ifelse(value >= 5 & value <= 15, 1, 0.5)
          } else if (slider == "strangers_slider") {
            ifelse(value >= 2 & value <= 10, 1, 0.5)
          } else {
            ifelse(value >= 5 & value <= 15, 1, 0.5)
          }
        }
      ),
      "Wellbeing" = list(
        sliders = c("read_slider", "journal_slider", "mental_slider", "wellbeing_slider"),
        score_func = function(slider, value) {
          # Optimal ranges based on reasonable activity levels
          if (slider == "read_slider") {
            ifelse(value >= 2 & value <= 10, 1, 0.5)
          } else if (slider == "journal_slider") {
            ifelse(value >= 1 & value <= 3, 1, 0.5)
          } else {
            ifelse(value >= 2 & value <= 10, 1, 0.5)
          }
        }
      ),
      "Spirituality" = list(
        sliders = c("meditate_slider", "mindful_slider", "religious_slider", "pray_slider", "yoga_slider", "sacred_slider", "spirit_slider"),
        score_func = function(slider, value) {
          # Optimal: 1-5 hours for most activities
          ifelse(value >= 1 & value <= 5, 1, 0.5)
        }
      )
    )
    
    # Compute effect sizes (beta_i) for each factor
    max_effect <- 5 # Maximum effect size
    effect_sizes <- sapply(selected_factors, function(factor) {
      mapping <- slider_mappings[[factor]]
      if (is.null(mapping)) return(0) # Default if no sliders defined
      
      scores <- sapply(mapping$sliders, function(slider) {
        value <- input[[slider]]
        if (is.null(value)) return(0.5) # Default if slider not set
        if (slider %in% c("calorie_slider", "protein_slider", "fat_slider", "carb_slider", "other_nutr_slider",
                          "swim_slider", "run_slider", "walk_slider", "hike_slider", "bike_slider",
                          "team_slider", "lifting_slider", "other_exerc_slider")) {
          mapping$score_func(value)
        } else {
          mapping$score_func(slider, value)
        }
      })
      mean(scores) * max_effect
    })
    
    # Generate factorial design
    design <- gen.factorial(levels = rep(2, num_factors), nVars = num_factors, varNames = selected_factors)
    design <- as.data.frame(lapply(design, function(x) ifelse(x == 1, -1, 1))) # Code as -1 and 1
    
    # Calculate health scores
    baseline <- 45 # Adjusted lower baseline for post-menopausal women
    health_scores <- apply(design, 1, function(row) {
      main_effects <- sum(effect_sizes * row)
      baseline + main_effects + rnorm(1, 0, 2.5) # Add random noise
    })
    
    # Combine design with health scores
    data <- cbind(design, HealthScore = health_scores)
    
    # Perform ANOVA
    formula_str <- paste("HealthScore ~", paste(selected_factors, collapse = " * "))
    anova_result <- aov(as.formula(formula_str), data = data)
    
    list(
      design = data,
      anova = summary(anova_result)
    )
  })
  
  # Output results (example rendering)
  output$designTable <- renderTable({
    req(factorCalc())
    factorCalc()$design
  })
  
  output$anovaResult <- renderPrint({
    req(factorCalc())
    factorCalc()$anova
  })
}
shinyApp(ui,server)