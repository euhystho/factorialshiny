library(shiny)
library(bslib)
library(daewr)
library(DoE.base)
library(ggplot2)
library(car)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("2^3 Factorial Design Demonstration"),
  theme = bs_theme(
    bootswatch = "journal"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Dataset:",
                  choices = c("Voltmeter Experiment", "Custom Data")),
      conditionalPanel(
        condition = "input.dataset == 'Custom Data'",
        fileInput("datafile", "Upload your CSV file",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      selectInput("tab", "Select View:",
                  choices = c("Design", "Assumptions", "ANOVA", "Effects", "Interactions")),
      conditionalPanel(
        condition = "input.tab == 'Interactions'",
        selectInput("interaction", "Select Interaction:",
                    choices = c("A×B", "A×C", "B×C"))
      ),
      br(),
      h4("Voltmeter Experiment Design", style = "font-weight: bold;"),
      p("2³ factorial design with the following factors:"),
      
      tags$div(
        style = "margin-left: 15px;",
        HTML("<b>Factor A:</b> Ambient Temperature"),
        tags$ul(
          tags$li("Low: 22°C"),
          tags$li("High: 32°C")
        ),
        
        HTML("<b>Factor B:</b> Voltmeter Warm-up Time"),
        tags$ul(
          tags$li("Low: 0.5 min"),
          tags$li("High: 5 min")
        ),
        
        HTML("<b>Factor C:</b> Circuit Warm-up Time"),
        tags$ul(
          tags$li("Low: 0.5 min"),
          tags$li("High: 5 min")
        )
      ),
      
      p(HTML("<b>Response:</b> Measured voltage in millivolts")),
      p("The design has 2 replicates for each treatment combination."),
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.tab == 'Design'",
        h3("Experimental Design"),
        DTOutput("design_table"),
      ),
      conditionalPanel(
        condition = "input.tab == 'ANOVA'",
        h3("ANOVA Results"),
        verbatimTextOutput("anova_output")
      ),
      conditionalPanel(
        condition = "input.tab == 'Effects'",
        h3("Effects Analysis"),
        plotOutput("effects_plot"),
        verbatimTextOutput("effects_output")
      ),
      conditionalPanel(
        condition = "input.tab == 'Interactions'",
        h3("Interaction Plots"),
        plotOutput("interaction_plot")
      ),
      conditionalPanel(
        condition = "input.tab == 'Assumptions'",
        h3("Model Assumptions"),
        tabsetPanel(
          tabPanel("Residuals vs Fitted", plotOutput("resid_fitted")),
          tabPanel("Normal Q-Q", plotOutput("qq_plot")),
          tabPanel("Outlier Check", verbatimTextOutput("outlier_check"))
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Data reactive
  getData <- reactive({
    if (input$dataset == "Voltmeter Experiment") {
      # Load the voltmeter experiment data from daewr package
      data(volt)
      return(volt)
    } else if (input$dataset == "Custom Data" && !is.null(input$datafile)) {
      # Load custom data
      read.csv(input$datafile$datapath)
    } else {
      # Default to volt data if no file uploaded
      data(volt)
      return(volt)
    }
  })
  
  # Fit model reactive
  getModel <- reactive({
    df <- getData()
    # Create model using contr.FrF2 for proper coding
    model <- lm(y ~ A*B*C, data = df, 
                contrasts = list(A = contr.FrF2, B = contr.FrF2, C = contr.FrF2))
    return(model)
  })
  
  # Output: Experimental Design Table
  output$design_table <- renderDT({
    df <- getData()
    datatable(df, options = list(pageLength = 16))
  })
  
  
  # Output: ANOVA Results
  output$anova_output <- renderPrint({
    model <- getModel()
    
    # Print summary and ANOVA
    cat("Model Summary:\n")
    print(summary(model))
    
    cat("\n\nANOVA Table:\n")
    print(anova(model))
    
    # Add Type II ANOVA
    cat("\n\nType II ANOVA:\n")
    print(Anova(model, type = 2))
    
    # Add Type III ANOVA
    drop1(model, .~., test="F")
    cat("\n\nType III ANOVA:\n")
    print(Anova(model, type = 3))
  })
  
  # Output: Effects Analysis
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
      text(plot_data$x[labeled], plot_data$y[labeled], 
           labels = parnames[labeled], pos = 2)
    }
  })
  
  # Output: Effects Analysis Text
  output$effects_output <- renderPrint({
    model <- getModel()
    
    # Extract coefficients (half effects)
    coeffs <- coef(model)[-1]  # Remove intercept
    
    # Calculate full effects (2 × coefficients)
    effects <- 2 * coeffs
    
    # Create a data frame of effects
    effects_df <- data.frame(
      Effect = names(effects),
      Estimate = effects,
      `Half_Effect` = coeffs,
      `Std_Error` = summary(model)$coefficients[-1, "Std. Error"],
      `t_value` = summary(model)$coefficients[-1, "t value"],
      `Pr(>|t|)` = summary(model)$coefficients[-1, "Pr(>|t|)"]
    )
    
    # Print effects table
    cat("Effects Table:\n")
    print(effects_df)
    
    # Identify significant effects
    cat("\nSignificant Effects (p < 0.05):\n")
    sig_effects <- effects_df[effects_df$`Pr(>|t|)` < 0.05, ]
    if (nrow(sig_effects) > 0) {
      print(sig_effects)
    } else {
      cat("No significant effects found at alpha = 0.05\n")
    }
  })
  
  # Output: Interaction Plot
  output$interaction_plot <- renderPlot({
    df <- getData()
    
    if (input$interaction == "A×B") {
      interaction.plot(df$A, df$B, df$y, type = "b", 
                       xlab = if (input$dataset == "Voltmeter Experiment") 
                         "Ambient Temperature (˚C)" else "Factor A", 
                       ylab = if (input$dataset == "Voltmeter Experiment") 
                         "Voltage (mV)" else "Response",
                       trace.label = if (input$dataset == "Voltmeter Experiment") 
                         "Warm-up minutes" else "Factor B", 
                       main = if (input$dataset == "Voltmeter Experiment") 
                             "Interaction Plot of Temperature x Voltmeter Warm-up Time" 
                              else "Interaction Plot of A × B",
                       pch = c(19, 17), lty = c(1, 2), 
                       col = c("blue", "red"), lwd = 2)
    } else if (input$interaction == "A×C") {
      interaction.plot(df$A, df$C, df$y, type = "b", 
                       xlab = if (input$dataset == "Voltmeter Experiment") 
                         "Ambient Temperature (˚C)" else "Factor A",  
                       ylab = if (input$dataset == "Voltmeter Experiment") 
                         "Voltage (mV)" else "Response",
                       trace.label = if (input$dataset == "Voltmeter Experiment") 
                         "Warm-up minutes" else "Factor C", 
                       main = if (input$dataset == "Voltmeter Experiment") 
                         "Interaction Plot of Temperature x Circuit Warm-up Time" 
                       else "Interaction Plot of A × C",
                       pch = c(19, 17), lty = c(1, 2), 
                       col = c("blue", "red"), lwd = 2)
    } else if (input$interaction == "B×C") {
      interaction.plot(df$B, df$C, df$y, type = "b", 
                       xlab = if (input$dataset == "Voltmeter Experiment") 
                         "Voltmeter Warm-up" else "Factor B", 
                       ylab = if (input$dataset == "Voltmeter Experiment") 
                         "Voltage (mV)" else "Response",
                       trace.label = if (input$dataset == "Voltmeter Experiment") 
                         "Circuit Warm-up" else "Factor C",  
                       main = if (input$dataset == "Voltmeter Experiment") 
                         "Interaction Plot of Voltmeter Warm-up Time x Circuit Warm-up Time" 
                       else "Interaction Plot of B × C",
                       pch = c(19, 17), lty = c(1, 2), 
                       col = c("blue", "red"), lwd = 2)
    }
  })
  
  # Output: Model Assumption Checks
  
  # Residuals vs Fitted
  output$resid_fitted <- renderPlot({
    model <- getModel()
    plot(fitted(model), residuals(model), 
         xlab = "Fitted Values", ylab = "Residuals",
         main = "Residuals vs Fitted", pch = 19, col = "blue")
    abline(h = 0, lty = 2, col = "red")
  })
  
  # Normal Q-Q Plot
  output$qq_plot <- renderPlot({
    model <- getModel()
    qqnorm(residuals(model), main = "Normal Q-Q Plot", pch = 19, col = "blue")
    qqline(residuals(model), col = "red", lwd = 2)
  })
  # Outlier Check
  output$outlier_check <- renderPrint({
    model <- getModel()
    df <- getData()
    
    # Try to use Gaptest from daewr if data structure is appropriate
    tryCatch({
      if (input$dataset == "Voltmeter Experiment") {
        cat("Gaptest Analysis for Outlier Detection:\n")
        gap_result <- Gaptest(df)
        
        if (is.null(gap_result)){
          cat("There are no outliers in this dataset!")
        } else {
          print(gap_result)
        }
      } else {
        cat("Outlier check only available for built-in datasets with appropriate structure.\n")
      }
    }, error = function(e) {
      cat("Error in outlier detection: ", e$message, "\n")
      cat("Cook's Distances for potential outliers:\n")
      cooks_d <- cooks.distance(model)
      print(sort(cooks_d[cooks_d > 4/length(cooks_d)], decreasing = TRUE))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)