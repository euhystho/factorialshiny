library(bslib)
library(bsicons)
library(plotly)

# Global Variables, adjust if needed:
min_plot_height <- 625
min_table_height <- 10

intro_card <- card(
  h5("Healthy Habits Circle Simulated Experiment Design", style = "font-weight: bold;"), # nolint
  p("2³ factorial design with the following factors:"),
  # nolint
  tags$div(
    style = "margin-left: 15px;",
    HTML("<b>Nutrition:</b>"),
    tags$ul(
      tags$li(HTML("Purposeful: Tracking calories or practicing healthy eating habits")),
      tags$li("Mindless: Not planning or practicing healthy eating habits")
    ),
    HTML("<b>Sleep:</b>"),
    tags$ul(
      tags$li("Sufficient:  7 hours of sleep"),
      tags$li("Insufficient: < 7 hours of sleep")
    ),
    HTML("<b>Exercise:</b> <i>Exercise Intensity</i>"),
    tags$ul(
      tags$li("Intense: Engaging in more-strenuous activities"),
      tags$li("Light: Engaging in non-strenuous actvities or no exercise")
    )
  ),
  p(HTML("<b>Response:</b> <i>Health Score</i>"))
)

plot_card <- list(
  card(
    card_header("Interaction Plot"),
    min_height = min_plot_height,
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
        )
      ),
      plotOutput("interaction_plot")
    )
  ),
  card(
    card_header("Factorial Design Plot through Plotly"),
    min_height = min_plot_height,
    plotlyOutput("factorial")
  ),
  card(
    card_header("Assumptions"),
    min_height = min_plot_height,
    plotlyOutput("assumptions")
  ),
  card(
    card_header("Effects"),
    min_height = min_plot_height,
    plotOutput("effects_plot")
  ),
  card(
    card_header("Residual vs Fitted Graph"),
    min_height = min_plot_height,
    plotOutput("residual")
  ),
  card(
    card_header("Normal QQ Plot"),
    min_height = min_plot_height,
    plotOutput("QQ")
  )
)

table_card <- list(
  card(
    card_header("ANOVA Table"),
    min_height = min_table_height,
    uiOutput("anova")
  ),
  card(
    card_header("Factorial Design Table"),
    min_height = min_table_height,
    uiOutput("design")
  ),
  card(
    card_header("Effects Table"),
    min_height = min_table_height,
    uiOutput("effects_output")
  )
)

text_card <- list(
  card(
    verbatimTextOutput("independence")
  ),
  card(
    verbatimTextOutput("outlier")
  )
)
