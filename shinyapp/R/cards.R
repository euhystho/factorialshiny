library(bslib)
intro_card <- card(
  h5("Healthy Habits Circle Simulated Experiment Design", style = "font-weight: bold;"),
  p("2³ factorial design with the following factors:"),
  
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