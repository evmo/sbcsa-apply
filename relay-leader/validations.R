v1 <- reactive(list(
  input$s_name != "", 
  grepl(".+@.+\\..+", input$s_email),
  input$s_mailing != ""
))

f1 <- c(
  "Please enter your name",
  "Please enter a valid email address",
  "Please enter a mailing address"
)

v2 <- reactive(list(
  input$team_name != "",
  input$team_size != "[SELECT]",
  input$leg_duration > 30,
  input$cc_name != ""
))

f2 <- c(
  "Please enter a team name",
  "Please select a team size (minimum 2, maximum 6)",
  "Please enter a valid leg duration (minimum 30 minutes)",
  "Please enter a crew chief"
)

v3 <- reactive(list(
  input$boat_known != "[SELECT]",
  !(input$start == "Catalina Island" & input$finish == "mainland"),
  !(input$start == "mainland" & input$finish == "Catalina Island"),
  !(input$start == "Catalina Island" & input$finish == "circumnavigation"),
  !(input$start == "mainland" & input$finish == "circumnavigation"),
  input$splash_date >= input$harbor_date
))

f3 <- c(
  "Please select an escort boat",
  "Route is not sanctioned by the SBCSA",
  "Route is not sanctioned by the SBCSA",
  "Route is not sanctioned by the SBCSA",
  "LOL",
  "Splash date must be after harbor departure"
)
