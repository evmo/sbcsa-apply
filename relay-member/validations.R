# RELAY MEMBER - validations

# START HERE
v0 <- reactive(list(
  input$team_name != "[SELECT]", 
  rv$swim_age >= 14,
  rv$swim_age < 14 || rv$swim_age >= 18 || input$parent_present
))

f0 <- c(
  "Please select a team",
  "The swimmer must be at least 14 years old on the date of the swim.",
  "Parent or guardian must be present for the rest of the application."
)

# SWIMMER INFO
v1 <- reactive(list(
  input$s_name != "",
  grepl(".+@.+\\..+", input$s_email) || grepl(".+@.+\\..+", input$p_email),
  input$s_mailing != "" || input$p_mailing != "", 
  input$ec_name != "",
  grepl(".+@.+\\..+", input$ec_email) || grepl("\\d", input$ec_phone)
))

f1 <- c(
  "Please enter your name",
  "Please enter a valid email address",
  "Please enter a mailing address",
  "Please enter the name of an emergency contact",
  "Please enter a phone # and/or email address for the emergency contact"
)

# SWIM EXPERIENCE
v2 <- reactive(list(
  input$experience != "",
  input$current_lifetime != "[SELECT]"
))

f2 <- c("Please describe your ocean swimming experience",
        "Please indicate your lifetime member status")

# HEALTH / MEDICAL
v3 <- reactive(list(
  input$med1,
  input$med2
))

f3 <- c(
  "You must agree with statement #1",
  "You must agree with statement #2"
)

# LIABILITY WAIVER
v4 <- reactive(list(
  input[[paste0("initial", 1)]] != "",
  input[[paste0("initial", 2)]] != "",
  input[[paste0("initial", 3)]] != "",
  input[[paste0("initial", 4)]] != "",
  input[[paste0("initial", 5)]] != "",
  input[[paste0("initial", 6)]] != "",
  input[[paste0("initial", 7)]] != "",
  input[[paste0("initial", 8)]] != "",
  input[[paste0("initial", 9)]] != "",
  input[[paste0("initial", 10)]] != "",
  input[[paste0("initial", 11)]] != "",
  input$waiver_box,
  input$waiver_sig != ""
))

f4 <- c(
  paste0("Please initial item #", 1),
  paste0("Please initial item #", 2),
  paste0("Please initial item #", 3),
  paste0("Please initial item #", 4),
  paste0("Please initial item #", 5),
  paste0("Please initial item #", 6),
  paste0("Please initial item #", 7),
  paste0("Please initial item #", 8),
  paste0("Please initial item #", 9),
  paste0("Please initial item #", 10),
  paste0("Please initial item #", 11),
  "Please check the box",
  "Signature is required"
)