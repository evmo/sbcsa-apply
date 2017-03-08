v0 <- reactive(list(
  rv$swim_age >= 14,
  rv$swim_age < 14 || rv$swim_age >= 18 || input$parent_present
))

f0 <- c(
  "Please select a team",
  "The swimmer must be at least 14 years old on the date of the swim.",
  "Parent or guardian must be present for the rest of the application."
)

v1 <- reactive(list(
  input$s_name != "", 
  grepl(".+@.+\\..+", input$s_email) || grepl(".+@.+\\..+", input$p_email),
  input$s_mailing != "" || input$p_mailing != "", 
  input$s_country != "[SELECT]",
  input$other_citizen == F | input$s_citizenship != "[SELECT]",
  input$ec_name != "",
  grepl(".+@.+\\..+", input$ec_email) || grepl("\\d", input$ec_phone)
))

f1 <- c(
	"Please enter your name",
  "Please enter a valid email address",
  "Please enter a mailing address",
  "Please select a country",
  "Please select a country of citizenship",
  "Please list an emergency contact",
  "Please enter a phone # and/or email address for the emergency contact"
)

v2 <- reactive(list(
  input$boat_known != "[SELECT]",
  !(input$start == "Catalina Island" & input$finish == "mainland"),
  !(input$start == "Catalina Island" & input$finish == "circumnavigation"),
  !(input$start == "mainland" & input$finish == "circumnavigation"),
  input$splash_date >= input$harbor_date
))

f2 <- c(
	"Route is not sanctioned by the SBCSA",
  "Route is not sanctioned by the SBCSA",
  "Route is not sanctioned by the SBCSA",
  "LOL",
  "Splash date must be after harbor departure"
)

v3 <- reactive(list(
  input$cc_name != ""
))

f3 <- c(
	"Please enter a crew chief"
)

v4 <- reactive(list(
  nchar(input$background_details) > 0 |
        (!is.null(input$swim_name1) && 
            input$swim_name1 != "" &&
            input$swim_dist1 != ""),
  input$feed_plan != "",
  input$feed_experience != "",
  input$stroke_rate != "",
  input$breathing != "[SELECT]",
  input$hypothermia != "",
  input$night_swimming != ""
))

f4 <- c(
	"Please enter at least one documented swim, or provide additional details.",
  "Please enter a feed plan",
  "Please enter experience with feed plan",
  "Please enter stroke rate",
  "Please select a breathing pattern",
  "Please enter hypothermia experience",
  "Please enter night swimming experience"
)

# HEALTH / MEDICAL
v5 <- reactive(list(
  input$med1,
  input$med2
))

f5 <- c(
  "You must agree with statement #1",
  "You must agree with statement #2"
)

# LIABILITY WAIVER
v6 <- reactive(list(
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

f6 <- c(
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

# SANCTION FEES
v7 <- reactive(list(
  input$payment_choice != "[SELECT]",
  input$current_lifetime != "[SELECT]",
  input$cancel_policy
))

f7 <- c(
	"Please select a payment method",
  "Please indicate your lifetime member status",
  "Please check the box that you understand the cancellation policy"
)