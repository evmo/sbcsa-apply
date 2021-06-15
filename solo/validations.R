# SOLO - validations

v0 <- reactive(list(
  rv$swim_age >= 14,
  rv$swim_age < 14 || rv$swim_age >= 18 || input$parent_present
))

f0 <- c(
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
  !(input$start == "mainland" & input$finish == "Catalina Island"),
  !(input$start == "Catalina Island" & input$finish == "circumnavigation"),
  !(input$start == "mainland" & input$finish == "circumnavigation"),
  input$splash_date >= input$harbor_date
))

f2 <- c(
  "Please select an escort boat",
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
  nchar(input$background_details) > 0,
  input$feed_plan != "",
  input$feed_experience != "",
  input$stroke_rate != "",
  input$hypothermia != "",
  input$night_swimming != ""
))

f4 <- c(
	"Please enter LongSwimsDB URL or other details on your swim background.",
  "Please enter a feed plan",
  "Please enter experience with feed plan",
  "Please enter stroke rate",
  "Please enter hypothermia experience",
  "Please enter night swimming experience"
)

# HEALTH / MEDICAL
v5 <- reactive(list(
  input$med1,
  input$med2,
  input$vax
))

f5 <- c(
  "You must agree with statement #1",
  "You must agree with statement #2",
  "You must indicate your understanding of the vaccination requirement"
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
