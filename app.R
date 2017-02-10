library(shiny); library(shinyjs); library(knitr); library(lubridate)
countries <- readLines("countries.txt")
islands <- readLines("islands.txt")
boats <- readLines("boats.txt")
waiver <- readLines("waiver.txt")
reqd <- function(input) div(p(class = 'reqd', "* Required"), input)

server <- function(input, output, session) {

  rv <- reactiveValues(v = 0, crew_count = 0, swim_count = 0)

  # observers for displaying hidden inputs
  
  observe({
    if (input$other_citizen == T) 
      shinyjs::show("s_citizenship") 
    else 
      shinyjs::hide("s_citizenship")
  })
  
  observe({
    if (input$route == "some other route") 
      shinyjs::show("route_other")
    else 
      shinyjs::hide("route_other")
  })
  
  observe({
    if (input$boat == "BOAT NOT LISTED")
      shinyjs::show("boat_pilot_other")
    else
      shinyjs::hide("boat_pilot_other")
  })
  
  observe({
    if (input$boat == "TO BE DETERMINED")
      shinyjs::show("boat_notify")
    else
      shinyjs::hide("boat_notify")
  })
  
  observe({
    if (input$more_background == T) 
      shinyjs::show("background_details") 
    else 
      shinyjs::hide("background_details")
  })
  
  observe({
    if (input$current_lifetime == "No") 
      shinyjs::show("new_lifetime") 
    else 
      shinyjs::hide("new_lifetime")
  })
  
  observeEvent(input$harbor_date, {
    updateDateInput(session, "splash_date", value = input$harbor_date)
  })
  
  observeEvent(input$gen_markdown, shinyjs::show("preview_and_sig"))
  
  # insertUIs for crew members & documented swims
  
  observeEvent(input$add_crew, {
    insertUI(selector = "#add_crew", "afterEnd", ui = tagList(div(
      column(4, textInput(paste0("crew_name", input$add_crew), "Name")),
      column(4, textInput(paste0("crew_role", input$add_crew), "Role")),
      column(4, textInput(paste0("crew_contact", input$add_crew), "Contact Phone or Email"))
    )))
    rv$crew_count <- rv$crew_count + 1
  })
  
  observeEvent(input$add_swim, {
    insertUI(selector = "#add_swim", "afterEnd", ui = tagList(div(
      column(2, textInput(paste0("swim_year", input$add_swim), "Year")),
      column(4, textInput(paste0("swim_name", input$add_swim), "Swim")),
      column(2, textInput(paste0("swim_dist", input$add_swim), "Distance")),
      column(2, textInput(paste0("swim_dur", input$add_swim), "Duration")),
      column(2, textInput(paste0("swim_temp", input$add_swim), "Temp"))
    )))
    rv$swim_count <- rv$swim_count + 1
  })
  
  # Page Validations ---------------------------------
  
  validation1 <- function(page) {
    validate(
      need(input$s_name != "", "Name required"),
      need(input$s_dob < Sys.Date() - 18 * 365, "Swimmer must be at least 14 years old"),
      need(grepl(".+@.+\\..+", input$s_email), "Valid email address required"),
      need(input$ec_name != "", "Emergency contact required")
    )
  }
  
  output$valid_page1 <- renderUI(validation1(page1))
  
  validation3 <- function(page) {
    validate(
      need(input$cc_name != "", "Crew chief required")
    )
  }
  
  output$valid_page3 <- renderUI(validation3(page3))
  
  validation4 <- function(page) {
    validate(
      need(input$feed_plan != "", "Feed plan required"),
      need(input$feed_experience != "", "Feed experience required"),
      need(input$stroke_rate != "", "Stroke rate required"),
      need(input$breathing != "", "Breathing pattern required"),
      need(input$hypothermia != "", "Hypothermia experience required"),
      need(input$night_swimming != "", "Night swimming experience required")
    )
  }
  
  output$valid_page4 <- renderUI(validation4(page4))
  
  validation5 <- function(page) {
    validate(
      need(input$med1, "You must agree with statement #1"),
      need(input$med2, "You must agree with statement #2")
    )
  }
  
  output$valid_page5 <- renderUI(validation5(page5))

  outFile <- reactiveFileReader(1000, session, "output.md", readLines)
  
  output$preview <- renderUI({
    rv$v
    includeMarkdown("output.md")
  })

  observeEvent(input$gen_markdown, {
    out <- knit_expand("template.md")
    writeLines(out, "output.md")
    rv$v <- rv$v + 1
  })
  
  # observeEvent(input$sig_submit, {
  #   js$saveSig
  # })
}

ui <- fluidPage(useShinyjs(),
  tags$head(
    includeScript("http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js"),
    includeScript("jquery.signature.js"),
    includeScript("sig-funcs.js"),
    includeCSS("jquery.signature.css"),
    includeCSS("custom.css"),
    includeCSS("http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/themes/south-street/jquery-ui.css")
  ),
  
  titlePanel("SBCSA"),
  navlistPanel("Solo Application",
      
    # -------- THE SWIMMER -------------
    
    tabPanel("Swimmer Info",
      reqd(textInput("s_name", "Full Name")),
      reqd(dateInput("s_dob", "Date of Birth", startview = "year",
                min = Sys.Date() - years(100), max = Sys.Date() - 365,
                value = Sys.Date() - years(40))),
      reqd(textInput("s_email", "Email Address")),
      textInput("s_phone", "Mobile Phone"),
      textAreaInput("s_mailing", "Mailing Address", width = 400, height = 125),
      p("Are you a primary citizen of a different country than your residence?"),
      checkboxInput("other_citizen", "Yes"),
      hidden(
        selectInput("s_citizenship", "Citizenship", choices = countries)
      ),
      h3("Emergency Contact Person"),
      p("Please list an emergency contact person who will 
          be on land during the swim."),
      fluidRow(
        reqd(column(6, textInput("ec_name", "Name"))),
        column(6, textInput("ec_rel", "Relationship to swimmer"))
      ),
      fluidRow(
        column(6, textInput("ec_phone", "Phone")),
        column(6, textInput("ec_email", "Email"))
      ),
      uiOutput("valid_page1")
    ),
    
    # ----------- THE SWIM --------------
    
    tabPanel("The Swim",
      radioButtons("route", "What route will you be swimming?", 
        choices = c("Anacapa to mainland", "some other route")
      ),
      hidden(
        textInput("route_other", "Please describe your proposed route")
      ),
      selectInput("boat", "Escort Boat", choices = boats),
      hidden(div(id = "boat_pilot_other",
        textInput("boat_other", "Name of Boat"),
        textInput("pilot_other", "Name of Pilot")
      )),
      hidden(div(id = "boat_notify", 
        p("Please notify the SBCSA as soon as you have
            made arrangements with an escort boat.")
      )),
      hr(),
      
      h4("When is your swim scheduled?"),
      fluidRow(
        column(8, dateInput("harbor_date", "Boat Departs Harbor")),
        column(4, selectInput("harbor_time", "Hour", seq(0, 23)))
      ),
      fluidRow(
        column(8, dateInput("splash_date", "Swimmer in the Water")),
        column(4, selectInput("splash_time", "Hour", seq(0, 23)))
      ),
      
      h4("Permission to publicize?"),
      p("The SBCSA publishes a list of upcoming swim attempts at the 
        beginning of each season. Do we have your permission to 
        publicize your attempt, or do you prefer to keep it private?"),
      radioButtons("publicize", NULL, c("Yes", "No, please keep it private"))
    ),
  
    # -------- SUPPORT TEAM ---------------
    
    tabPanel("Support Team",
      h4("Crew Chief"),
      p("Who will be your lead support person on the boat?"),
      reqd(textInput("cc_name", "Name")),
      textInput("cc_email", "Email"),
      textInput("cc_phone", "Phone"),
      h4("Other Support Crew"),
      p("Please list all other crew members (besides crew chief).
          Indicate each crew member's specific role (kayaker,
          feeder, support swimmer, etc.)"),
      actionButton("add_crew", "Add Crew Member"),
      uiOutput("valid_page3")
    ),
    
    # -------- MARATHON SWIMMING BACKGROUND --------
    
    tabPanel("Marathon Swimming Background",
      p("Please list up to five documented marathon swims you have completed, 
        especially swims from the past 1-3 years."),
      p("These should be observed, sanctioned solo swims such as the 
        English Channel or Catalina Channel, or organized races 
        such as the Semana Nautica 6-Mile."),
      actionButton("add_swim", "Add Documented Swim"),
      
p("You may wish to provide additional information about your 
        swimming background and training -- especially if your documented 
        marathon swimming history is somewhat sparse."),
      p("Provide more details on swimming background?"),
      checkboxInput("more_background", "Yes"),
      hidden(
        p("Use the box below to tell us more about your 
          swimming background and training. Details might include:"),
        tags$ul(
          tags$li("How far you swim each week, on average."),
          tags$li("Major training swims recently completed."),
          tags$li("Proportion of training you do in open water vs. the pool."),
          tags$li("Your local body of open water - typical water temps, 
                  how long you swim at these temps, etc."),
          tags$li("Your sustainable swimming pace (minutes/seconds per 100m 
                  for pool swimming, or minutes per statute mile for open water)")
        ),
        textAreaInput("background_details", "Details")
      ),
      
      reqd(textAreaInput("feed_plan", "What is your feeding plan? 
                    Product(s)? Frequency? From boat or kayak?",
                    width = 400, height = 125)),
      reqd(textAreaInput("feed_experience", "What experience do you have 
                    using this feed plan on long swims?",
                    width = 400, height = 125)),
      reqd(textInput("stroke_rate", "What is your typical stroke rate 
                for a swim of this distance (strokes per minute)?")),
      reqd(selectInput("breathing", "What is your breathing pattern?",
                  c("right only", "left only", "mostly right, but can
                    breathe bilateral", "mostly left, but can breathe 
                    bilateral", "bilateral"))),
      reqd(textAreaInput("hypothermia", "What is your experience (if any) 
                    with hypothermia in the context of swimming.",
                    width = 400, height = 100)),
      reqd(textAreaInput("night_swimming", "What is your experience (if any) 
                    swimming at night",
                    width = 400, height = 100)),
      uiOutput("valid_page4")
    ),
    
    # ---------- HEALTH & MEDICAL CLEARANCE ----------
    
    tabPanel("Health & Medical Clearance",
      includeMarkdown("medical.md"),
      textAreaInput("health_disclosure", "Health Disclosure",
                    width = 400, height = 125),
      
      p("Please read the following two statements. 
         If they are true, please check the boxes."),
      
      p("1. To the best of my knowledge, I am in excellent general health 
        and have not omitted any information which might be 
        relevant to my ability to swim the Santa Barbara Channel"),
      reqd(checkboxInput("med1", "Yes, this is true")),
      
      p("2. I have been examined by a medical doctor within the past 12 months, 
        and have been specifically cleared to undertake this event."),
      reqd(checkboxInput("med2", "Yes, this is true")),
      
      reqd(dateInput("med_date", "Date of Medical Exam")),
      uiOutput("valid_page5")
    ),
    
    # ------ WAIVER & RELEASE OF LIABILITY ---------
    
    tabPanel("Waiver & Release of Liability",
      h5(waiver[1]), hr(),
      p(waiver[2]), p(waiver[3]), reqd(textInput("initial1", "Initials", width = 50)),
      p(waiver[4]), reqd(textInput("initial1", "Initials", width = 50)),
      p(waiver[5]), reqd(textInput("initial2", "Initials", width = 50)),
      p(waiver[6]), reqd(textInput("initial3", "Initials", width = 50)),
      p(waiver[7]), reqd(textInput("initial4", "Initials", width = 50)),
      p(waiver[8]), reqd(textInput("initial5", "Initials", width = 50)),
      p(waiver[9]), reqd(textInput("initial6", "Initials", width = 50)),
      p(waiver[10]), reqd(textInput("initial7", "Initials", width = 50)),
      p(waiver[11]), reqd(textInput("initial8", "Initials", width = 50)),
      p(waiver[12]), reqd(textInput("initial9", "Initials", width = 50)),
      p(waiver[13]), reqd(textInput("initial10", "Initials", width = 50)),
      p(waiver[14]), reqd(textInput("initial11", "Initials", width = 50))
    ),
    
    # --------- SANCTION FEES ---------
    
    tabPanel("Sanction Fees",
      includeMarkdown("sanction_fees.md"),
      
      radioButtons("payment_choice", "Which method of payment do you plan to use?",
                   c("Dwolla", "PayPal", "personal check", "wire transfer",
                     "not sure yet")),
      
      p("We will send specific instructions along with the invoice."),
      h3("Lifetime Membership"),
      
      radioButtons("current_lifetime", "Are you currently a SBCSA Lifetime Member?",
                   c("Yes", "No")),
      
      hidden(div(id = "new_lifetime",
        p("SBCSA Lifetime Members are entitled to a $100 discount 
          on Solo sanction fees, for life, in addition to other 
          benefits (described here)."),
        p("Lifetime Membership costs $250 (one time only), 
          and is tax-deductible for U.S. taxpayers. 
          The discount is valid immediately, including for this swim."),
        radioButtons("lifetime_purchase", "Are you interesting in purchasing 
                     a Lifetime Membership at this time?", c("Yes", "No"))
      )),
      
      h3("Cancellation Policy"),
      includeMarkdown("cancel_policy.md"),
      p("I understand the cancellation policy"),
      
      checkboxInput("cancel_policy", "Yes")
    ),
    
    # --------- PREVIEW PAGE -----------
    
    tabPanel("Preview Application Submission",
      actionButton("gen_markdown", "Generate Preview"),
      hidden(div(id = "preview_and_sig",
        uiOutput("preview"),
        div(id = "sig"),
        tags$div(HTML('
          <p style="clear: both;"><button id="clear">Clear</button> 
          <button id="sig_submit">Submit</button></p>
          '))
      )),
      hidden(div(id = "sig_json"))
    )
  )
)

shinyApp(ui = ui, server = server)