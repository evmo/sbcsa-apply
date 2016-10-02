library(shiny); library(shinyjs); library(knitr); library(lubridate)
countries <- readLines("countries.txt")
islands <- readLines("islands.txt")
boats <- readLines("boats.txt")
waiver <- readLines("waiver.txt")
jsCode <- "
$(function() {
	$('#sig').signature();
$('#clear').click(function() {
  $('#sig').signature('clear');
});
$('#json').click(function() {
  alert($('#sig').signature('toJSON'));
});
$('#svg').click(function() {
  alert($('#sig').signature('toSVG'));
});
});
"

server <- function(input, output, session) {

  rv <- reactiveValues(v = 0, crew_count = 0, swim_count = 0)

  observe({
    if (input$other_citizen == T) show("s_citizenship") 
    else hide("s_citizenship")
  })
  
  observe({
    if (input$route == "some other route") show("route_other")
    else hide("route_other")
  })
  
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
  
  observe({
    if (input$more_background == T) show("background_details") 
    else hide("background_details")
  })
  
  observe({
    if (input$current_lifetime == "No") show("new_lifetime") 
    else hide("new_lifetime")
  })

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
  
  validation <- function(page) {
    validate(
      need(input$s_name != "", "Name required")
    )
  }
  
  output$valid_page1 <- renderUI(validation(page1))
}

ui <- fluidPage(useShinyjs(),
  tags$head(
    includeScript("http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/themes/south-street/jquery-ui.css"),
    includeScript("jquery.signature.js"),
    includeCSS("jquery.signature.css"),
    includeCSS("custom.css"),
    includeCSS("http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/themes/south-street/jquery-ui.css"),
    tags$script(jsCode)
  ),
  
  titlePanel("SBCSA"),
  navlistPanel("Solo Application",
      
    # -------- THE SWIMMER -------------
    
    tabPanel("Swimmer Info",
      textInput("s_name", "Full Name"),
      selectInput("s_gender", "Gender", c("[SELECT]", "Female", "Male")),
      dateInput("s_dob", "Date of Birth", startview = "year",
                min = Sys.Date() - years(100), max = Sys.Date() - 365,
                value = Sys.Date() - years(40)),
      textInput("s_email", "Email Address"),
      fluidRow(
        column(6, textInput("s_phone", "Phone")),
        column(6, textInput("s_phone_alt", "Alt. Phone"))
      ),
      textAreaInput("s_mailing", "Mailing Address"),
      p("Are you a primary citizen of a different country than your residence?"),
      checkboxInput("other_citizen", "Yes"),
      hidden(
        selectInput("s_citizenship", "Citizenship", choices = countries)
      ),
      h3("Emergency Contact Person"),
      p("Please list an emergency contact person. 
        Note: This should be someone who is not on 
        the escort boat during the swim."),
      fluidRow(
        column(6, textInput("ec_name", "Name")),
        column(6, textInput("ec_email", "Email"))
      ),
      fluidRow(
        column(6, textInput("ec_phone", "Phone")),
        column(6, textInput("ec_alt_phone", "Alt. Phone"))
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
      selectInput("escort_boat", "Escort Boat", choices = boats),
      p("Note: You should have already made arrangements with 
        this boat before submitting this application."),
      p("When is your escort boat scheduled to depart the harbor?"),
      fluidRow(
        column(8, dateInput("harbor_date", "Date")),
        column(4, selectInput("harbor_time", "Hour", seq(0, 23)))
      ),
      p("When is your swim scheduled to begin? (i.e., swimmer in the water)"),
      fluidRow(
        column(8, dateInput("splash_date", "Date")),
        column(4, selectInput("splash_time", "Hour", seq(0, 23)))
      ),
      p("The SBCSA publishes a list of upcoming swim attempts at the 
        beginning of each season. Do we have your permission to 
        publicize your attempt, or do you prefer to keep it private?"),
      radioButtons("publicize", "Permission to Publicize?",
                   c("Yes", "No, please keep it private"))
    ),
  
    # -------- SUPPORT TEAM ---------------
    
    tabPanel("Support Team",
      h4("Crew Chief"),
      p("Who will be your lead support person on the boat?"),
      textInput("cc_name", "Name"),
      textInput("cc_email", "Email"),
      textInput("cc_phone", "Phone"),
      h4("Other Support Crew"),
      p("Please list all other crew members (excluding crew chief).
          Indicate each crew member's specific role (kayaker,
          feeder, support swimmer, etc.)"),
      actionButton("add_crew", "Add Crew Member")
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
      textAreaInput("feeding_plan", "What is your feeding plan? 
                    Product(s)? Frequency? From boat or kayak?"),
      textAreaInput("feed_experience", "What experience do you have 
                    using this feed plan on long swims?"),
      textInput("stroke_rate", "What is your typical stroke rate 
                for a swim of this distance (strokes per minute)?"),
      selectInput("breathing", "What is your breathing pattern?",
                  c("right only", "left only", "mostly right, but can
                    breathe bilateral", "mostly left, but can breathe 
                    bilateral", "bilateral")),
      textAreaInput("hypothermia", "What is your experience (if any) 
                    with hypothermia in the context of swimming."),
      textAreaInput("night_swiming", "What is your experience (if any) 
                    swimming at night")
    ),
    
    # ---------- HEALTH & MEDICAL CLEARANCE ----------
    
    tabPanel("Health & Medical Clearance",
      includeMarkdown("medical.md"),
      textAreaInput("health_disclosure", "Health Disclosure"),
      p("Please read the following two statements. 
        If they are true, please check the boxes and sign your name below."),
      p("1. To the best of my knowledge, I am in excellent general health 
        and have not omitted any information which might be 
        relevant to my ability to swim the Santa Barbara Channel"),
      checkboxInput("med1", "Yes, this is true"),
      p("I have been examined by a medical doctor within the past 12 months, 
        and have been specifically cleared to undertake this event."),
      checkboxInput("med2", "Yes, this is true"),
      dateInput("med_date", "Date of Medical Exam")
    ),
    
    # ------ WAIVER & RELEASE OF LIABILITY ---------
    
    tabPanel("Waiver & Release of Liability",
      h5(waiver[1]), hr(),
      p(waiver[2]), p(waiver[3]), textInput("initial1", "Initials", width = 50),
      p(waiver[4]), textInput("initial1", "Initials", width = 50),
      p(waiver[5]), textInput("initial2", "Initials", width = 50),
      p(waiver[6]), textInput("initial3", "Initials", width = 50),
      p(waiver[7]), textInput("initial4", "Initials", width = 50),
      p(waiver[8]), textInput("initial5", "Initials", width = 50),
      p(waiver[9]), textInput("initial6", "Initials", width = 50),
      p(waiver[10]), textInput("initial7", "Initials", width = 50),
      p(waiver[11]), textInput("initial8", "Initials", width = 50),
      p(waiver[12]), textInput("initial9", "Initials", width = 50),
      p(waiver[13]), textInput("initial10", "Initials", width = 50),
      p(waiver[14]), textInput("initial11", "Initials", width = 50),
      tags$div(HTML('
        <div id="sig"></div>
        <p style="clear: both;"><button id="clear">Clear</button> 
        <button id="json">To JSON</button> <button id="svg">To SVG</button></p>
      '))
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
      uiOutput("preview")
    )
  )
)

shinyApp(ui = ui, server = server)