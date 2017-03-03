library(shiny)
library(shinyjs)
library(knitr)
library(lubridate)
library(rmarkdown)
library(magrittr)
library(markdown)
library(gmailr)

source("fill_sample.R")
countries <- readLines("../_includes/countries.txt")
islands <- readLines("../_includes/islands.txt")
boats <- readLines("../_includes/boats.txt")
waiver <- readLines("../_includes/waiver.txt")
reqd <- function(input) div(class = 'required', input)
dev_mode <- function() if (grepl("dev", getwd())) 1 else 0
DIR <- if (dev_mode()) "~/dev/sbcsa-apply" else "~/sbcsa-apply"
DATADIR <- file.path(DIR, "data")
emails <- readLines("../conf/emails.txt")

server <- function(input, output, session) {

  rv <- reactiveValues(v = 0)

  shinyjs::addClass(class = "disabled-link", 
                    selector = ".nav li:nth-child(10) a,
                                .nav li:nth-child(11) a")
  shinyjs::addClass(class = "red", selector = ".nav li:nth-child(12) a")
  
  observe({  # observers for displaying hidden inputs

    if (dev_mode()) shinyjs::show("fill_sample")

    if (input$other_citizen == T) 
      shinyjs::show("s_citizenship") 
    else 
      shinyjs::hide("s_citizenship")

    if (input$route == "some other route") 
      shinyjs::show("route_other")
    else 
      shinyjs::hide("route_other")

    if (input$boat == "BOAT NOT LISTED")
      shinyjs::show("boat_pilot_other")
    else
      shinyjs::hide("boat_pilot_other")

    if (input$boat == "TO BE DETERMINED")
      shinyjs::show("boat_notify")
    else
      shinyjs::hide("boat_notify")

    if (rv$crew_count > 0)
      shinyjs::show("remove_crew")
    else
      shinyjs::hide("remove_crew")

    if (rv$swim_count > 0)
      shinyjs::show("remove_swim")
    else
      shinyjs::hide("remove_swim")

    if (rv$swim_count >= 5)
      shinyjs::hide("add_swim")
    else
      shinyjs::show("add_swim")

    if (input$more_background == T) 
      shinyjs::show("more_details") 
    else 
      shinyjs::hide("more_details")

    if (input$current_lifetime == "No") 
      shinyjs::show("new_lifetime") 
    else 
      shinyjs::hide("new_lifetime")
  })
  
  observeEvent(input$fill_sample, {
    if (dev_mode())
      fill_sample(session)
  })
  
  # when harbor departure entered, update splash date to match
  observeEvent(input$harbor_date, {
    updateDateInput(session, "splash_date", value = input$harbor_date)
  })
  
  # insertUI for crew members
  rv$crew_count <- 0
  observeEvent(input$add_crew, {
    rv$crew_count <<- rv$crew_count + 1
    crewid <- paste0('crew', rv$crew_count)
    insertUI(selector = "#crew", "beforeEnd", ui = tagList(div(id = crewid,
      column(4, textInput(paste0("crew_name", rv$crew_count), "Name")),
      column(4, textInput(paste0("crew_role", rv$crew_count), "Role")),
      column(4, textInput(paste0("crew_contact", rv$crew_count), "Contact Phone or Email"))
    )))
  })
  
  observeEvent(input$remove_crew, {
    removeUI(selector = paste0('#crew', rv$crew_count))
    if (rv$crew_count > 0) rv$crew_count <<- rv$crew_count - 1
  })
  
  # insertUI for documented swims
  rv$swim_count <- 0
  observeEvent(input$add_swim, {
    rv$swim_count <<- rv$swim_count + 1
    swimid <- paste0('swim', rv$swim_count)
    insertUI(selector = "#doc_swims", "beforeEnd", ui = tagList(div(id = swimid,
      textInput(paste0("swim_name", input$add_swim), 
                label = paste("Swim", rv$swim_count, "Description"), 
                width = "100%"),
      fluidRow(
        column(2, selectInput(paste0("swim_year", input$add_swim), 
                              "Year",
                              choices = seq(2017, 1997))),
        column(2, textInput(paste0("swim_dist", input$add_swim), "Distance")),
        column(2, selectInput(paste0("swim_units", input$add_swim), 
                              "Units",
                              choices = c("miles", "km"))),
        column(2, textInput(paste0("swim_hr", input$add_swim), "Hr")),
        column(2, textInput(paste0("swim_min", input$add_swim), "Min")),
        column(2, textInput(paste0("swim_temp", input$add_swim), "Temp"))
      )
    )))
  })
  
  observeEvent(input$remove_swim, {
    n <- rv$swim_count
    removeUI(selector = paste0("#swim", n))
    if (rv$swim_count > 0) rv$swim_count <<- rv$swim_count - 1
  })
  
  output$waiver_agree <- renderUI({
    statement <- paste0("I, ", input$s_name, tolower(
        ", HAVE READ THIS WAIVER AND RELEASE OF LIABILITY, 
         FULLY UNDERSTAND ITS TERMS, UNDERSTAND THAT I HAVE GIVEN UP 
         SUBSTANTIAL RIGHTS BY SIGNING IT, AND HAVE SIGNED IT FREELY 
         AND VOLUNTARILY WITHOUT ANY INDUCEMENT. "),
      "I understand that typing my name represents a legal signature.")
    checkboxInput("waiver_box", statement, width = "100%")
  })
  
  # Page Validations ---------------------------------
  
  validation1 <- function() {
    validate(
      need(input$s_name != "", 
           "Please enter your name"),
      need(input$s_dob < Sys.Date() - 18 * 365, 
           "Swimmer must be at least 14 years old"),
      need(grepl(".+@.+\\..+", input$s_email), 
           "Please enter a valid email address"),
      need(input$s_mailing != "", "Please enter a mailing address"),
      need(input$s_country != "[SELECT]", "Please select a country"),
      need(input$other_citizen == F | input$s_citizenship != "[SELECT]",
            "Please select a country of citizenship"),
      need(input$ec_name != "", "Please list an emergency contact")
    )
  }
  
  output$valid_page1 <- renderUI(validation1())
  
  validation2 <- function() {
    validate(
      need(input$boat != "[SELECT]",
           "Please select a boat"),
      need(input$route == "Anacapa to mainland" | 
        (input$route == "some other route" & input$route_other != ""),
            "Please describe the route")
    )
  }
  
  output$valid_page2 <- renderUI(validation2())
  
  validation3 <- function() {
    validate(
      need(input$cc_name != "", "Please enter a crew chief")
    )
  }
  
  output$valid_page3 <- renderUI(validation3())
  
  validation4 <- function() {
    validate(
      need(nchar(input$background_details) > 0 |
        (!is.null(input$swim_name1) && 
            input$swim_name1 != "" &&
            input$swim_dist1 != ""),
        "Need swim background"
      ),
      need(input$feed_plan != "", 
           "Please enter a feed plan"),
      need(input$feed_experience != "", 
           "Please enter experience with feed plan"),
      need(input$stroke_rate != "", 
           "Please enter stroke rate"),
      need(input$breathing != "[SELECT]",
           "Please select a breathing pattern"),
      need(input$hypothermia != "", 
           "Please enter hypothermia experience"),
      need(input$night_swimming != "", 
           "Please enter night swimming experience")
    )
  }
  
  output$valid_page4 <- renderUI(validation4())
  
  validation5 <- function() {
    validate(
      need(input$med1, "You must agree with statement #1"),
      need(input$med2, "You must agree with statement #2")
    )
  }
  
  output$valid_page5 <- renderUI(validation5())

  validation6 <- function() {
    validate(
      need(input$initial1 != "", "Please initial item #1"),
      need(input$initial2 != "", "Please initial item #2"),
      need(input$initial3 != "", "Please initial item #3"),
      need(input$initial4 != "", "Please initial item #4"),
      need(input$initial5 != "", "Please initial item #5"),
      need(input$initial6 != "", "Please initial item #6"),
      need(input$initial7 != "", "Please initial item #7"),
      need(input$initial8 != "", "Please initial item #8"),
      need(input$initial9 != "", "Please initial item #9"),
      need(input$initial10 != "", "Please initial item #10"),
      need(input$initial11 != "", "Please initial item #11"),
      # need(input$waiver_box,      "Please check the box"),
      need(input$waiver_sig != "", "Signature is required")
    )
  }

  output$valid_page6 <- renderUI(validation6())

  validation7 <- function() {
    validate(
      need(input$payment_choice != "[SELECT]", 
            "Please select a payment method"),
      need(input$current_lifetime != "[SELECT]", 
            "Please indicate your lifetime member status"),
      need(input$cancel_policy, 
            "Please check the box that you understand the cancellation policy")
    )
  }

  output$valid_page7 <- renderUI(validation7())

  observe({
    if (all(is.null( validation1() ), 
            is.null( validation2() ),
            is.null( validation3() ),
            is.null( validation4() ),
            is.null( validation5() ),
            is.null( validation6() ),
            is.null( validation7() ) 
      )) {
      shinyjs::removeClass(
        class = "disabled-link", 
        selector = ".nav li:nth-child(10) a"
      )}
      shinyjs::addClass(class = "green", selector = ".nav li:nth-child(10) a")
  })

  # construct output file paths
  observe({
    rv$fn <- gsub(" ", "_", input$s_name) %>% tolower
    rv$file_md <- file.path(DATADIR, paste0(rv$fn, ".md"))
    rv$file_html <- file.path(DATADIR, paste0(rv$fn, ".html"))
    rv$file_pdf <- file.path(DATADIR, paste0(rv$fn, ".pdf"))
  })

  observeEvent(input$gen_markdown, {
    out <- knit_expand("template.md")  # expand R expressions in template
    writeLines(out, rv$file_md)
    markdownToHTML(text = out, 
                   output = rv$file_html,
                   stylesheet = "../output.css")
    rv$v <- rv$v + 1
    shinyjs::show("preview")
    shinyjs::show("draw_sig")
    shinyjs::show("submit_button")
    shinyjs::removeClass(
        class = "disabled-link", 
        selector = ".nav li:nth-child(11) a"
    )
    shinyjs::addClass(class = "green", selector = ".nav li:nth-child(11) a")
  })

  output$preview <- renderUI({
    rv$v
    includeHTML(rv$file_html)
  })

  observeEvent(input$submit_app, {
    shinyjs::disable("submit_app")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    tryCatch({
      # convert html to pdf
      system(paste("wkhtmltopdf", rv$file_html, rv$file_pdf))
      # email notification
      options("gmailr.httr_oath_cache" = 'gm_auth')
      gmail_auth()
      mime("Reply-To" = input$s_email) %>%
        to(emails) %>% 
        from(emails) %>%
        subject(paste0("SBCSA sanction application for ", input$s_name)) %>%
        attach_file(rv$file_pdf) %>%
        send_message
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::hide("submit_app")
      shinyjs::hide("submit_msg")
      shinyjs::show("download_next_steps")
      shinyjs::addClass(
        class = "disabled-link", 
        selector = ".nav li:nth-child(2) a,
                    .nav li:nth-child(3) a,
                    .nav li:nth-child(4) a,
                    .nav li:nth-child(5) a,
                    .nav li:nth-child(6) a,
                    .nav li:nth-child(7) a,
                    .nav li:nth-child(8) a,
                    .nav li:nth-child(9) a,
                    .nav li:nth-child(10) a,
                    .nav li:nth-child(12) a"
      )
    })
  })
  
  output$download <- downloadHandler(
    filename = "SBSCA_sanction_app.pdf",
    content <- function(file) file.copy(rv$file_pdf, file)
  )
}

ui <- function(request) {
  fluidPage(useShinyjs(),
    tags$head(
      includeCSS("../custom.css"),
      HTML("<title>SBCSA Solo Swim Sanction Application</title>"),
      includeScript("../GA.js")
    ),
    
    navlistPanel("Solo Application", id = "navlist",
        
      # -------- INSTRUCTIONS -------------

      tabPanel("Instructions",
        includeMarkdown("../_includes/instructions.md")
      ),

      # -------- THE SWIMMER -------------
      
      tabPanel("The Swimmer",
        h2("The Swimmer"),
        hidden(actionButton("fill_sample", "Fill in sample data")),
        reqd(textInput("s_name", "Full Name")),
        fluidRow(
          column(6, reqd(dateInput("s_dob", "Date of Birth", startview = "year",
                          min = Sys.Date() - years(100), max = Sys.Date() - 365,
                          value = Sys.Date() - years(40)))),
          column(6, reqd(selectInput("s_gender", 
                                     "Gender",
                                     choices = c("[SELECT]", "female", "male"),
                                     selectize = F)))
        ),
        fluidRow(
          column(6, reqd(textInput("s_email", "Email Address"))),
          column(6, textInput("s_phone", "Mobile Phone"))
        ),
        reqd(textAreaInput("s_mailing", "Mailing Address", width = 400, height = 125)),
        fluidRow(
          column(6, 
            reqd(selectInput("s_country", 
                              "Country", 
                               choices = countries,
                               selectize = F))
          ),
          column(6, 
            checkboxInput("other_citizen", width = "100%",
                          label = "Check if you a primary citizen of a 
                                   different country than your residence")
          )
        ),
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
        h2("The Swim"),
        radioButtons("route", "What route will you be swimming?", 
          choices = c("Anacapa to mainland", "some other route")
        ),
        hidden(
          textInput("route_other", "Please describe your proposed route")
        ),
        reqd(selectInput("boat", "Escort Boat", choices = boats, selectize = F)),
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
        radioButtons("publicize", NULL, c("Yes", "No, please keep it private")),
        uiOutput("valid_page2")
      ),
    
      # -------- SUPPORT TEAM ---------------
      
      tabPanel("Support Team",
        h2("Support Team"),
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
        hidden(actionButton("remove_crew", "Delete Crew Member")),
        div(id = "crew"),
        uiOutput("valid_page3")
      ),
      
      # -------- MARATHON SWIMMING BACKGROUND --------
      
      tabPanel("Swim Experience",
        h2("Swim Experience"),
        p("Please list up to five documented marathon swims you have completed, especially swims from the past 1-3 years."),
        actionButton("add_swim", "Add Documented Swim"),
        hidden(actionButton("remove_swim", "Delete Most Recent")),
        div(id = "doc_swims"),
        hr(),
        p("You may wish to provide additional information about your 
          swimming background and training -- especially if your documented 
          marathon swimming experience is somewhat sparse."),
        p("Provide more details on swimming background?"),
        checkboxInput("more_background", "Yes"),
        hidden(div(id = "more_details",
          includeMarkdown("../_includes/more_background.md"),
          textAreaInput("background_details", "Details", width = 400, height = 200)
        )),
        reqd(textAreaInput("feed_plan", "What is your feeding plan? 
                      Product(s)? Frequency? From boat or kayak?",
                      width = 400, height = 125)),
        reqd(textAreaInput("feed_experience", "What experience do you have 
                      using this feed plan on long swims?",
                      width = 400, height = 125)),
        reqd(textInput("stroke_rate", "What is your typical stroke rate 
                  for a swim of this distance (strokes per minute)?")),
        reqd(selectInput("breathing", "What is your breathing pattern?",
                    c("[SELECT]", "right only", "left only", "mostly right, but can
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
      
      tabPanel("Medical Clearance",
        h2("Medical Clearance"),
        includeMarkdown("../_includes/medical.md"),
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
      
      tabPanel("Liability Waiver",
        h2("Liability Waiver"),
        h5(waiver[1]), hr(),
        p(waiver[2]), 
        p(waiver[3]), reqd(textInput("initial1", "Initials", width = 70)),
        p(waiver[4]), reqd(textInput("initial2", "Initials", width = 70)),
        p(waiver[5]), reqd(textInput("initial3", "Initials", width = 70)),
        p(waiver[6]), reqd(textInput("initial4", "Initials", width = 70)),
        p(waiver[7]), reqd(textInput("initial5", "Initials", width = 70)),
        p(waiver[8]), reqd(textInput("initial6", "Initials", width = 70)),
        p(waiver[9]), reqd(textInput("initial7", "Initials", width = 70)),
        p(waiver[10]), reqd(textInput("initial8", "Initials", width = 70)),
        p(waiver[11]), reqd(textInput("initial9", "Initials", width = 70)),
        p(waiver[12]), reqd(textInput("initial10", "Initials", width = 70)),
        p(waiver[13]), reqd(textInput("initial11", "Initials", width = 70)),
        uiOutput("waiver_agree"),
        reqd(textInput("waiver_sig", 
                       "Electronic Signature",
                       placeholder = "Please type your full name")),
        uiOutput("valid_page6")
      ),
      
      # --------- SANCTION FEES ---------
      
      tabPanel("Sanction Fees",
        h2("Sanction Fees"),
        includeMarkdown("../_includes/sanction_fees.md"),
        
        selectInput("payment_choice", 
                    label = "Which method of payment do you prefer?",
                    choices = c("[SELECT]", "Dwolla", "PayPal", 
                                "personal check", "wire transfer")
        ),
        
        p("We will send specific instructions along with the invoice."),
        
        h3("Lifetime Membership"),
        selectInput("current_lifetime",
                    label = "Are you currently a SBCSA Lifetime Member?",
                    choices = c("[SELECT]", "No", "Yes")
        ),
        hidden(div(id = "new_lifetime",
          includeMarkdown("../_includes/lifetime.md"),
          radioButtons("lifetime_purchase", width = "100%",
                       label = "Are you interesting in purchasing a 
                                Lifetime Membership at this time?", 
                       choices = c("No", "Yes")
          )
        )),
        
        h3("Cancellation Policy"),
        includeMarkdown("../_includes/cancel_policy.md"),
        p("I understand the cancellation policy"),
        checkboxInput("cancel_policy", "Yes"),
        uiOutput("valid_page7")
      ),
      
      # --------- PREVIEW PAGE -----------
      
      tabPanel("PREVIEW",
        h2("Preview"), 
        p("If you change any data in the previous tabs,
          you can re-generate the preview."),
        hr(),
        actionButton("gen_markdown", "Generate Preview"),
        hidden(uiOutput("preview"))
      ),
      
      # --------- SUBMIT APP -----------
      
      tabPanel("SUBMIT",
        h2("Submit Application"),
        actionButton("submit_app", "SUBMIT"),
        hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error", 
              div(br(), tags$b("Error: "), span(id = "error_msg"))
        )),
        hidden(div(id = "download_next_steps",
          h4("Application submitted. If you would like to download a copy
            for your records, please click this button:"),
          downloadButton("download"),
          includeMarkdown("../_includes/next_steps.md")
        ))
      ),
      
      # --------- SAVE & RETURN -----------
      
      tabPanel("Save & Return Later", 
        h2("Save and Return Later"),
        p("Click to save your data and return later."),
        p("A small window will pop up - copy the URL to your computer.
           To return to your application, enter this URL into your browser."),
        bookmarkButton()
      )
    )
  )
}
enableBookmarking(store = "server")
shinyApp(ui = ui, server = server)