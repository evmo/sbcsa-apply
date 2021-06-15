#------------------ SOLO APP ---------------------

library(shiny)
library(shinyjs)
library(knitr)
library(lubridate)
library(rmarkdown)
library(magrittr)
library(markdown)
library(gmailr)

countries <- readLines("../_includes/countries.txt")
islands <- readLines("../_includes/islands.txt")
source("../common/func.R")
source("../common/func-ui.R")
source("swim_experience_ui.R")
DIR <- if (dev_mode()) "~/dev/sbcsa-apply" else "~/sbcsa-apply"
DATADIR <- file.path(DIR, "data")
source("fill_sample.R")

server <- function(input, output, session) {

  rv <- reactiveValues(v = 0)

  # disable PREVIEW & SUBMIT tabs
  disable(10)
  disable(11)
  # Save & Return --> red
  shinyjs::addClass(class = "red",
    selector = ".nav li:nth-child(12) a")

  # fill in all inputs - for testing preview & submit
  observeEvent(input$fill_sample, {
    if (dev_mode())
      fill_sample(session)
  })

  # observers for toggling hidden inputs
  observe({
    if (dev_mode()) shinyjs::show("fill_sample")

    if (input$other_citizen == T) shinyjs::show("s_citizenship")
    else                           shinyjs::hide("s_citizenship")

    if (input$start == "not listed here") {
      shinyjs::show("custom_route")
      shinyjs::hide("finish_ui")
    } else {
      shinyjs::hide("custom_route")
      shinyjs::show("finish_ui")
    }

    if (input$boat_known == "BOAT NOT LISTED") {
      shinyjs::show("boat_other")
      shinyjs::show("other_pilot_harbor")
      shinyjs::show("more_boat_details")
    } else {
      shinyjs::hide("boat_other")
      shinyjs::hide("other_pilot_harbor")
      shinyjs::hide("more_boat_details")
    }

    if (input$boat_known == "TO BE DETERMINED") {
      shinyjs::show("boat_notify")
      shinyjs::show("more_boat_details")
    } else {
      shinyjs::hide("boat_notify")
      shinyjs::hide("more_boat_details")
    }

    if (rv$crew_count > 0) shinyjs::show("remove_crew")
    else                   shinyjs::hide("remove_crew")

    if (input$current_lifetime == "No") shinyjs::show("new_lifetime")
    else                                shinyjs::hide("new_lifetime")
  })

  # update harbor_date input with already-entered splash_date
  observeEvent(input$splash_date, {
    updateDateInput(session, "harbor_date", value = input$splash_date)
  })

  # display already-inputted splash date on "Swim" tab
  output$splash_date_disp <- renderUI(p(input$splash_date))

  # remove start location from selectInput of finish location
  output$finish_ui <- renderUI({
    choices <- c(islands[islands != input$start], "circumnavigation")
    selectInput("finish", "Finish Location", choices = choices,
                selected = "mainland", selectize = F)
  })

  # calculate age on date of swim
  observe({
    req(is_date(input$s_dob))
    req(is_date(input$splash_date))
    rv$swim_age <- as.period(interval(start = input$s_dob,
                                      end = input$splash_date))$year
    # toggle under18 parent/guardian inputs
    if (rv$swim_age < 18 && rv$swim_age >= 14) {
      shinyjs::show("under18")
      shinyjs::hide("swimmer_contact")
      shinyjs::show("parent_contact")
      shinyjs::show("waiver_minor")
      shinyjs::hide("waiver_adult")
    } else {
      shinyjs::hide("under18")
      shinyjs::show("swimmer_contact")
      shinyjs::hide("parent_contact")
      shinyjs::hide("waiver_minor")
      shinyjs::show("waiver_adult")
    }
  })

  # insertUI for crew members
  rv$crew_count <- 0
  observeEvent(input$add_crew, {
    rv$crew_count <<- rv$crew_count + 1
    crewid <- paste0('crew', rv$crew_count)
    insert_crew(crewid, rv$crew_count)
  })

  # removeUI for crew members
  observeEvent(input$remove_crew, {
    removeUI(selector = paste0('#crew', rv$crew_count))
    if (rv$crew_count > 0)
      rv$crew_count <<- rv$crew_count - 1
  })

  #----------- PAGE VALIDATIONS ----------------#

  source("validations.R", local = T)$value

  output$valid_page0 <- renderUI(renderValid(v0(), f0))
  output$valid_page1 <- renderUI(renderValid(v1(), f1))
  output$valid_page2 <- renderUI(renderValid(v2(), f2))
  output$valid_page3 <- renderUI(renderValid(v3(), f3))
  output$valid_page4 <- renderUI(renderValid(v4(), f4))
  output$valid_page5 <- renderUI(renderValid(v5(), f5))
  output$valid_page7 <- renderUI(renderValid(v7(), f7))

  observe(toggle_green(2, is_valid(v0())))
  observe(toggle_green(3, is_valid(v1())))
  observe(toggle_green(4, is_valid(v2())))
  observe(toggle_green(5, is_valid(v3())))
  observe(toggle_green(6, is_valid(v4())))
  observe(toggle_green(7, is_valid(v5())))
  observe(toggle_green(8, is_valid(v5())))
  observe(toggle_green(9, is_valid(v7())))

  all_valid <- reactive({
    all(is_valid(v0()),
        is_valid(v1()),
        is_valid(v2()),
        is_valid(v3()),
        is_valid(v4()),
        is_valid(v5()),
        is_valid(v7()))
  })

  # if all page validations pass, enable preview tab
  observe({
    req(!is.na(all_valid()))
    if (all_valid()) {
      undisable(10)
      greenify(10)
    } else {
      disable(10)
      degreen(10)
      disable(11)
      degreen(11)
    }
  })

  # construct output file paths
  observe({
    rv$fn <- gsub(" ", "_", input$s_name) %>% tolower
    rv$file_md <- file.path(DATADIR, paste0(rv$fn, ".md"))
    rv$file_html <- file.path(DATADIR, paste0(rv$fn, ".html"))
    rv$file_pdf <- file.path(DATADIR, paste0(rv$fn, ".pdf"))
  })

  # generate markdown preview file
  observeEvent(input$gen_markdown, {
    out <- knit_expand("template.md")  # expand R expressions in template
    writeLines(out, rv$file_md)
    markdownToHTML(text = out,
                   output = rv$file_html,
                   stylesheet = "../output.css")
    rv$v <- rv$v + 1
    shinyjs::show("preview")
    shinyjs::show("submit_button")
    undisable(11)  # enable submit tab
    greenify(11)
  })

  # display markdown preview
  output$preview <- renderUI({
    rv$v
    includeHTML(rv$file_html)
  })

  # submit actions
  observeEvent(input$submit_app, {
    shinyjs::disable("submit_app")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    tryCatch({
      # convert html to pdf
      system(paste("wkhtmltopdf", rv$file_html, rv$file_pdf))
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
        selector = paste(
                    paste(".nav li:nth-child(", c(3:10, 12), ")", sep = ""),
                    collapse = ",")
      )
    })
  })

  output$download <- downloadHandler(
    filename = "SBSCA_sanction_app.pdf",
    content <- function(file) file.copy(rv$file_pdf, file)
  )

  onBookmark(function(state) {
    log_save("solo", input$s_name, input$s_email)
  })
}

ui <- function(request) {
  fluidPage(useShinyjs(),
    tags$head(
      includeCSS("../custom.css"),
      HTML("<title>SBCSA Solo Swim Sanction Application</title>"),
      includeScript("../GA.js")
    ),

    navlistPanel("Solo Swim", id = "navlist", widths = c(3, 9),

      # -------- START HERE -------------

      tabPanel("Start Here",
        includeMarkdown("instructions.md"),
        h3("Complete these questions first:"),
        fluidRow(
          column(6,
            dateInput("s_dob",
                      label = "What is the swimmer's date of birth?",
                      value = "1970-01-01",
                      startview = "year",
                      min = Sys.Date() - years(100),
                      max = Sys.Date() - years(13))
          ),
          column(6,
            dateInput("splash_date",
                      label = "When is the swim scheduled to begin?",
                      value = Sys.Date(),
                      startview = "month",
                      min = Sys.Date(),
                      max = Sys.Date() + years(1))
          )
        ),
        under18(),
        hidden(actionButton("fill_sample", "Fill in sample data")),
        uiOutput("valid_page0")
      ),

      # -------- THE SWIMMER -------------

      tabPanel("The Swimmer",
        h2("The Swimmer"),
        swimmer_info(),
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

        parent_contact(),
        emerg_contact(),
        uiOutput("valid_page1")
      ),

      # ----------- THE SWIM --------------

      tabPanel("The Swim",
        h2("The Swim"),
        route_boat_date(),
        fluidRow(
          column(4, h5("Splash Date"), uiOutput("splash_date_disp")),
          column(4, selectInput("splash_time", "Time of Day",
            c("12:01am - 4:00am", "4:01am - 8:00am", "8:01am - noon",
              "afternoon", "early evening / sunset", "late night")))
        ),
        publicize(),
        uiOutput("valid_page2")
      ),

      # -------- SUPPORT TEAM ---------------

      tabPanel("Support Team",
        h2("Support Team"),
        support_crew(),
        uiOutput("valid_page3")
      ),

      # -------- MARATHON SWIMMING EXPERIENCE --------

      tabPanel("Swim Experience",
        swim_experience(),
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
        reqd(dateInput("med_date",
                       label = "Date of Medical Exam",
                       min = Sys.Date() - years(1),
                       max = Sys.Date() + months(6))),
        hr(),
        h4("Vaccination Requirement for 2021 Swims"),
        p("In the interest of the health and safety of participants in SBCSA-
           sanctioned swims in 2021, everyone aboard the escort boat -
           swimmer, crew, pilot, and observer - will be required to
           provide proof of COVID-19 vaccination."),
        reqd(checkboxInput("vax",
              "I understand and will comply with this requirement",
              width = "100%")),
        uiOutput("valid_page5")
      ),

      # ------ LIABILITY WAIVER ---------

      tabPanel("Liability Waiver",
        h2("Liability Waiver"),
        p("All SBCSA-sanctioned swimmers are required to sign a
           liability waiver."),
        hidden(div(id = "waiver_minor",
          p("After this application is processed, a SBCSA representative
             will reach out to the swimmer and their designated parent/guardian
             to complete the waiver")
        )),
        hidden(div(id = "waiver_adult",
          p("Before continuing this application, please read and
             sign the waiver at HelloSign:"),
          p(strong(a(href="https://app.hellosign.com/s/LQBu8o2D",
                     "https://app.hellosign.com/s/LQBu8o2D"))),
          checkboxInput("waiver_done", "I signed and submitted the waiver.")
        ))
      ),

      # --------- SANCTION FEES ---------

      tabPanel("Sanction Fees",
        h2("Sanction Fees"),
        includeMarkdown("../_includes/sanction_fees.md"),

        selectInput("payment_choice",
                    label = "Which method of payment do you prefer?",
                    choices = c("[SELECT]", "Credit Card", "PayPal",
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
          includeMarkdown("next_steps.md")
        ))
      ),

      # --------- SAVE & RETURN -----------

      tabPanel("Save & Return Later",
        save_return_ui()
      )
    )
  )
}
enableBookmarking(store = "server")
shinyApp(ui = ui, server = server)
