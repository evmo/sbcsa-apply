#------------------ RELAY MEMBER APP ---------------------

library(shiny)
library(shinyjs)
library(knitr)
library(lubridate)
library(rmarkdown)
library(magrittr)
library(markdown)
library(gmailr)

teams <- read.csv("../data/teams.csv", stringsAsFactors = F)
countries <- readLines("../_includes/countries.txt")
emails <- readLines("../conf/emails.txt")
source("../common/func.R")
source("../common/func-ui.R")
DIR <- if (dev_mode()) "~/dev/sbcsa-apply" else "~/sbcsa-apply"
DATADIR <- file.path(DIR, "data")
source("fill_sample.R")

server <- function(input, output, session) {

  rv <- reactiveValues(v = 0, swim_date = Sys.Date())

  # disable SUBMIT tab
  disable(7)
  # Save & Return --> red
  shinyjs::addClass(class = "red", 
    selector = ".nav li:nth-child(8) a")

  # fill in all inputs - for testing preview & submit
  observeEvent(input$fill_sample, {
    if (dev_mode())
      fill_sample(session)
  })

  # observers for toggling hidden inputs
  observe({  
    if (dev_mode()) shinyjs::show("fill_sample")
    if (input$current_lifetime == "No") shinyjs::show("new_lifetime") 
    else                                shinyjs::hide("new_lifetime")
  })

  # calculate age on date of swim
  observe({
    req(is_date(input$s_dob))
    if (input$team_name == "[SELECT]")
      rv$swim_date <- Sys.Date()
    else
      rv$swim_date <- as.Date(teams[teams$name==input$team_name, "swim_date"])
    
    rv$swim_age <- as.period(interval(start = input$s_dob, 
                                      end = rv$swim_date))$year
    # toggle under18 parent/guardian inputs
    if (rv$swim_age < 18 && rv$swim_age >= 14) {
      shinyjs::show("under18")
      shinyjs::hide("swimmer_contact")
      shinyjs::show("parent_contact")
      shinyjs::show("waiver_parent")
    } else {
      shinyjs::hide("under18")
      shinyjs::show("swimmer_contact")
      shinyjs::hide("parent_contact")
      shinyjs::hide("waiver_parent")
    }
  })

  #----------- PAGE VALIDATIONS ----------------#

  source("validations.R", local = T)$value

  output$valid_page0 <- renderUI(renderValid(v0(), f0))
  output$valid_page1 <- renderUI(renderValid(v1(), f1))
  output$valid_page2 <- renderUI(renderValid(v2(), f2))
  output$valid_page3 <- renderUI(renderValid(v3(), f3))
  output$valid_page4 <- renderUI(renderValid(v4(), f4))

  observe(toggle_green(2, is_valid(v0())))
  observe(toggle_green(3, is_valid(v1())))
  observe(toggle_green(4, is_valid(v2())))
  observe(toggle_green(5, is_valid(v3())))
  observe(toggle_green(6, is_valid(v4())))

  all_valid <- reactive({
    all(is_valid(v0()), 
        is_valid(v1()), 
        is_valid(v2()), 
        is_valid(v3()),
        is_valid(v4()))
  })

  # if all page validations pass, enable preview tab
  observe({
    req(!is.na(all_valid()))
    if (all_valid()) {
      undisable(7)
      greenify(7)
    } else {
      disable(7)
      degreen(7)
    }
  })

  # submit actions
  observeEvent(input$submit_app, {
    shinyjs::disable("submit_app")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    tryCatch({
      fn <- gsub(" ", "_", 
              paste(input$team_name, 
                    input$s_name)) %>% tolower
      file_md <- file.path(DATADIR, paste0(fn, ".md"))
      out <- knit_expand("template.md")  # expand R expressions in template
      writeLines(out, file_md)
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::hide("submit_app")
      shinyjs::hide("submit_msg")
      shinyjs::show("next_steps")
      shinyjs::addClass(
        class = "disabled-link", 
        selector = paste(
                    paste(".nav li:nth-child(", c(2:6, 8), ")", sep = ""), 
                    collapse = ",")
      )
    })
  })

  onBookmark(function(state) {
    log_save("member", input$s_name, input$s_email)
  })
}

ui <- function(request) {
  fluidPage(useShinyjs(),
    tags$head(
      includeCSS("../custom.css"),
      HTML("<title>SBCSA Relay Member Application</title>"),
      includeScript("../GA.js")
    ),
    
    navlistPanel("Relay Member", id = "navlist", widths = c(3, 9),
        
      # -------- INSTRUCTIONS -------------

      tabPanel("Start Here",
        includeMarkdown("instructions.md"),
        fluidRow(
          column(6, reqd(dateInput("s_dob", 
                      label = "What is the swimmer's date of birth?",
                      value = "1970-01-01",
                      startview = "year",
                      min = Sys.Date() - years(100),
                      max = Sys.Date() - years(13)))
          ),
          column(6, reqd(selectInput("team_name", 
                                "Relay Team Name", 
                                choices = c("[SELECT]", teams$name),
                                selectize = F))
          )
        ),
        hidden(div(id = "under18",
          h4("Swimmer will be younger than 18 on the date of the swim.
              Is there a parent or guardian present to help complete 
              the rest of the application?"),
          checkboxInput("parent_present", "Yes")
        )),
        hidden(actionButton("fill_sample", "Fill in sample data")),
        uiOutput("valid_page0")
      ),

      # -------- SWIMMER INFO -------------

      tabPanel("Swimmer Info",
        h2("Swimmer Info"),
        swimmer_info(),
        parent_contact(),
        emerg_contact(),
        under18(),
        uiOutput("valid_page1")
      ),

      # -------- SWIM EXPERIENCE -------------

      tabPanel("Swim Experience",
        h2("Swim Experience"),
        reqd(textAreaInput("experience", width = 500, height = 300,
                      "Please describe your ocean swimming experience")),
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
        uiOutput("valid_page2")
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
        uiOutput("valid_page3")
      ),
      
      # ------ WAIVER & RELEASE OF LIABILITY ---------
      
      tabPanel("Liability Waiver",
        waiver_ui(),
        uiOutput("valid_page4")
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
        hidden(div(id = "next_steps",
          h4("Application submitted."),
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