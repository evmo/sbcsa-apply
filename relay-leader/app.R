#------------------ RELAY LEADER APP ---------------------

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
emails <- readLines("../conf/emails.txt")
source("../common/func.R")
source("../common/func-ui.R")
DIR <- if (dev_mode()) "~/dev/sbcsa-apply" else "~/sbcsa-apply"
DATADIR <- file.path(DIR, "data")
source("fill_sample.R")

server <- function(input, output, session) {

  rv <- reactiveValues(v = 0)

  # disable SUBMIT tab
  shinyjs::addClass(class = "disabled-link", 
                    selector = ".nav li:nth-child(6) a")
  # Save & Return --> red
  shinyjs::addClass(class = "red", 
    selector = ".nav li:nth-child(7) a")
  # Intro page --> green
  greenify(2)
  # don't need to know gender
  shinyjs::hide("s_gender")

  # fill in all inputs - for testing preview & submit
  observeEvent(input$fill_sample, {
    if (dev_mode())
      fill_sample(session)
  })

  # observers for toggling hidden inputs
  observe({  
    if (dev_mode()) shinyjs::show("fill_sample")
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
    } else {
      shinyjs::hide("boat_other")
      shinyjs::hide("other_pilot_harbor")
    }

    if (input$boat_known == "TO BE DETERMINED") shinyjs::show("boat_notify")
    else                                        shinyjs::hide("boat_notify")

    if (rv$crew_count > 0) shinyjs::show("remove_crew")
    else                   shinyjs::hide("remove_crew")
  })
  
  output$team_members <- renderUI({
    if (input$team_size == "[SELECT]") NULL
    else {
      list(
        h3("Team Members"),
        lapply(1:input$team_size, function(x) {
          fluidRow(
            column(6, 
              textInput(paste0("member_name", x), paste("Swimmer", x, "Name"))
            ),
            column(6, 
              textInput(paste0("member_email", x), paste("Swimmer", x, "Email"))
            ))
        })
      )
    }
  })

  output$alt_members <- renderUI({
    if (input$alt_size == 0) NULL
    else {
      lapply(1:input$alt_size, function(x) {
        fluidRow(
          column(6, 
            textInput(paste0("alt_name", x), paste("Alternate", x, "Name"))
          ),
          column(6, 
            textInput(paste0("alt_email", x), paste("Alternate", x, "Email"))
          )
        )
      })
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

  output$finish_ui <- renderUI({
    choices <- c(islands[islands != input$start], "circumnavigation")
    selectInput("finish", "Finish Location", choices = choices, 
                selected = "mainland", selectize = F)
  })

  #----------- PAGE VALIDATIONS ----------------#

  source("validations.R", local = T)$value

  output$valid_page1 <- renderUI(renderValid(v1(), f1))
  output$valid_page2 <- renderUI(renderValid(v2(), f2))
  output$valid_page3 <- renderUI(renderValid(v3(), f3))

  observe(toggle_green(3, is_valid(v1())))
  observe(toggle_green(4, is_valid(v2())))
  observe(toggle_green(5, is_valid(v3())))

  all_valid <- reactive({
    all(is_valid(v1()), 
        is_valid(v2()), 
        is_valid(v3()))
  })

  # if all page validations pass, enable preview tab
  observe({
    req(!is.na(all_valid()))
    if (all_valid()) {
      undisable(6)
      greenify(6)
    } else {
      disable(6)
      degreen(6)
    }
  })

  # submit actions
  observeEvent(input$submit_app, {
    shinyjs::disable("submit_app")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    tryCatch({
      fn <- gsub(" ", "_", input$team_name) %>% tolower
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
                    paste(".nav li:nth-child(", c(2:5, 7), ")", sep = ""), 
                    collapse = ",")
      )
    })
  })

  onBookmark(function(state) {
    log_save("leader", input$s_name, input$s_email)
  })
}

ui <- function(request) {
  fluidPage(useShinyjs(),
    tags$head(
      includeCSS("../custom.css"),
      HTML("<title>SBCSA Relay Team Leader Application</title>"),
      includeScript("../GA.js")
    ),
    
    navlistPanel("Relay Team Leader", id = "navlist", widths = c(3, 9),
        
      # -------- INSTRUCTIONS -------------

      tabPanel("Instructions",
        includeMarkdown("instructions.md"),
        hidden(actionButton("fill_sample", "Fill in sample data"))
      ),

      # -------- TEAM LEADER INFO -------------

      tabPanel("Team Leader Info",
        h2("Team Leader Info"),
        swimmer_info(),
        uiOutput("valid_page1")
      ),

      # -------- THE TEAM -------------

      tabPanel("The Team",
        h2("The Team"),
        reqd(textInput("team_name", "Team Name")),
        fluidRow(
          column(4, 
            reqd(selectInput("team_size", "Number of swimmers", 
                         choices = c("[SELECT]", seq(2, 6))))
          ),
          column(4, 
            reqd(selectInput("alt_size", "Number of alternates?", 
                         choices = seq(0, 2)))
          ),
          column(4,
            reqd(numericInput("leg_duration", "Leg duration",
                          min = 30, value = 60, step = 15))
          )
        ),
        hr(),
        uiOutput("team_members"),
        uiOutput("alt_members"),
        h3("Support Crew"),
        support_crew(),
        uiOutput("valid_page2")
      ),

      # -------- THE SWIM -------------

      tabPanel("The Swim",
        h2("The Swim"),
        route_boat_date(),
        fluidRow(
          column(6, 
            reqd(dateInput("splash_date", 
                      label = "When is the swim scheduled to begin?",
                      startview = "year",
                      min = Sys.Date(),
                      max = Sys.Date() + years(1)))
          ),
          column(6, 
            selectInput("splash_time", "Hour", seq(0, 23))
          )
        ),
        publicize(),
        uiOutput("valid_page3")
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