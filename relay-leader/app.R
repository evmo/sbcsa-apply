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
waiver <- readLines("../_includes/waiver.txt")
emails <- readLines("../conf/emails.txt")
source("../common/func.R")
source("../common/func-ui.R")
DIR <- if (dev_mode()) "~/dev/sbcsa-apply" else "~/sbcsa-apply"
DATADIR <- file.path(DIR, "data")

server <- function(input, output, session) {

  rv <- reactiveValues(v = 0)
  
  output$team_members <- renderUI({
    lapply(1:input$team_size, function(x) {
      fluidRow(
        column(6, 
          textInput(paste0("member_name", x), paste("Swimmer", x, "Name"))
        ),
        column(6, 
          textInput(paste0("member_email", x), paste("Swimmer", x, "Email"))
        ))
    })
  })

  output$alternates <- renderUI({
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
  })

  validation1 <- function() {
    validate(
      need(input$s_name != "", 
           "Please enter your name"),
      need(grepl(".+@.+\\..+", input$s_email) || grepl(".+@.+\\..+", input$p_email), 
           "Please enter a valid email address"),
      need(input$s_mailing != "" || input$p_mailing != "", 
           "Please enter a mailing address")
    )
  }

  output$valid_page1 <- renderUI(validation1())

  validation2 <- function() {
    validate(
      need(
      )
    )
  }

  output$valid_page2 <- renderUI(validation2())

  validation3 <- function() {
    validate(
      need(
      )
    )
  }

  output$valid_page3 <- renderUI(validation3())
}

ui <- function(request) {
  fluidPage(useShinyjs(),
    tags$head(
      includeCSS("../custom.css"),
      HTML("<title>SBCSA Relay Team Leader Application</title>"),
      includeScript("../GA.js")
    ),
    
    navlistPanel("Relay Team Leader", id = "navlist", widths = c(3, 9),
        
      # -------- START HERE -------------

      tabPanel("Start Here",
        includeMarkdown("instructions.md"),
        swimmer_info(),
        uiOutput("valid_page1")
      ),

      # -------- THE TEAM -------------

      tabPanel("The Team",
        textInput("team_name", "Team Name"),
        fluidRow(
          column(4, 
            selectInput("team_size", "Number of swimmers", choices = seq(2, 6))
          ),
          column(4, 
            selectInput("alt_size", "Number of alternates?", choices = seq(0, 2))
          ),
          column(4,
            textInput("leg_duration", "Leg duration", placeholder = "minutes")
          )
        ),
        hr(),
        uiOutput("team_members"),
        uiOutput("alternates"),
        uiOutput("valid_page2")
      ),

      # -------- THE SWIM -------------

      tabPanel("The Swim",
        route_boat_date(),
        fluidRow(
          column(6, 
            dateInput("splash_date", 
                      label = "When is the swim scheduled to begin?",
                      startview = "year",
                      min = Sys.Date(),
                      max = Sys.Date() + years(1))
          ),
          column(6, 
            selectInput("splash_time", "Hour", seq(0, 23))
          )
        ),
        publicize(),
        uiOutput("valid_page3")
      )
    )
  )
}

enableBookmarking(store = "server")
shinyApp(ui = ui, server = server)