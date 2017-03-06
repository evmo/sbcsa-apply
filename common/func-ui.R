swimmer_info <- function() {
  list(
    fluidRow(
      column(6, reqd(textInput("s_name", "Full Name"))),
      column(6, reqd(selectInput("s_gender", 
                                 "Gender",
                                 choices = c("[SELECT]", "female", "male"),
                                 selectize = F)))
    ),
    div(id = "swimmer_contact",
      fluidRow(
        column(6, reqd(textInput("s_email", "Email Address"))),
        column(6, textInput("s_phone", "Phone"))
      ),
      reqd(textAreaInput("s_mailing", "Mailing Address", width = 400, height = 125))
    )
  )
}

route_boat_date <- function() {
  boats <- readLines("../_includes/boats.txt")
  list(
    div(id = "route",
    column(6, reqd(selectInput("start", "Start Location",
      choices = islands, 
      selected = "Anacapa Island",
      selectize = F))),
    column(6, uiOutput("finish_ui"))
    ),
    hidden(textInput("custom_route", "Please describe your proposed route")),
    div(id = "boat", fluidRow(
      column(6,
        reqd(selectInput("boat_known", "Escort Boat", 
          choices = boats, selectize = F))
        ),
      column(6,
        hidden(textInput("boat_other", "Name of Boat"))
        )
    )),
    hidden(div(id = "other_pilot_harbor", fluidRow(
      column(6, textInput("pilot_other", "Name of Pilot")),
      column(6, textInput("harbor_other", "Where is the boat docked?"))
    ))),
    hidden(div(id = "boat_notify", 
      p("Please notify the SBCSA as soon as you have
        made arrangements with an escort boat.")
    )),
    hr(),

    h4("When is your swim scheduled?"),
    fluidRow(
      column(6, dateInput("harbor_date", "Boat Departs Harbor")),
      column(6, selectInput("harbor_time", "Hour", seq(0, 23)))
    )
  )
}

publicize <- function() {
  list(
    h4("Permission to publicize?"),
    p("The SBCSA publishes a list of upcoming swim attempts at the 
      beginning of each season. Do we have your permission to 
      publicize your attempt, or do you prefer to keep it private?"),
    radioButtons("publicize", NULL, c("Yes", "No, please keep it private"))
  )
}

support_crew <- function() {
  list(
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
    hidden(actionButton("remove_crew", "Delete Last")),
    div(id = "crew")
  )
}

insert_crew <- function(crewid, crew_count) {
  insertUI(selector = "#crew", "beforeEnd", 
             ui = tagList(div(id = crewid,
                column(4, 
                 textInput(paste0("crew_name", crew_count), 
                           "Name")
                ),
                column(4, 
                 textInput(paste0("crew_role", crew_count), 
                           "Role")
                ),
                column(4, 
                 textInput(paste0("crew_contact", crew_count), 
                           "Contact Phone or Email")
                )
    )))
}

waiver_ui <- function() {
  waiver <- readLines("../_includes/waiver.txt")
  list(
    h2("Liability Waiver"),
      hidden(div(id = "waiver_parent", 
        h4("The parent or guardian of the swimmer should read this form,
          initial each section, and enter an electronic signature at the bottom.")
      )),
      h5(waiver[1]), 
      hr(),
      p(waiver[2]),
      
      lapply(3:13, function(i) { list(
        p(waiver[i]),
        reqd(textInput(paste0("initial", i-2), "Initials", width = 70))
      )}),

      checkboxInput("waiver_box", 
        label = "I have read this waiver and release of liability, fully understand its terms, understand that I have given up substantial rights by signing it, and have signed it freely and voluntarily without any inducement.",
        width = "100%"
      ),
      reqd(textInput("waiver_sig", 
                     "Electronic Signature",
                     placeholder = "Please type your full name"))
    )
}