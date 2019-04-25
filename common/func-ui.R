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

parent_contact <- function() {
  list(
    hidden(div(id = "parent_contact",
      h4("Parent/Guardian Contact Info"),
      reqd(textInput("p_name", "Parent - Full Name")),
      fluidRow(
        column(6, reqd(textInput("p_email", "Email Address"))),
        column(6, textInput("p_phone", "Phone"))
      ),
      reqd(textAreaInput("p_mailing", "Mailing Address", width = 400, height = 125))
    ))
  )
}

emerg_contact <- function() {
  list(
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
    )
  )
}

under18 <- function() {
  list(
    hidden(div(id = "under18",
      h4("Swimmer will be younger than 18 on the date of the swim.
          Is there a parent or guardian present to help complete 
          the rest of the application?"),
      checkboxInput("parent_present", "Yes")
    ))
  )
}

route_boat_date <- function() {
  boats <- readLines("../_includes/boats.txt")
  islands <- readLines("../_includes/islands.txt")
  list(
    div(id = "route", fluidRow(
    column(6, reqd(selectInput("start", "Start Location",
      choices = islands, 
      selected = "Anacapa Island",
      selectize = F))),
    column(6, uiOutput("finish_ui"))
    )),
    hidden(textInput("custom_route", "Please describe your proposed route")),
    div(id = "boat", fluidRow(
      column(6,
        reqd(selectInput("boat_known", "Escort Boat/Pilot", 
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
      column(4, reqd(dateInput("harbor_date", 
                                "Boat Departs Harbor",
                                min = Sys.Date(),
                                max = Sys.Date() + years(1),
                                value = Sys.Date()))),
      column(4, selectInput("splash_time", "Time of Day", 
            c("12:01am - 4:00am", "4:01am - 8:00am", "8:01am - noon", 
              "afternoon", "early evening / sunset", "late night")))
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

save_return_ui <- function() {
  list(
    h2("Save and Return Later"),
    p("Click to save your data and return later."),
    p("A small window will pop up - copy the URL to your computer.
       To return to your application, enter this URL into your browser."),
    bookmarkButton()
  )
}