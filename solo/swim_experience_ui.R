swim_experience <- function() {
	list(
  	 h2("Swim Experience"),

     # p("Please list up to five documented marathon swims you have completed, especially swims from the past 1-3 years."),
     # actionButton("add_swim", "Add Documented Swim"),
     # hidden(actionButton("remove_swim", "Delete Last")),
     # div(id = "doc_swims"),
     # hr(),
     # p("You may wish to provide additional information about your
     #   swimming background and training -- especially if your documented
     #   marathon swimming experience is somewhat sparse."),
     # p("Provide more details on swimming background?"),
     # checkboxInput("more_background", "Yes"),

     div(id = "more_details",
       includeMarkdown("../_includes/more_background.md"),
       textAreaInput("background_details", "Details", width = 400, height = 200)
      ),
     reqd(textAreaInput("feed_plan", "What is your feeding plan?
       Product(s)? Frequency? From boat or kayak?",
       width = 400, height = 125)),
     reqd(textAreaInput("feed_experience", "What experience do you have
       using this feed plan on long swims?",
       width = 400, height = 125)),
     reqd(textInput("stroke_rate", "What is your typical stroke rate
       for a swim of this distance (strokes per minute)?")),
     reqd(textAreaInput("hypothermia", "What is your experience (if any)
       with hypothermia in the context of swimming.",
       width = 400, height = 100)),
     reqd(textAreaInput("night_swimming", "What is your experience (if any)
       swimming at night",
       width = 400, height = 100))
    )
}
