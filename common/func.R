reqd <- function(input) div(class = 'required', input)

dev_mode <- function() if (grepl("dev", getwd())) 1 else 0

days_in_month <- function(month) {
	m <- which(month.name == month)
	if (m %in% c(4, 6, 9, 11)) 30
    else if (m == 2) 29
    else 31
}

greenify <- function(n) {
  shinyjs::addClass(class = "green", 
                       selector = paste0(".nav li:nth-child(", n, ") a"))
}

degreen <- function(n) {
  shinyjs::removeClass(class = "green", 
                       selector = paste0(".nav li:nth-child(", n, ") a"))
}

toggle_green <- function(tab_num, condition) {
  shinyjs::toggleClass(class = "green", 
                       selector = paste0(".nav li:nth-child(", tab_num, ") a"),
                       condition = condition)
}

disable <- function(n) {
  shinyjs::addClass(class = "disabled-link", 
                       selector = paste0(".nav li:nth-child(", n, ") a"))
}

undisable <- function(n) {
  shinyjs::removeClass(class = "disabled-link", 
                       selector = paste0(".nav li:nth-child(", n, ") a"))
}

log_save <- function(app, name, email) {
  msg <- paste(app, name, email, Sys.time(), sep = "\t")
  write(msg, "../data/log.tsv", append = T)
}

is_date <- function(mydate) {
  tryCatch(!is.na(as.Date(mydate)),  
           error = function(err) {FALSE})  
}

is_valid <- function(tests) all(tests)

renderValid <- function(tests, feedback) {
  list(
    lapply(1:length(tests), function(x) {
      if (tests[x] == FALSE) {
        p(feedback[x])
      }
    })
  )
}