reqd <- function(input) div(class = 'required', input)

dev_mode <- function() if (grepl("dev", getwd())) 1 else 0

days_in_month <- function(month) {
	m <- which(month.name == month)
	if (m %in% c(4, 6, 9, 11)) 30
    else if (m == 2) 29
    else 31
}

need_initial <- function(input, n) {
  need(input[[paste0("initial", n)]] != "", 
       paste0("Please initial item #", n))
}

greenify <- function(n) {
  addClass(class = "green", selector = paste0(".nav li:nth-child(", n, ") a"))
}
