days_in_month <- function(month) {
	m <- which(month.name == month)
	if (m %in% c(4, 6, 9, 11)) 30
    else if (m == 2) 29
    else 31
}

# page_valid_to_green <- function(page) {
#   validation = paste0("validation", page)
#   selector = paste0(".nav li:nth-child(", page, ") a")
# 	observe({
#     if (is.null(do.call(validation, list()))) {
#       shinyjs::addClass(class = "green", selector = selector)
#     }
# }