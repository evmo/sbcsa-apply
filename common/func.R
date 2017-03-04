days_in_month <- function(month) {
	m <- which(month.name == month)
	if (m %in% c(4, 6, 9, 11)) 30
    else if (m == 2) 29
    else 31
}