get_percent <- function(value, invert_colors = NULL) {
	if (length(value) == 0) {
		return(HTML("<span class='percent NA'> Data not available </span>"))
	}
	
	value <- round(value, digits = 2)
	
	if (value > 0) {
		CSS_class <- "percent positive"
		sign <- "+"
	} else if (value < 0) {
		CSS_class <- "percent negative"
		sign <- ""
	} else {
		CSS_class <- "percent zero"
		sign <- ""
	}
	
	if (!is.null(invert_colors) && invert_colors == TRUE) {
		CSS_class <- paste(CSS_class, "inverted")
	}
	
	glue::glue("<span class='{CSS_class}'> {sign}{value}%</span>") |> HTML()
}
