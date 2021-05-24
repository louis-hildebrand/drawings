library(plotrix)

# Constants --------------------------------------------------
NUM_ROWS <- 25
NUM_COLS <- 35
UP_COLOR <- "cyan"
DOWN_COLOR <- "magenta"
LWD <- 4

# Plotting function ------------------------------------------
fill_cell <- function(row, col) {
	is_principal_diagonal <- (row+col) %% 2 == 0
	is_up <- sample(c(T, F), size=1)
	
	color <- ifelse(is_up, UP_COLOR, DOWN_COLOR)
	if (is_principal_diagonal && is_up) {
		center_x <- col
		center_y <- row
		deg1 <- 0
		deg2 <- 90
	} else if (is_principal_diagonal && !is_up) {
		center_x <- col + 1
		center_y <- row + 1
		deg1 <- 180
		deg2 <- 270
	} else if (!is_principal_diagonal && is_up) {
		center_x <- col + 1
		center_y <- row
		deg1 <- 90
		deg2 <- 180
	} else {
		center_x <- col
		center_y <- row + 1
		deg1 <- 270
		deg2 <- 360
	}
	
	draw.arc(x=center_x, y=center_y, deg1=deg1, deg2=deg2,
			 col=color, lwd=LWD)
}

# Plotting ---------------------------------------------------
par(bg="black")
plot(0, 0, 
	 xlim=c(0, NUM_COLS), ylim=c(0, NUM_ROWS),
	 axes=F, xlab="", ylab="", type="n")
for (r in 0:NUM_ROWS) {
	for (c in 0:NUM_COLS) {
		fill_cell(r, c)
	}
}
