# Constants --------------------------------------------------
NUM_ROWS <- 50
NUM_COLS <- 70
COLORS <- c("midnightblue", rgb(17, 14, 66, maxColorValue=255), 
			rgb(7, 5, 41, maxColor=255), "black")

# Color function ---------------------------------------------
get_color <- function(row, col) {
	x <- 1 - row/NUM_ROWS
	y <- 1 - col/NUM_COLS
	dist <- sqrt(x*x + y*y)
	
	mean <- 2.75*dist*length(COLORS)
	index <- rchisq(n=1, df=mean)
	index <- max(1, index)
	index <- min(index, length(COLORS))
	return (COLORS[index])
}

# Plotting ---------------------------------------------------
par(bg="white")
plot(0, 0,
	 xlim=c(0, NUM_COLS), ylim=c(0, NUM_ROWS),
	 axes=F, xlab="", ylab="", type="n")
for (r in 0:(NUM_ROWS-1)) {
	for (c in 0:(NUM_COLS-1)) {
		color <- get_color(r, c)
		x <- c(c, c, c+1, c+1)
		y <- c(r, r+1, r, r+1)
		rect(c, r, c+1, r+1,
			 density=NA, col=color, border=NA)
	}
}
