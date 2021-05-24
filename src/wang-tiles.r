# Constants --------------------------------------------------
NUM_ROWS <- 25
NUM_COLS <- 35
COLORS <- c("black", "midnightblue", "mediumorchid4")
COLORS_PROB <- c(3, 3, 1)

# State ------------------------------------------------------

# Save the tile colors in an array. For a given tile, store 
# the colors counterclockwise starting from the right.
tile_colors <- array(dim=c(NUM_ROWS, NUM_COLS, 4))

# Filling functions ------------------------------------------
draw_tile <- function(row, col) {
	# Right
	color <- choose_tile_color(row, col, 1)
	draw_tile_section(row, col, 1, color)
	tile_colors[row + 1, col + 1, 1] <- color
	
	# Top
	color <- choose_tile_color(row, col, 2)
	draw_tile_section(row, col, 2, color)
	tile_colors[row + 1, col + 1, 2] <- color
	
	# Left
	color <- choose_tile_color(row, col, 3)
	draw_tile_section(row, col, 3, color)
	tile_colors[row + 1, col + 1, 3] <- color
	
	# Bottom
	color <- choose_tile_color(row, col, 4)
	draw_tile_section(row, col, 4, color)
	tile_colors[row + 1, col + 1, 4] <- color
	
	# Update tile_colors
	assign("tile_colors", tile_colors, envir=.GlobalEnv)
}

choose_tile_color <- function(row, col, side) {
	# Find neighbor
	if (side == 1) {
		neighbor_row <- row
		neighbor_col <- col + 1
		neighbor_side <- 3
	} else if (side == 2) {
		neighbor_row <- row + 1
		neighbor_col <- col
		neighbor_side <- 4
	} else if (side == 3) {
		neighbor_row <- row
		neighbor_col <- col - 1
		neighbor_side <- 1
	} else {
		neighbor_row <- row - 1
		neighbor_col <- col
		neighbor_side <- 2
	}
	
	# Get neighboring color
	if (!is_in_bounds(neighbor_row, neighbor_col)) {
		return(sample(COLORS, size=1, prob=COLORS_PROB))
	} else {
		neighbor_color <- tile_colors[neighbor_row + 1, 
									  neighbor_col + 1,
									  neighbor_side]
		color <- ifelse(is.na(neighbor_color),
							  sample(COLORS, size=1, prob=COLORS_PROB),
							  neighbor_color)
		return(color)
	}
}

is_in_bounds <- function(row, col) {
	row >= 0 && row < NUM_ROWS  && col >= 0 && col < NUM_COLS
}

draw_tile_section <- function(row, col, side, color) {
	# Get triangle bounds
	if (side == 1) {
		x <- c(col + 1, col + 0.5, col + 1)
		y <- c(row, row + 0.5, row + 1)
	} else if (side == 2) {
		x <- c(col, col + 0.5, col + 1)
		y <- c(row + 1, row + 0.5, row + 1)
	} else if (side == 3) {
		x <- c(col, col + 0.5, col)
		y <- c(row, row + 0.5, row + 1)
	} else if (side == 4) {
		x <- c(col, col + 0.5, col + 1)
		y <- c(row, row + 0.5, row)
	}
	
	# Draw triangle
	polygon(x, y, col=color, density=NA, border=NA)
}

# Plotting ---------------------------------------------------
par(bg="white")
plot(0, 0, 
	 xlim=c(0, NUM_COLS), ylim=c(0, NUM_ROWS),
	 axes=F, xlab="", ylab="", type="n")
for (r in 0:(NUM_ROWS-1)) {
	for (c in 0:(NUM_COLS-1)) {
		draw_tile(r, c)
	}
}
