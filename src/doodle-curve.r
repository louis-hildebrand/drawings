# Constants --------------------------------------------------
X_MAX <- 42
X_MIN <- -X_MAX/2
Y_MAX <- 30
Y_MIN <- -Y_MAX/2
X_RANGE <- seq(from=0, to=X_MAX, by=1)
COLOR <- "royalblue4"
LWD <- 2

# Plotting function ------------------------------------------
f <- function(x) {
	return (Y_MAX*(1-x/X_MAX))
}

# Plotting ---------------------------------------------------
par(bg = "black")
plot(0, 0, 
	 xlim=c(X_MIN, X_MAX), ylim=c(Y_MIN, Y_MAX),
	 axes=F, xlab="", ylab="", type="n")
for (x in X_RANGE) {
	y <- f(x)
	lines(c(x, X_MAX), c(Y_MAX, y),
		  col=COLOR, lwd=LWD)
	lines(c(X_MIN + X_MAX - x, X_MIN), c(Y_MIN, Y_MIN + Y_MAX - y),
		  col=COLOR, lwd=LWD)
}
