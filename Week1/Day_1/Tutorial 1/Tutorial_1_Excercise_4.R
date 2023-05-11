#parameter values
a <- 0.221363
b <- 1.229524
K <- 0.095948
t <- seq(from = 0, to = 10, by = 0.01)

y <- function(t) {
  a * t^b * exp(-K * t)
}

# Plot function
plot(t, y(t), type = "l", xlab = "t", ylab = "y(t)", main = "Function y(t) = at^b e^(-Kt)")

# Add vertical line
abline(v = b/K, col = "red")

# Save plot as JPEG file
jpeg(filename = "y-function-plot.jpg", quality = 100)
plot(t,y(t), type = "l", xlab = "t", ylab = "y(t)", main = "Function y(t) = at^b e^(-Kt)")
abline(v = b/K, col = "red")
dev.off()

# Compute integral
result <- integrate(y, lower = 0, upper = 100)
cat("computed integral", result$value)