# Set Km values
Km <- seq(from = 5, to = 50, by = 5)
Vmax <- 1
s <- seq(from = 0, to = 100, by = 0.1)

# Create empty plot
plot(s,rep(NA, length(s)), type = "l", xlim = c(0, 100), ylim = c(0, Vmax), xlab = "s", ylab = "v(s)")

# Add curves for each Km value
for (k in Km) {
  v <- (Vmax * s) / (k + s)
  lines(x = s, y = v, col = rainbow(length(Km))[which(Km == k)])
}
legend(x = "topright", legend = Km, col = rainbow(length(Km)), lty = 1)
