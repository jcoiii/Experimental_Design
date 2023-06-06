rm()
set.seed(123) 
#3a
library(dplyr)
df <- expand.grid(A = c(-1, 0, 1), B = c(-1, 0, 1))
df <- df %>%
  mutate(Yield = 1 + 2*A + 3*B - A^2 - B^2 - 2*A*B)
  # I assume b0 = 1, b1 = 2, b2 = 3, b3 = b4 = -1, interaction coefficient = -2
print(df)

model <- lm(Yield ~ A + B + I(A^2) + I(B^2) + A:B, data = df)
summary(model)

#3b
library(lattice)
library(akima)
A_vals <- seq(-1, 1, length.out = 100)
B_vals <- seq(-1, 1, length.out = 100)
grid <- expand.grid(A = A_vals, B = B_vals)
grid$Yield <- predict(model, newdata = grid)

# smoother plot interp
interp <- akima::interp(x = grid$A, y = grid$B, z = grid$Yield)

# contour plot
contour_plot <- filled.contour(x = interp$x, y = interp$y, z = interp$z,
                    xlab = "A", ylab = "B",
                    plot.title = title(main = "Contour Plot of Yield"))

# perspective plot
persp_plot <- persp(x = interp$x, y = interp$y, z = interp$z, theta = 45, 
                    phi = 45, xlab = "A", ylab = "B", zlab = "Yield", 
                    main = "Perspective Plot of Yield")

# Combine the two plots in a single panel.
# Still wont work to show side by side
par(mfrow = c(1, 2))
print(persp_plot)  
print(contour_plot)
par(mfrow = c(1, 1))