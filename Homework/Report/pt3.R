library(rsm)

# Create the data frame
data <- data.frame(
  Run = 1:16,
  A = c(2.5, 2.5, 2.5, 2.5, 5, 5, 5, 5, 7.5, 7.5, 7.5, 7.5, 10, 10, 10, 10),
  B = c(54, 105, 205, 329, 54, 105, 205, 329, 54, 105, 205, 329, 54, 105, 205, 329),
  C = c(45, 35, 25, 15, 35, 45, 15, 25, 25, 15, 45, 35, 15, 25, 35, 45),
  D = c(0, 80, 160, 240, 160, 240, 0, 80, 240, 160, 80, 0, 80, 0, 240, 160),
  Y = c(873, 918, 1089, 1420, 1754, 1242, 2483, 1767, 2782, 3255, 2102, 2145, 4298, 4099, 3016, 2497)
)

# Fit the refined response surface model

rsm_model <- rsm(Y ~ (A + C + B:D), data = data)

# Print the summary of the model
print(summary(rsm_model))


# Define a function to generate 3D plots
generate_plot <- function(f1, f2, fixed_factors) {
  # Create a grid of values for f1 and f2
  grid <- expand.grid(seq(min(data[[f1]]), max(data[[f1]]), length.out = 50),
                      seq(min(data[[f2]]), max(data[[f2]]), length.out = 50))
  
  # Name the variables in the grid
  names(grid) <- c(f1, f2)
  
  # Add the fixed factors to the grid
  for (factor in names(fixed_factors)) {
    grid[[factor]] <- fixed_factors[[factor]]
  }
  
  # Add predicted response
  grid$Y <- predict(rsm_model, newdata = grid)
  
  # Reshape the data into a matrix for 3D plot
  grid_matrix <- with(grid, matrix(Y, nrow = length(unique(get(f1))), 
                                   ncol = length(unique(get(f2)))))
  
  # Create the x, y, and z sequences
  x_seq <- seq(min(data[[f1]]), max(data[[f1]]), length.out = 50)
  y_seq <- seq(min(data[[f2]]), max(data[[f2]]), length.out = 50)
  z_seq <- grid$Y
  
  # Set up the layout for the plots
  layout(matrix(c(1, 1), 1, 2, byrow = TRUE), widths = c(1, 1), heights = c(1, 1))
  
  # Create the 3D plot
  persp(x = x_seq, y = y_seq, z = grid_matrix, 
        xlab = f1, ylab = f2, zlab = "Response", 
        main = paste("3D Plot (", f1, ", ", f2, ")", sep = ""),
        ticktype = "detailed")  # Adding detailed tick marks
  
  # Create the 2D contour plot
  filled.contour(x = x_seq, y = y_seq, z = grid_matrix, 
                 xlab = f1, ylab = f2,
                 main = paste("2D Plot (", f1, ", ", f2, ")", sep = ""),
                 color.palette = terrain.colors)
}

# Generate 3D and 2D plots
generate_plot("A", "C", list("B" = mean(data$B), "D" = mean(data$D)))
generate_plot("A", "B", list("C" = mean(data$C), "D" = mean(data$D)))
generate_plot("C", "B", list("A" = mean(data$A), "D" = mean(data$D)))