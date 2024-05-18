df
library(dplyr)
library(fda)
library(ggplot2)
library(ggplotly)

df_list <- df %>%
  group_by(Rok) %>%
  group_split()

# Remove the first two columns from each dataframe in the list
df_list <- lapply(df_list, function(x) x[, "Wskaznik", drop = FALSE])

# Define the B-spline basis parameters
n_basis <- 6  # Number of basis functions, adjust as needed
range_val <- c(1, 12)  # Range of the data (months 1 to 12)

# Function to create B-spline basis
create_bspline_basis <- function(data, n_basis, range_val) {
  # Ensure the 'Wskaznik' column is numeric
  somedata <- as.numeric(data$Wskaznik)

  # Create a B-spline basis
  basis <- create.bspline.basis(rangeval = range_val, nbasis = n_basis)
  
  # Fit the B-spline basis to the data
  smooth_basis <- smooth.basis(argvals = 1:12, y = somedata, fdParobj = basis)
  
  return(smooth_basis)
}

# Apply the function to each dataframe in df_list
bspline_list <- lapply(df_list, create_bspline_basis, n_basis, range_val)

# Create a function to plot the B-spline basis
plot_bspline <- function(smooth_basis, year) {
  # Generate a sequence of values to evaluate the smooth curve
  eval_points <- seq(1, 12, length.out = 100)
  
  # Evaluate the smooth curve at the generated points
  smooth_values <- eval.fd(eval_points, smooth_basis$fd)
  
  # Create a dataframe for plotting
  plot_data <- data.frame(
    M.c = eval_points,
    Wskaznik = as.vector(smooth_values),
    Rok = year
  )
  
  return(plot_data)
}

# Generate plot data for each year
plot_data_list <- lapply(unique(df$Rok), function(year) {
  year_df <- df[df$Rok == year, ]  # Filter dataframe for the current year
  smooth_basis <- create_bspline_basis(year_df, n_basis, range_val)  # Create basis for the current year
  plot_bspline(smooth_basis, year)  # Plot data for the current year
})

# Combine all plot data into one dataframe
plot_data <- do.call(rbind, plot_data_list)

# Plot using ggplot2
ggplotly(ggplot(plot_data, aes(x = M.c, y = Wskaznik, color = factor(Rok))) +
  geom_line(size = 1) +
  labs(title = "B-spline Smoothed Data by Year", x = "Month", y = "Smoothed Values") +
  theme_minimal() +
  scale_color_discrete(name = "Year"))
