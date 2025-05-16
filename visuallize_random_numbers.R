visualize_prng <- function(random_numbers, title = "PRNG Scatter Plot") {
  # Check if the input is valid
  if (length(random_numbers) < 2) {
    stop("The random number vector must contain at least two elements.")
  }
  
  # Create pairs of consecutive numbers
  x <- random_numbers[1:(length(random_numbers) - 1)]
  y <- random_numbers[2:length(random_numbers)]
  
  # Scatter plot of consecutive pairs
  plot(x, y, main = title, xlab = "x[i]", ylab = "x[i+1]",
       pch = 16, col = rgb(0.2, 0.4, 0.6, 0.7))
}


middle_square <- function(seed, n) {
  # Ensure the seed is a positive integer
  if (!is.numeric(seed) || seed <= 0 || floor(seed) != seed) {
    stop("Seed must be a positive integer.")
  }
  
  # Initialize the result vector
  random_numbers <- numeric(n)
  current_number <- seed
  
  for (i in 1:n) {
    # Square the current number
    squared <- current_number^2
    
    # Convert the squared number to a string
    squared_str <- as.character(squared)
    
    # Ensure the squared string has at least 8 digits (pad with zeros)
    if (nchar(squared_str) < 8) {
      squared_str <- sprintf("%08d", squared)
    }
    
    # Extract the middle 4 digits
    len <- nchar(squared_str)
    mid_start <- floor((len - 4) / 2) + 1
    middle_digits <- substr(squared_str, mid_start, mid_start + 3)
    
    # Update the current number and normalize to [0, 1]
    current_number <- as.numeric(middle_digits)
    random_numbers[i] <- current_number / 10000
  }
  
  return(random_numbers)
}

# Example usage
b <- middle_square(seed = 176543, n = 100000)
visualize_prng(b)


set.seed(123)
random_numbers <- runif(100000)
visualize_prng(random_numbers, title = "Built-in R PRNG: Animated Consecutive Pairs")
