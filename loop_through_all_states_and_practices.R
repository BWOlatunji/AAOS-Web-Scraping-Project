library(tidyverse)

# Define your custom function
my_function <- function(x, y) {
  # Your custom logic here
  result <- x + y  # Example: Adding the values of the two columns
  return(result)
}

# Loop through each row of the data frame
for (i in 1:nrow(my_data)) {
  result <- my_function(my_data[i, "column1"], my_data[i, "column2"])
  results <- c(results, result)
}


