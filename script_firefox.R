# Load required libraries
library(RSelenium)
library(purrr)
library(dplyr)

# Start a Selenium server and create a remote driver
driver <- rsDriver(browser = "firefox")
remDr <- driver[["client"]]

# Navigate to the web page
url <- 'https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.'
remDr$navigate(url)


# Function to perform the search for a given pair of dropdown selections
search_and_extract <- function(dropdown1_value, dropdown2_value) {
  # Select dropdown1 item
  remDr$findElement(using = "id", value = "dropdown1")$clickElement()
  remDr$findElement(using = "xpath", value = paste("//option[text()='", dropdown1_value, "']"))$clickElement()
  
  # Select dropdown2 item
  remDr$findElement(using = "id", value = "dropdown2")$clickElement()
  remDr$findElement(using = "xpath", value = paste("//option[text()='", dropdown2_value, "']"))$clickElement()
  
  # Click the search button
  remDr$findElement(using = "id", value = "btnSearch")$clickElement()
  
  # Wait for search results to load (modify the timeout as needed)
  Sys.sleep(5)
  
  # Extract search results from the div
  results <- remDr$findElement(using = "xpath", value = "//div[@id='searchResults']")$getElementText()
  results <- unlist(strsplit(results, "\n"))
  
  return(results)
}

# Define the combinations for dropdown1 and dropdown2
dropdown1_values <- c("Option1", "Option2", "Option3")  # Modify with your actual options
dropdown2_values <- c("ItemA", "ItemB", "ItemC")  # Modify with your actual options

# Use purrr to iterate through combinations and extract results
results_list <- pmap(list(dropdown1_values, dropdown2_values), search_and_extract)

# Close the Selenium driver
remDr$close()
driver$server$stop()

# Combine the results into a data frame
results_df <- data.frame(
  Dropdown1 = rep(dropdown1_values, each = length(dropdown2_values)),
  Dropdown2 = rep(dropdown2_values, length(dropdown1_values)),
  SearchResult = unlist(results_list),
  stringsAsFactors = FALSE
)

# Print or save the results data frame
print(results_df)
