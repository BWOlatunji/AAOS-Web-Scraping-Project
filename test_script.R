# Function to select an item from dropdown1 and dropdown2 and click the button
search_and_scrape <- function(item1, item2) {
  # Find and select dropdown1
  dropdown1 <- remDr$findElement(using = "id", value = "fourboot_ctl00$mainContentPlaceHolder$DdlStateSpecialty")
  dropdown1$sendKeysToElement(list(state_names[1], key = 'enter'))
  
  # Find and select dropdown2
  dropdown2 <- remDr$findElement(using = "id", value = "fourboot_ctl00$mainContentPlaceHolder$DdlSpecialty")
  dropdown2$sendKeysToElement(list(item2))
  
  # Find and click the search button
  search_button <- remDr$findElement(using = "id", value = "ctl00_mainContentPlaceHolder_BtnSearch")
  search_button$clickElement()
  
  # Wait for some time to ensure the results load
  Sys.sleep(5)
  
  # Retrieve the search results as HTML
  search_results <- remDr$getPageSource()[[1]]
  
  # Parse the HTML to extract the list items
  parsed_results <- read_html(search_results) %>%
    html_nodes("ul.list--pipe-vertical li") %>%
    html_text()
  
  return(parsed_results)
}

# Create an empty data frame to store the results
results_df <- data.frame()

# Loop through the dropdown options and scrape the results
dropdown1_options <- c("Option1", "Option2", "Option3")  # Replace with actual dropdown options
dropdown2_options <- c("OptionA", "OptionB", "OptionC")  # Replace with actual dropdown options

for (item1 in state_names[1]) {
  for (item2 in practice_specialities[1]) {
    results <- search_and_scrape(item1, item2)
    
    # # Create a data frame with the results
    result_df <- data.frame(
      SearchResults = results
    )
    
    # Append the results to the main data frame
    results_df <- bind_rows(results_df, result_df)
  }
}

# Stop the Selenium server and close the browser
remDr$close()
rD$server$stop()
