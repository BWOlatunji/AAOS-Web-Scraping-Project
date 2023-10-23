

collect_data <- function(state_item, practice_item){
  # assign the class name required
  preferred_class <- "disabled"
  
  # Create an empty data frame to store the results
  results_df <- data.frame()
  
  pagination_next <- tryCatch(
    # try searching for the ul for the pagination
    remDr$findElement(using = "class name", "pagination"),
    error = function(e) NULL
  )
  
  if (is.null(pagination_next)) {
    # If either of the elements doesn't exist, scrape and return the table
    results <- search_and_scrape(state_item, practice_item)
    
    # Append the results to the main data frame
    results_df <- bind_rows(results_df, results)
    
    print("One-paged result job completed!")
  } else {
    while (TRUE) {
      results <- search_and_scrape(item1, item2)
      
      # Append the results to the main data frame
      results_df <- bind_rows(results_df, results)
      
      # Find the li HTML element by its ID selector
      li_pagination_lastpage <- remDr$findElement(using = 'xpath', '//*[@id="ctl00_mainContentPlaceHolder_liLastPage"]')
      
      # Check if the preferred class name is present
      if (preferred_class %in% as.character(str_split(unlist(li_pagination_lastpage$getElementAttribute("class")),"\\s+",simplify = T))) {
        # Do something when the class is found
        print("Preferred class found! Multi-paged result completed!")
        break
      } else {
        # Click on the link to potentially load new content
        next_button <- remDr$findElement(using = 'xpath', '//*[@id="ctl00_mainContentPlaceHolder_LinkButtonNextPage"]')
        next_button$clickElement()
        print("Next Button clicked")
        # Wait for some time to allow the new content to load
        Sys.sleep(10) 
      }
    }
  }
  
  return(results_df)
}

