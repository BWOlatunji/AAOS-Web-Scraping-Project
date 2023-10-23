# Load packages
library(RSelenium)
library(rvest)
library(wdman)
library(netstat)
library(tidyverse)


# remote driver session
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = "116.0.5845.98",
                             port = free_port(),
                             verbose = FALSE)

remDr <- rs_driver_object$client


# Navigate to the webpage
remDr$open()
remDr$navigate('https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.')

# Using rvest, navigate to the web page
aaos_page <- read_html('https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.')

# extract all state names
state_select_tag <- aaos_page |> html_elements("#ctl00_mainContentPlaceHolder_DdlStateSpecialty")

state_names <- state_select_tag |> 
  html_elements("option") |> 
  html_text2()
# remove the first element i.e. "Select a State"
state_names <- state_names[-1]


# extract all state names
practice_specialty_select_tag <- aaos_page  |> 
  html_elements("#ctl00_mainContentPlaceHolder_DdlSpecialty")

practice_specialties <- practice_specialty_select_tag |> 
  html_elements("option") |> 
  html_text2()

# remove the first element i.e. "Select a Practice Specialty"
practice_specialties <- practice_specialties[-1]

# combine the state names and practice specialties as a data frame
# Repeat each practice specialty for each state
aaos_data <- tibble(
  state_name = rep(state_names, each = length(practice_specialties)),
  practice_specialty = rep(practice_specialties, times = length(state_names))
)

# Function to select an item from dropdown1 and dropdown2 and click the button
search_and_scrape <- function(state_item, practice_item) {
  ## Search begins
  # Identify the state dropdown element using its HTML attributes (e.g., id)
  state_dropdown <- remDr$findElement(using = 'xpath', '//*[@id="fourboot_ctl00$mainContentPlaceHolder$DdlStateSpecialty"]')
  
  # Click on the dropdown to open it
  state_dropdown$clickElement()
  
  # Locate and click the desired item in the dropdown
  state_to_select <- state_item  # Replace with the text of the item you want to select
  state_xpath <- paste0("//button[text()='", state_to_select, "']")
  state_element <- remDr$findElement(using = "xpath", value = state_xpath)
  state_element$clickElement()
  
  # Identify the practice dropdown element using its HTML attributes (e.g., id)
  practice_dropdown <- remDr$findElement(using = 'xpath', '//*[@id="fourboot_ctl00$mainContentPlaceHolder$DdlSpecialty"]')
  
  # Click on the dropdown to open it
  practice_dropdown$clickElement()
  
  # Locate and click the desired item in the dropdown
  practice_to_select <- practice_item  # Replace with the text of the item you want to select
  practice_xpath <- paste0("//button[text()='", practice_to_select, "']")
  practice_element <- remDr$findElement(using = "xpath", value = practice_xpath)
  practice_element$clickElement()
  
  # Find and click the search button
  search_button <- remDr$findElement(using = 'xpath', value ='//*[@id="ctl00_mainContentPlaceHolder_BtnSearch"]')
  search_button$clickElement()
  
  ## Search ends
  
  ## Extract Number of result pages returned
  # First, find the h3 tag containing the text - "results"
  h3_elements <- remDr$findElements(using="tag name", value = "h3")
  # Loop through h3 elements to find the one with "results" in its text
  numeric_value <- 0
  for (h3_element in h3_elements) {
    h3_text <- h3_element$getElementText()
    if (grepl("results", h3_text)) {
      # Extract the numeric value from the H3 text
      numeric_value <- as.numeric(gsub("[^0-9.]", "", h3_text))
      break  # Exit the loop once a match is found
    }
  }
  
  # Define the number of pages you want to scrape
  num_pages <- round(numeric_value/20)  
  
  # Initialize an empty list to store the scraped data
  results_df <- data.frame()
  # assign the class name required
  preferred_class <- "disabled"
  if(num_pages<=1){
    # Retrieve the search results as HTML
    search_results <- remDr$getPageSource()[[1]]
    ## Collect and save data as tibble
    # Extract data from the current page 
    result_tbl <- tibble::tibble(
      memberName_results = read_html(search_results)  |> 
        html_elements("ul.list--pipe-vertical li.box--list h3 strong.memberLabelName")  |> 
        html_text2(),
      memberStatus_results = read_html(search_results)  |> 
        html_elements("ul.list--pipe-vertical li.box--list p")  |> 
        html_text2(),
      memberAddress_results = read_html(search_results)  |> 
        html_elements("ul.list--pipe-vertical li.box--list") |> 
        html_elements("div.row.block--colored.mt-2.mx-0") |> 
        html_elements("div.col-md-6") |> 
        html_elements("div.py-2") |> 
        html_text2(),
      memberAllAddress_results = read_html(search_results)  |> 
        html_elements("ul.list--pipe-vertical li.box--list") |> 
        html_elements("div.row.block--colored.mt-2.mx-0") |> 
        html_elements("div.col-md-6:nth-child(1)") |> 
        html_text2(),
      memberPractice_results = read_html(search_results)  |> 
        html_elements("ul.list--pipe-vertical li.box--list") |> 
        html_elements("div.row.block--colored.mt-2.mx-0") |> 
        html_elements("div.col-md-6:nth-child(2)") |> 
        html_text2()
    )
    
    # add code to add the state and practice specialty to the tibble
    result_tbl <- result_tbl |> add_column(state_name         = rep(state_to_select, each = nrow(result_tbl)), 
                                           practice_specialty = rep(practice_to_select, each = nrow(result_tbl)), 
                                           .before = "memberName_results")
    
    
    # Append the results to the main data frame
    results_df <- bind_rows(results_df, result_tbl)
    # Do something when the data has been saved successfully
    print("One-paged result completed!")
    # Go back to main search page
    remDr$navigate('https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.')
    Sys.sleep(5)
  } else{
    for (page in 1:num_pages) {
      
      # Retrieve the search results as HTML
      search_results <- remDr$getPageSource()[[1]]
      ## Collect and save data as tibble
      # Extract data from the current page 
      result_tbl <- tibble::tibble(
        memberName_results = read_html(search_results)  |> 
          html_elements("ul.list--pipe-vertical li.box--list h3 strong.memberLabelName")  |> 
          html_text2(),
        memberStatus_results = read_html(search_results)  |> 
          html_elements("ul.list--pipe-vertical li.box--list p")  |> 
          html_text2(),
        memberAddress_results = read_html(search_results)  |> 
          html_elements("ul.list--pipe-vertical li.box--list") |> 
          html_elements("div.row.block--colored.mt-2.mx-0") |> 
          html_elements("div.col-md-6") |> 
          html_elements("div.py-2") |> 
          html_text2(),
        memberAllAddress_results = read_html(search_results)  |> 
          html_elements("ul.list--pipe-vertical li.box--list") |> 
          html_elements("div.row.block--colored.mt-2.mx-0") |> 
          html_elements("div.col-md-6:nth-child(1)") |> 
          html_text2(),
        memberPractice_results = read_html(search_results)  |> 
          html_elements("ul.list--pipe-vertical li.box--list") |> 
          html_elements("div.row.block--colored.mt-2.mx-0") |> 
          html_elements("div.col-md-6:nth-child(2)") |> 
          html_text2()
      )
      
      # add code to add the state and practice specialty to the tibble
      result_tbl <- result_tbl |> add_column(state_name         = rep(state_to_select, each = nrow(result_tbl)), 
                                             practice_specialty = rep(practice_to_select, each = nrow(result_tbl)), 
                                             .before = "memberName_results")
      
      
      # Append the results to the main data frame
      results_df <- bind_rows(results_df, result_tbl)
      
      # Find the li HTML element by its ID selector
      li_pagination_lastpage <- remDr$findElement(using = 'xpath', '//*[@id="ctl00_mainContentPlaceHolder_liLastPage"]')
      
      # Check if the preferred class name is present
      if (preferred_class %in% as.character(str_split(unlist(li_pagination_lastpage$getElementAttribute("class")),"\\s+",simplify = T))) {
        # Do something when the class is found
        print("Preferred class found! Multi-paged result completed!")
        # Go back to main search page
        remDr$navigate('https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.')
        Sys.sleep(5)
        break
      } else {
        # Click on the link to potentially load new content
        next_button <- remDr$findElement(using = 'id', value = "ctl00_mainContentPlaceHolder_LinkButtonNextPage")
        next_button$clickElement()
        print("Next Button clicked")
        # Wait for some time to allow the new content to load
        Sys.sleep(5) 
      }
    }
  }
  #write_rds(results_df, "data/texas2.rds")
  return(results_df)
}

alaska_data <- search_and_scrape(state_item = state_names[2],
                                practice_item = practice_specialties[1])

write_rds(alaska_data, "data/alaska.rds")

