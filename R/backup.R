# Load packages
library(RSelenium)
library(rvest)
library(wdman)
library(netstat)
library(tidyverse)

# remote driver session
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = "118.0.5993.70",
                             port = free_port(),
                             verbose = FALSE)

remDr <- rs_driver_object$client
# remDr$open() # optional since the driver has been declared above
# Navigate to the webpage
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
aaos_state_and_practice_specialty_tbl <- tibble(
  state_name = rep(state_names, each = length(practice_specialties)),
  practice_specialty = rep(practice_specialties, times = length(state_names))
)


search_and_scrape <- function(state_item, practice_item) {
  ## Search begins
  # Identify the state dropdown element using its HTML attributes (e.g. id)
  state_dropdown <-
    remDr$findElement(using = 'xpath',
                      '//*[@id="fourboot_ctl00$mainContentPlaceHolder$DdlStateSpecialty"]')
  
  # the dropdown is clicked to open it
  state_dropdown$clickElement()
  
  # assign the state_item argument value to this variable
  state_to_select <- state_item
  
  # use the state argument value in the xpath statement taht will be used to
  # select the desired state value
  state_xpath <- paste0("//button[text()='", state_to_select, "']")
  
  # remote driver finds the element based on the xpath
  state_element <-
    remDr$findElement(using = "xpath", value = state_xpath)
  
  # Now click the selected state
  state_element$clickElement()
  
  # Identify the practice dropdown element using its HTML attributes (e.g., id)
  practice_dropdown <-
    remDr$findElement(using = 'xpath',
                      '//*[@id="fourboot_ctl00$mainContentPlaceHolder$DdlSpecialty"]')
  
  # the dropdown is clicked to open it
  practice_dropdown$clickElement()
  
  # assign the practice_item argument value to this variable
  practice_to_select <- practice_item
  
  # use the practice argument value in the xpath statement taht will be used to
  # select the desired practice specialty value
  practice_xpath <-
    paste0("//button[text()='", practice_to_select, "']")
  
  # remote driver finds the element based on the xpath
  practice_element <-
    remDr$findElement(using = "xpath", value = practice_xpath)
  # Now click the selected state
  practice_element$clickElement()
  
  # remote driver finds the element based on the xpath click the search button
  search_button <-
    remDr$findElement(using = 'xpath', value = '//*[@id="ctl00_mainContentPlaceHolder_BtnSearch"]')
  
  search_button$clickElement()
  
  ## Search ends
  
  ## Extract Number of results returned
  
  # First, find the h3 tag containing the text - "results". This h3 tag contains the total number of results returned plus the text - "results"
  h3_elements <- remDr$findElements(using = "tag name", value = "h3")
  
  # variable to save the numeric value contained in the h3 tag text
  numeric_value <- 0
  # Loop through h3 elements on the web page to find the one containing     "results" in its text
  for (h3_element in h3_elements) {
    h3_text <- h3_element$getElementText()
    if (grepl("results", h3_text)) {
      # Extract the numeric value from the h3 tag text
      numeric_value <- as.numeric(gsub("[^0-9.]", "", h3_text))
      break  # Exit the loop once a match is found
    }
  }
  
  # Define the number of pages we want to scrape
  # the maximum results on a page is 20. So, to determine the number of pages,
  # we divide the total results by 20 and round it to the nearest whole number
  # to get the number of pages
  num_pages <- ceiling(numeric_value / 20)
  
  # Initialize an empty data frame to store the scraped data
  results_df <- data.frame()
  
  # assign the class name required that will be used to end the loop   when found
  preferred_class <- "disabled"
  # Some seraches can return "No results found" i.e. NA results
  # Where records are found, the minimum number of page is 1. 
  # So, we check if there is no results returned, 
  # if the number of page is 1,
  # and there are more than 1 page result returned
  if(is.na(num_pages)){
    result = NULL
    results_df = rbind(results_df, result)
    
    # Do something when the data has been saved successfully
    print(paste("No results found for", " state: ",state_item, 
                " practice specialty: ", practice_item,"!"))
    
    # Go back to main search page
    remDr$navigate(
      'https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.'
    )
    Sys.sleep(5)
  } else if(num_pages <= 1) {
    # Retrieve the search results as HTML
    search_results <- remDr$getPageSource()[[1]]
    
    ## Collect and save data as tibble
    # Extract data from the current page
    result_tbl <- tibble::tibble(
      memberName_results = read_html(search_results)  |>
        html_elements(
          "ul.list--pipe-vertical li.box--list h3 strong.memberLabelName"
        )  |>
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
    
    
    # add the state and practice specialty to the tibble
    result_tbl <-
      result_tbl |> add_column(
        state_name         = rep(state_to_select, each = nrow(result_tbl)),
        practice_specialty = rep(practice_to_select, each = nrow(result_tbl)),
        .before = "memberName_results"
      )
    
    
    # Append the results to the main data frame
    results_df <- bind_rows(results_df, result_tbl)
    
    # Do something when the data has been saved successfully
    print("One-paged result completed!")
    
    # Go back to main search page
    remDr$navigate(
      'https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.'
    )
    Sys.sleep(5)
  } else{
    for (page in 1:num_pages) {
      
      # Retrieve the search results as HTML
      search_results <- remDr$getPageSource()[[1]]
      
      ## Collect and save data as tibble
      # Extract data from the current page
      result_tbl <- tibble::tibble(
        memberName_results = read_html(search_results)  |>
          html_elements(
            "ul.list--pipe-vertical li.box--list h3 strong.memberLabelName"
          )  |>
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
      
      # add the state and practice specialty to the tibble
      result_tbl <-
        result_tbl |> add_column(
          state_name         = rep(state_to_select, 
                                   each = nrow(result_tbl)),
          practice_specialty = rep(practice_to_select, 
                                   each = nrow(result_tbl)),
          .before = "memberName_results"
        )
      
      
      # Append the results to the main data frame
      results_df <- bind_rows(results_df, result_tbl)
      
      # Find the li HTML element by its ID selector 
      # i.e. li element that navigates to the last page
      li_pagination_lastpage <-
        remDr$findElement(using = 'xpath',
                          '//*[@id="ctl00_mainContentPlaceHolder_liLastPage"]')
      
      # Check if the preferred class name is present in the li element's class
      # i.e. "disabled"
      if (preferred_class %in% as.character(str_split(
        unlist(li_pagination_lastpage$getElementAttribute("class")),
        "\\s+",
        simplify = T
      ))) {
        # Do something when the class is found
        print("Preferred class found! Multi-paged result completed!")
        # Go back to main search page
        remDr$navigate(
          'https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.'
        )
        Sys.sleep(5)
        break
      } else {
        # find the anchor that navigates to the next page 
        # for multiple paged results
        next_button <-
          remDr$findElement(
            using = 'id',
            value = "ctl00_mainContentPlaceHolder_LinkButtonNextPage")
        
        # click the next button to load the next set of results
        next_button$clickElement()
        
        # print this statement out so we know the button was found and clicked
        print("Next Button clicked")
        
        # Wait for some time to allow the new content to load
        Sys.sleep(5)
      }
    }
  }
  # Save the result as csv file. Please create the folders as referenced below
  write_csv(results_df, paste("data/csv/raw_data/", 
                              str_replace(state_item, 
                                          pattern = " ",
                                          replacement = "_"), 
                              "_", 
                              str_replace(practice_item, 
                                          pattern = " ",
                                          replacement = "_"), 
                              "_data.csv"))
  return(results_df)
}


# For all practice specialty in Alabama state
all_raw_subseted_state_results <- data.frame()
# Loop through each row of the data frame
for (i in 1:nrow(aaos_state_and_practice_specialty_tbl[1:16,])) {
  result <- search_and_scrape(aaos_state_and_practice_specialty_tbl$state_name[i], aaos_state_and_practice_specialty_tbl$practice_specialty[i])
  all_raw_subseted_state_results <- rbind(all_raw_subseted_state_results, result)
}

clean_raw_data <- function(data_tbl) {
  cleaned_data <- data_tbl |>
    mutate(
      Status = memberStatus_results |>
        str_remove(pattern = "Member Status: ") |>
        str_remove(pattern = "\\d+"),
      Year = memberStatus_results |>
        str_extract(pattern = "\\d+") |>
        as.numeric() |>
        replace_na(0),
      Address = memberAddress_results |> str_remove(pattern = "Office:"),
      Phone = memberAllAddress_results |>
        str_extract(pattern = "Office Phone:\\s*\\(\\d+\\)\\d+-\\d+") |>
        str_remove(pattern = "Office Phone: "),
      Fax = memberAllAddress_results |>
        str_extract(pattern = "Fax:\\s*\\(\\d+\\)\\s*\\d+-\\d+"),
      Date = Sys.time() |>
        str_remove(pattern = "Fax: ")
    ) |>
    select(memberName_results,
           Status,
           practice_specialty,
           Address,
           Phone,
           Fax,
           Year,
           Date) |>
    set_names(
      c(
        "Name",
        "Member Status",
        "Practice Specialty",
        "Address",
        "Phone",
        "Fax",
        "Year",
        "Date and timestamp"
      )
    )
  
  return(cleaned_data)
}

all_cleaned_subseted_state_results <- clean_raw_data(data_tbl = all_raw_subseted_state_results)  
# Save the result as csv file. Please create the folders as referenced below
write_csv(all_cleaned_subseted_state_results, "data/csv/cleaned_data/alabama_cleaned_data.csv")







