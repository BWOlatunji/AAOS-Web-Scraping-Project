# Install packages if not already installed
if (!requireNamespace("rvest", quietly = TRUE)) {
  install.packages("rvest")
}

if (!requireNamespace("RSelenium", quietly = TRUE)) {
  install.packages("RSelenium")
}

if (!requireNamespace("purrr", quietly = TRUE)) {
  install.packages("purrr")
}

# Load the required libraries
# Load packages
library(RSelenium)
library(rvest)
library(wdman)
library(netstat)
library(tidyverse)

# Set up a remote driver using Chrome
# remote driver session
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = "119.0.6045.105",
                             port = free_port(),
                             verbose = FALSE)

remDr <- rs_driver_object$client
# Open the webpage
#remDr$open()
remDr$navigate('https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.')

# Get the HTML content of the webpage
html <- remDr$getPageSource()[[1]]

# Close the remote driver
#remDr$close()

# Parse the HTML content
html_parsed <- read_html(html)

# Select the ".options-list" nodes
options_lists <- html_parsed %>%
  html_elements(".options-list")

# Iterate through each ".options-list" node
result <- map(options_lists, function(options_list) {
  # Extract button elements within each ".options-list" node
  buttons <- options_list %>%
    html_elements("button")
  
  # Extract text from each button element
  button_texts <- buttons %>%
    map_chr(~ html_text(.))
  
  # Create a tibble with the extracted text
  tibble(button_text = button_texts)
})

# Print the resulting tibbles
#print(result)

state_Specialty <- result[[2]] |> set_names("state_name") |> slice(-1) |> pull()
practice_specialty <- result[[3]] |> set_names("specialty") |> slice(-1) |> pull()


# combine the state names and practice specialties as a data frame
# Repeat each practice specialty for each state
state_and_practice_specialty_tbl <- tibble(
  state_name = rep(state_Specialty, each = length(practice_specialty)),
  practice_specialty = rep(practice_specialty, times = length(state_Specialty))
)





# Stop the Selenium server
remDr$close()
