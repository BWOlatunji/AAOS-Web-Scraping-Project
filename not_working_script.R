# Load RSelenium package
library(RSelenium)
library(rvest)
library(wdman)
library(netstat)
library(tidyverse)


# Create a remote driver session (you may need to specify the path to your ChromeDriver)
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = "116.0.5845.98",
                             port = free_port(),
                             verbose = FALSE)

remDr <- rs_driver_object$client


# Navigate to the webpage
remDr$open()
remDr$navigate('https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.')


aaos_page <- read_html('https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.')

# extract all state names
state_select_tag <- aaos_page |> html_elements("#ctl00_mainContentPlaceHolder_DdlStateSpecialty")

state_names <- state_select_tag |> 
  html_elements("option") |> 
  html_text2()

state_names <- state_names[-1]


# extract all state names
practice_speciality_select_tag <- aaos_page  |> 
  html_elements("#ctl00_mainContentPlaceHolder_DdlSpecialty")

practice_specialities <- practice_speciality_select_tag |> 
  html_elements("option") |> 
  html_text2()

practice_specialities <- practice_specialities[-1]


# Function to perform the search for a given pair of dropdown selections
search_and_extract <- function(dropdown1_value, dropdown2_value) {
  # Select dropdown1 item
  remDr$findElement(using = "id", value = "fourboot_ctl00$mainContentPlaceHolder$DdlStateSpecialty")$clickElement()
  remDr$findElement(using = "xpath", value = paste("//option[text()='", dropdown1_value, "']"))$clickElement()
  
  # Select dropdown2 item
  remDr$findElement(using = "id", value = "fourboot_ctl00$mainContentPlaceHolder$DdlSpecialty")$clickElement()
  remDr$findElement(using = "xpath", value = paste("//option[text()='", dropdown2_value, "']"))$clickElement()
  
  # Click the search button
  remDr$findElement(using = "id", value = "ctl00_mainContentPlaceHolder_BtnSearch")$clickElement()
  
  # Wait for search results to load (modify the timeout as needed)
  Sys.sleep(5)
  
  # Extract search results from the div
  results <- remDr$findElement(using = "xpath", value = "//div[@class='list--pipe-vertical']")$getElementText()
  results <- unlist(strsplit(results, "\n"))
  
  return(results)
}

# Define the combinations for dropdown1 and dropdown2
dropdown1_values <- c("Option1", "Option2", "Option3")  # Modify with your actual options
dropdown2_values <- c("ItemA", "ItemB", "ItemC")  # Modify with your actual options

# Use purrr to iterate through combinations and extract results
results_list <- pmap(list(state_names[1:2], practice_specialities[1]), search_and_extract)

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





# get_info <- function(state_names, practice_specialities) {
#   result_output = list()
#   for(s in 1:length(state_names)) {
#     
#     remDr$refresh()
#     Sys.sleep(1)
#     
#     state_element <- remDr$findElement(using = 'id', value = "fourboot_ctl00$mainContentPlaceHolder$DdlStateSpecialty")
#     
#     state_element$sendKeysToElement(list(state_names[s]))
#     
#     for (p in 1:length(practice_specialities)) {
#       
#       practice_element <- remDr$findElement(using = 'id', value = "fourboot_ctl00$mainContentPlaceHolder$DdlSpecialty")
#       
#       practice_element$sendKeysToElement(list(practice_specialities[p]))
#     }
#     
#     button_element <- remDr$findElement(using = 'id', value = "ctl00_mainContentPlaceHolder_BtnSearch")
#     
#     button_element$clickElement()
#     Sys.sleep(3)
#     
#     # result ul
#     result_output <- remDr$findElements(using = "class", value = "box--list")
#     
#     # result_values <- lapply(result_output, function (x) x$getElementText()) |> 
#     #   unlist()
#     
#   }
#   
#   return(result_output)
# }
# 
# 
# vector_out <- get_info(state_names[1:2], practice_specialities[1])
# 
# # change audi to whatever your option value is
# option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'audi']")
# option$clickElement()
# 
# # <button class="btn dropdown-toggle selectpicker form-control btn-outline-dark" type="button" id="fourboot_ctl00$mainContentPlaceHolder$DdlStateSpecialty" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">Alaska</button>
# # 
# # <button class="btn dropdown-toggle selectpicker form-control btn-outline-dark" type="button" id="fourboot_ctl00$mainContentPlaceHolder$DdlSpecialty" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">Adult Reconstructive Orthopaedic Surgery</button>
# #   
# # <input type="submit" name="ctl00$mainContentPlaceHolder$BtnSearch" value="Search" onclick="javascript:WebForm_DoPostBackWithOptions(new WebForm_PostBackOptions(&quot;ctl00$mainContentPlaceHolder$BtnSearch&quot;, &quot;&quot;, true, &quot;&quot;, &quot;&quot;, false, false))" id="ctl00_mainContentPlaceHolder_BtnSearch" class="btn btn--solid">  
# #   
