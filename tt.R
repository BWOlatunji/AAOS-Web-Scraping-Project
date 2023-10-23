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

# dropdown1 <- remDr$findElement(using = "id", value = "fourboot_ctl00$mainContentPlaceHolder$DdlStateSpecialty")
# dropdown1$sendKeysToElement(list(state_names[3]))
# 
# # Find and select dropdown2
# dropdown2 <- remDr$findElement(using = "id", value = "fourboot_ctl00$mainContentPlaceHolder$DdlSpecialty")
# dropdown2$sendKeysToElement(list(item2))



get_member_name <- function(ps_names) {
  final <- c()
  for(i in 1:length(ps_names)) {
    Sys.sleep(5)
    
    ps_element <- remDr$findElement(using = 'xpath', '//*[@id="fourboot_ctl00$mainContentPlaceHolder$DdlSpecialty"]')
    
    ps_element$sendKeysToElement(list(ps_names[i]))
    
    button_element <- remDr$findElement(using = 'id', value = "ctl00_mainContentPlaceHolder_BtnSearch")
    
    button_element$clickElement()
    Sys.sleep(3)
    
    # out <- remDr$findElements(using = "class", value = "memberLabelName")
    # output <- out$getElementText()
    # extract all state names
    labelname_tag <- aaos_page |> html_elements("li")
    
    member_names <- labelname_tag |> 
      html_elements("strong") |> 
      html_text2()
    final <- c(final, member_names)
    
  }
  
  return(final)
}


vector_out <- get_member_name(practice_specialities)

tibble::tibble(
  name = starwars_characters %>% html_element("b") %>% html_text2(),
  species = starwars_characters %>% html_element("i") %>% html_text2(),
  weight = starwars_characters %>% html_element(".weight") %>% html_text2()
)