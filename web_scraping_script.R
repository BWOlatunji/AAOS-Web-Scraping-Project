# libraries
library(tidyverse)
library(rvest)
library(RSelenium)
library(wdman)
library(netstat)

# 8) URL where the data was found
# 9) Date and time stamp from time it was scraped

rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = "116.0.5845.98",
                             port = free_port(),
                             verbose = FALSE)
remDr <- rs_driver_object$client
remDr$open()

remDr$maxWindowSize()

remDr$navigate('https://www7.aaos.org/member/directory/search.aspx?directory=public&_ga=2.25343037.1215811434.1696392931-892052855.1696392931.')

remDr$findElement(using = 'class name', 'button')

