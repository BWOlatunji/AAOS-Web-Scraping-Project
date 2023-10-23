library(wdman)
library(RSelenium)

chromeCommand <- chrome(retcommand = T,
                        verbose = F,
                        check = F)

chromeCommand

binman::list_versions("chromedriver")
