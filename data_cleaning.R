library(tidyverse)

texas_data_raw <- read_rds("data/texas.rds")
texas_data_cleaned <- texas_data_raw |> 
  mutate(Status = memberStatus_results |> 
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
           str_remove(pattern = "Fax: ")) |> 
  select(memberName_results,Status,practice_specialty,
         Address, Phone, Fax, Year, Date) |> 
  set_names(c("Name","Member Status","Practice Specialty",
              "Address","Phone", "Fax", "Year", "Date and timestamp"))

write_csv(texas_data_raw, "data/csv/texas_data_raw.csv")

