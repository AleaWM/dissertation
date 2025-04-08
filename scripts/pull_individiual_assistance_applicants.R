# ----
# Purpose: Pull FEMA Individual Assistance application data for Cook County, including accepted and rejected applications
# Input(s): FEMA Individual Assistance API (two methods implemented)
# Output(s): data/raw/indiv_assistance_CookCounty.csv
# Last updated: 
# ----

# Receiving data in JSON, saving in RDS - a single R object.
library(tidyverse)
library(httr)         # wrapper for curl package - may require installation
library("jsonlite") 



datalist = list()

# Code needed to obtain data on flood insurance claims in IL without the rfema package ------------------

# define the url for the appropriate api end point
base_url <- "https://www.fema.gov/api/open/v1/IndividualsAndHouseholdsProgramValidRegistrations"

# append the base_url to apply filters
filters <- "?$count=true&$filter=(damagedStateAbbreviation%20eq%20'IL')"

#filters <- "?$inlinecount=allpages&$filter=(state%20eq%20'IL')"
filters <- "?$count=true&$filter=(county%20eq%20'Cook (County')"
#filters <- "?$inlinecount=allpages&$filter=(disasterNumber%20eq%204728)"

api_query <- paste0(base_url, filters)

# run a query setting the top_n parameter to 1 to check how many records match the filters
# For Cook County only
record_check_query <- "https://www.fema.gov/api/open/v1/IndividualsAndHouseholdsProgramValidRegistrations?$filter=disasterNumber%20%eq%20%4729%20%or%20%disasterNumber%20%eq%20%4728"
#record_check_query <- "https://www.fema.gov/api/open/v1/IndividualsAndHouseholdsProgramValidRegistrations?$inlinecount=allpages&$top=1&$filter=(county%20%eq%20%'Cook (County)')"
#disasterNumber%20%eq%20%4728"

# for all of Illinois:
# record_check_query <- "https://www.fema.gov/api/open/v2/FimaNfipClaims?$inlinecount=allpages&$top=1&$filter=(state%20eq%20'IL')"

# run the api call and determine the number of matching records
result <- GET(record_check_query)
result <- GET(paste0(base_url, 
                     "?$inlinecount=allpages&$filter=(damagedStateAbbreviation%20eq%20'IL')"
                     #"?$count=true&$filter=(county%20eq%20'Cook (County')"))
                     #"?$filter=state%20eq%20'IL'"
                     ))
jsonData <- httr::content(result)        
n_records <- jsonData$metadata$count 

# calculate number of calls neccesary to get all records using the 
# 1000 records/ call max limit defined by FEMA
iterations <- ceiling(n_records / 1000)

# initialize a skip counter which will indicate where in the full 
# data set each API call needs to start from.
skip <- 0

# make however many API calls are neccesary to get the full data set
for (i in seq(from = 1, to = iterations, by = 1)) {
  # As above, if you have filters, specific fields, or are sorting, add
  # that to the base URL or make sure it gets concatenated here.
  result <- httr::GET(paste0(api_query, "&$skip=", (i - 1) * 1000))
  if (result$status_code != 200) {
    status <- httr::http_status(result)
    stop(status$message)
  }
  json_data <- httr::content(result)[[2]]
  
  # for data returned as a list of lists, correct any discrepancies
  # in the length of the lists by adding NA values to the shorter lists
  
  # calculate longest list
  max_list_length <- max(sapply(json_data, length))
  
  # add NA values to lists shorter than the max list length
  json_data <- lapply(json_data, function(x) {
    c(x, rep(NA, max_list_length - length(x)))
    
  })
  
  if (i == 1) {
    # bind the data into a single data frame
    data <- data.frame(do.call(rbind, json_data))
  } else {
    data <- dplyr::bind_rows(
      data,
      data.frame(do.call(rbind, json_data))
    )
  }
}

# remove the html line breaks from returned data frame (if there are any)  
data <- as.data.frame(lapply(data, function(data) gsub("\n", "", data)))

# view the retrieved data
# data

data %>% write_csv("./inputs/data/indiv_assistance_CookCounty2.csv")



##### New way ###########
filters< -"?$inlinecount=allpages&$filter=(damagedStateAbbreviation%20eq%20'IL'&$allrecords=true)"

#filters <- "?$inlinecount=allpages&$filter=(countyCode%20eq%20'17031')&$allrecords=true"
datalist = list()

# Code needed to obtain data on flood insurance claims in IL without the rfema package ------------------

# define the url for the appropriate api end point
base_url <- "https://www.fema.gov/api/open/v1/IndividualsAndHouseholdsProgramValidRegistrations"
# append the base_url to apply filters
#filters <- "?$count=true&$filter=(damagedStateAbbreviation%20eq%20'IL')"

api_query <- paste0(base_url, filters)
result <- GET(paste0(base_url, 
                     "?$inlinecount=allpages&$filter=(damagedStateAbbreviation%20eq%20'IL')&$allrecords=true"))
jsonData <- httr::content(result)[[2]]
data <- data.frame(do.call(rbind, jsonData))
data <- as.data.frame(lapply(data, function(data) gsub("\n", "", data)))
data %>% write_csv("./inputs/data/indiv_assistance_CookCounty3.csv")

