# Get Addresses for PINs missing flood factor scores

library(httr2)
library(jsonlite)
library(dplyr)
library(readxl)

# Load your file
df <- read_excel("./data/processed/missing_floodfactor_scores.xlsx") |>
  mutate(pin10 = str_pad(pin10, width = 10, pad = "0", side = "left"))

# Get unique list of non-missing pin10s
pin10_list <- df$pin10 |> na.omit() |> unique()

# Function to query Cook County API in batches
query_cook_api <- function(pins) {
  base_url <- "https://datacatalog.cookcountyil.gov/resource/3723-97qp.json"
  
  # Chunk pins to avoid overly long URLs (50 is safe)
  pin_batches <- split(pins, ceiling(seq_along(pins) / 50))
  
  all_results <- list()
  
  for (batch in pin_batches) {
    pin_query <- paste0("pin10 in(", paste0("'", batch, "'", collapse = ","), ")")
    
    resp <- request(base_url) |>
      req_url_query(`$where` = pin_query) |>
      req_perform()
    
    if (resp_status(resp) == 200) {
      results <- resp |>
        resp_body_json(simplifyDataFrame = TRUE)
      all_results <- append(all_results, list(results))
    } else {
      warning("Failed request with status: ", resp_status(resp))
    }
    
    Sys.sleep(0.5)  # respect rate limits
  }
  
  # Combine all results
  bind_rows(all_results)
}

# Run the query
cook_data <- query_cook_api(pin10_list)

cook_data2 <- cook_data |>
  mutate(across(everything(), ~ na_if(.x, "UNKNOWN"))) |>
# Create a "completeness" score — number of non-missing fields
  mutate(non_missing = rowSums(!is.na(across(c(prop_address_full:mail_address_zipcode_1))))) |>
  group_by(pin10) |>
  arrange(desc(year), desc(non_missing)) |>
  slice(1) |>
  ungroup()

cook_data2 <- cook_data2 |> select(-c(prop_address_state, mail_address_state))


# Check and pull for pins that do not have 0's padding the number
# had 1556 pins before string padding my excel file when reading it in.
# 1752 after zero padding my excel file

still_missing_data <- anti_join(df, cook_data2, by = "pin10")

# 1312301035 is 3115 W Foster Ave Chicago, is class 315 now? maybe?
# 0519314069 and 0519314070 is 1706 and 1707 Northfield Square B, Northfield
# 0519314072 and 0519314074 are 596 Oak Street Winnetka
# 0521117003 and 0521117004 are 596 Oak Street Winnetka became 0521117018 - bought and combined parcels.


joined <- left_join(df, cook_data2)

# Save the results
write.csv(joined, "data/processed/pins_with_some_addresses.csv", row.names = FALSE)
