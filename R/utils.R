BASE_URL <- "https://www.loc.gov"

STATES <- c("alabama", "alaska", "arizona", "arkansas", "california",
            "colorado", "connecticut", "delaware", "district of columbia",
            "florida", "georgia", "hawaii", "idaho", "illinois", "indiana",
            "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland",
            "massachusetts", "michigan", "minnesota", "mississippi",
            "missouri", "montana", "nebraska", "nevada", "new hampshire",
            "new jersey", "new mexico", "new york", "north carolina",
            "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania",
            "rhode island", "south carolina", "south dakota", "tennessee",
            "texas", "utah", "vermont", "virginia", "washington",
            "west virginia", "wisconsin", "wyoming")

# process a list of results from the json into a tibble
process_results <- function(r) {
  purrr::map(r, process_row) |>
    dplyr::bind_rows()
}

# process a single row of results from the json
process_row <- function(row) {

  county <- NA
  if(length(row$location_county) > 0) {
    county <- row$location_county[[1]]
  }
  state <- NA
  if(length(row$location_state) > 0) {
    state <- row$location_state[[1]]
  }
  country <- NA
  if(length(row$location_country) > 0) {
    country <- row$location_country[[1]]
  }

  tibble::tibble(item_url = row$id,
         date = row$date,
         publication = combine_list(row$partof_title),
         languages = combine_list(row$language),
         county = county,
         state = state,
         country = country,
         text = row$page_coordinate_data$relevant_snippet)
}

# collapse a list of character strings into a single comma separated
# character string
combine_list <- function(x) {
  paste(x, collapse = ", ")
}
