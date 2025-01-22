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
process_row <- function(row, ...) {

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
         text = retrieve_snippet(row$word_coordinates_url))
}

# collapse a list of character strings into a single comma separated
# character string
combine_list <- function(x) {
  paste(x, collapse = ", ")
}

# retrieve the snippet from the URL provided in the response
retrieve_snippet <- function(url) {

  response <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    httr2::req_retry(max_tries = 30,
                     is_transient = \(resp) httr2::resp_status(resp)
                     %in% c(429, 500, 502, 503, 520, 522)) |>
    httr2::req_throttle(rate = 80 / 60)

  return(response[[1]]$relevant_snippet)

}
