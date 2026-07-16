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

SOUTH <- c("alabama", "arkansas", "district of columbia", "florida",
           "georgia", "louisiana", "mississippi", "missouri", "north carolina",
           "south carolina", "tennessee", "texas", "virginia", "west virginia")

NORTH <- STATES[!(STATES %in% SOUTH)]

REGIONS <- list(North = NORTH, South = SOUTH)

TRANSIENT_CODES <- c(
  408,  # Request Timeout
  425,  # Too Early (sometimes transient with proxies/CDNs)
  429,  # Too Many Requests
  500,  # Internal Server Error
  502,  # Bad Gateway
  503,  # Service Unavailable
  504,  # Gateway Timeout
  520,  # Cloudflare unknown error
  522,  # Cloudflare connection timeout
  524,  # Cloudflare timeout
  525   # Cloudflare SSL handshake failure
)

# process a list of results from the json into a tibble
process_results <- function(r) {
  purrr::map(r, process_row) |>
    dplyr::bind_rows()
}

# process a single row of results from the json
process_row <- function(row, ...) {

  tibble::tibble(
    item_url = row$id,
    date = row$date,
    publication = combine_list(row$partof_title),
    pub_id = combine_list(row$number_lccn),
    languages = combine_list(row$language),
    county = combine_list(row$location_county),
    state = combine_list(row$location_state),
    country = combine_list(row$location_country),
    description = combine_list(row$description),
    url_snippet = row$word_coordinates_url
  )
}

# collapse a list of character strings into a single comma separated
# character string
combine_list <- function(x) {
  if(purrr::is_empty(x)) {
    return(NA)
  }
  paste(x, collapse = ", ")
}
