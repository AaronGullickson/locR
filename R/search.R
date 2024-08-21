loc_count_state_year <- function(query, year_start = 1756, year_end = 1963,
                                 facets = NULL, ...) {
  
  req <- create_basic_request(query, ...) |>
    req_url_query(at = "pagination")

  full_count <- NULL

  for (state in STATES) {
    cat("Searching", query, "in", state, "\n")
    for (year in year_start:year_end) {
      cat("\t", year, "\n")
      facets["location_state"] <- state
      response <- req |>
        add_facets(facets) |>
        restrict_years(year, year) |>
        req_perform()

      content <- response |>
        resp_body_json()

      n <- content$pagination$of

      full_count <- full_count |>
        bind_rows(tibble(state, year, n))
    }
  }

  return(full_count)
}

loc_search_pages <- function(query, year_start = 1756, year_end = 1963,
                             facets = NULL, ...) {
  
  req <- create_basic_request(query, ...) |>
    add_facets(facets) |>
    restrict_years(year_start, year_end)

  cat(
    "Searching", query, "[", paste(year_start, year_end, sep = "-"), "]....\n",
    "\tretreiving first page results\n"
  )

  # get the first page
  response <- req |>
    req_perform()

  page_content <- response |>
    resp_body_json()

  # get total number of pages required
  pages_total <- page_content$pagination$total

  # get total number of results
  results_total <- page_content$pagination$of

  if (results_total > 100000) {
    # TODO: stop and print warning
  }

  cat("\tTotal of ", results_total, "results on", pages_total, "page(s)\n")

  # use first page of results to start dataset
  search_results <- process_results(page_content$results)

  # turn the page
  page <- 2

  # while loop through pages and add results
  while (page <= pages_total) {
    cat("\t\tretreiving page", paste(page, pages_total, sep = "/"), "\n")
    response <- req |>
      req_url_query(sp = page, at = "results") |>
      req_perform()
    page_content <- response |>
      resp_body_json()
    search_results <- process_results(page_content$results) |>
      bind_rows(search_results)
    page <- page + 1
  }

  return(search_results)
}

create_basic_request <- function(query,
                                 items_page = 20,
                                 combine_type = "OR",
                                 throttle_rate = 80 / 60,
                                 retries = 10) {
  
  req <- request(BASE_URL) |>
    req_url_path(c("collections/chronicling-america/")) |>
    req_url_query(dl = "page",
                  ops = combine_type,
                  qs = query,
                  c = items_page,
                  searchType = "advanced",
                  sp = 1,
                  fo = "json",
                  .multi = function(x) { paste(x, collapse = "%20") }) |>
    req_retry(max_tries = retries,
              is_transient = \(resp) resp_status(resp) 
                %in% c(429, 500, 502, 503, 520, 522)) |>
    req_throttle(rate = throttle_rate)

  return(req)
}

add_facets <- function(req, facets) {
  if (is.null(facets)) {
    return(req)
  }
  req |>
    req_url_query(fa = paste(paste(names(facets), facets, sep = ":"),
                  collapse = "|"))
}

restrict_years <- function(req, year_start, year_end) {
  req |>
    req_url_query(dates = paste(year_start, year_end, sep = "/"))
}
