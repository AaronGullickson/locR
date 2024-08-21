loc_count_state_year <- function(query, year_start = 1756, year_end = 1963,
                                 facets = NULL, ...) {

  req <- create_basic_loc_request(query, ...) |>
    httr2::req_url_query(at = "pagination")

  full_count <- NULL

  for (state in STATES) {
    cat("Searching", query, "in", state, "\n")
    for (year in year_start:year_end) {
      cat("\t", year, "\n")
      facets["location_state"] <- state
      response <- req |>
        add_facets(facets) |>
        restrict_years(year, year) |>
        httr2::req_perform()

      content <- response |>
        httr2::resp_body_json()

      n <- content$pagination$of

      full_count <- full_count |>
        dplyr::bind_rows(tibble::tibble(state, year, n))
    }
  }

  return(full_count)
}

loc_search_pages <- function(query, year_start = 1756, year_end = 1963,
                             facets = NULL, ...) {

  req <- create_basic_loc_request(query, ...) |>
    add_facets(facets) |>
    restrict_years(year_start, year_end)

  cat(
    "Searching", query, "[", paste(year_start, year_end, sep = "-"), "]....\n",
    "\tretreiving first page results\n"
  )

  # get the first page
  response <- req |>
    httr2::req_perform()

  page_content <- response |>
    httr2::resp_body_json()

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
      httr2::req_url_query(sp = page, at = "results") |>
      httr2::req_perform()
    page_content <- response |>
      httr2::resp_body_json()
    search_results <- process_results(page_content$results) |>
      dplyr::bind_rows(search_results)
    page <- page + 1
  }

  return(search_results)
}

#' Create a basic [httr2] query request to the *Chronicling America* API
#'
#' @description
#'
#' Produce an [httr2] request to the *Chronicling America* API that can then be
#' further modified before being sent.
#'
#' @details
#'
#' See [here](https://www.loc.gov/apis/json-and-yaml/) for more information
#' about the *Chronicling America* API for json. This is the API introduced
#' in 2024 which will eventually replace the older API.
#'
#' The search automatically is conducted on pages and uses the advanced
#' search type option, which will allow users to utilize more advanced search
#' patterns.
#'
#' This request also identifies several common http errors that affect the API
#' as transient errors so that they will not disrupt retries.
#'
#' @param query Either a character string or a vector of character strings. Searching
#' on multiple words can be conducted by either separating words with a space in
#' a single character string or providing a vector of character strings.
#'
#' @param items_page The number of items returned per page of results. The
#' default is 20, but for large searches, it will be more efficient to put
#' more items on a single page. This value should never exceed 1000.
#'
#' @param combine_type A character string equaling "OR", "AND", or "PHRASE". When
#' searching multiple words, this argument identifies whether to search for any
#' of them ("OR"), all of them ("AND") or the exact phrase ("PHRASE").
#'
#' @param throttle_rate The throttle rate as searches per second. The default
#' uses `80/60` which is the specified crawl rate for collections.
#'
#' @param retries The number of retries to attempt when the search fails because
#' of an error.
#'
#' @returns An [httr2] request that can be further modified or sent.
#'
#' @export
create_basic_loc_request <- function(query,
                                     items_page = 20,
                                     combine_type = "OR",
                                     throttle_rate = 80 / 60,
                                     retries = 10) {

  req <- httr2::request(BASE_URL) |>
    httr2::req_url_path(c("collections/chronicling-america/")) |>
    httr2::req_url_query(dl = "page",
                         ops = combine_type,
                         qs = query,
                         c = items_page,
                         searchType = "advanced",
                         sp = 1,
                         fo = "json",
                         .multi = function(x) { paste(x, collapse = "%20") }) |>
    httr2::req_retry(max_tries = retries,
                     is_transient = \(resp) httr2::resp_status(resp)
                     %in% c(429, 500, 502, 503, 520, 522)) |>
    httr2::req_throttle(rate = throttle_rate)

  return(req)
}

#' Add facets to a *Chronicling America* API request
#'
#' @description
#'
#' This function is meant to be applied to the result of a [create_basic_loc_request]
#' call. It will add "facets" that further restrict the search (e.g. by language,
#' location, subject, etc.)
#'
#' @details
#'
#' See [here](https://www.loc.gov/apis/json-and-yaml/requests/parameters/) for
#' details on available facets and their syntax.
#'
#' This function is designed to be used in a pipe.
#'
#' @param req An [httr2] request, typically created using [create_basic_loc_request].
#'
#' @param facets A vector of named character string. The name should provide the
#' facet name and the character string should indicate the value.
#'
#' @returns An [httr2] request that can be further modified or sent.
#'
#' @examples
#' create_basic_loc_request("banana") |>
#'    add_facets(c(location_state = "florida", language = "english"))
#'
#' @export
add_facets <- function(req, facets) {
  if (is.null(facets)) {
    return(req)
  }
  req |>
    httr2::req_url_query(fa = paste(paste(names(facets), facets, sep = ":"),
                                    collapse = "|"))
}

#' Restrict years for a *Chronicling America* API request
#'
#' @description
#'
#' This function is meant to be applied to the result of a [create_basic_loc_request]
#' call, by restricting the years of the search.
#'
#' @details
#'
#' This function uses the format `?dates=year_start/year_end` to restrict by
#' years. There are also API parameters for exact start and end dates that are
#' not used here.
#'
#' This function is designed to be used in a pipe.
#'
#' @param req An [httr2] request, typically created using [create_basic_loc_request].
#'
#' @param year_start An integer giving the starting year for the search.
#'
#'@param year_end An integer giving the end year for the search.
#'
#' @returns An [httr2] request that can be further modified or sent.
#'
#' @examples
#' create_basic_loc_request("banana") |>
#'    restrict_years(1912, 1917)
#'
#' @export
restrict_years <- function(req, year_start, year_end) {
  req |>
    httr2::req_url_query(dates = paste(year_start, year_end, sep = "/"))
}
