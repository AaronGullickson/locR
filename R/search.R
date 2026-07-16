#' Count frequency of a word from a *Chronicling America* search
#'
#' @description
#' Count frequency of a word from a *Chronicling America* search across states
#' and years.
#'
#' @details
#'
#' This function will simply count the frequency (number of results) for a given
#' search across region (North/South) and years, returning a [tibble] of those frequencies.
#' If the user would prefer to get actual search items, they should use [loc_search_pages]
#' instead.
#'
#' Currently, the function only counts across the 50 states of the US and the
#' District of Columbia.
#'
#' @param query Either a character string or a vector of character strings used
#' to search pages. The format here should be identical to [create_basic_loc_request].
#'
#' @param year_start An integer giving the starting year for the search. If not
#' provided, defaults to earliest date of 1756.
#'
#' @param year_end An integer giving the ending year for the search. If not
#' provided, defaults to latest date of 1963.
#'
#' @param facets A set of facets to further restrict the search, as defined in [add_facets].
#'
#' @param ... Additional parameters that are passed on to [create_basic_loc_request].
#'
#' @returns a [tibble] giving the state, year, and frequency.
#'
#' @examples
#'
#' loc_count_region_year("banana", year_start = 1910, year_end = 1915,
#'                  facets = c(language = "english"), items_page = 5)
#'
#' @export
loc_count_region_year <- function(query, year_start = 1756, year_end = 1963,
                                  facets = NULL, ...) {

  req <- create_basic_loc_request(query, ...) |>
    httr2::req_url_query(at = "pagination")

  full_count <- NULL

  for(year in year_start:year_end) {
    cat("\t", year, "\n")
    full_count <- purrr::imap_dfr(REGIONS, \(states, region) {
      response <- req |>
        httr2::req_url_query(location_state = paste(states, collapse = "!")) |>
        restrict_years(year, year) |>
        httr2::req_perform()

      content <- response |>
        httr2::resp_body_json()

      n <- content$pagination$of

      dplyr::bind_rows(tibble::tibble(region, year, n))
    }) |>
      dplyr::bind_rows(full_count)
  }

  return(full_count)
}

#' Retrieve all items from a *Chronicling America* search
#'
#' @description
#' Retrieve all items from a *Chronicling America* search and place them into a
#' [tibble] dataset.
#'
#' @details
#'
#' It is best to use year restrictions and/or facets to reduce the total volume
#' of the search. Searches with more than 100,000 total items will likely be
#' refused by the API. It is also generally useful to increase the `items_page`
#' parameter from its default of 20 to reduce the problem of "deep paging"
#' across results and reduce the overall number of requests. The `items_page`
#' cannot be larger than 1000. A value of 500 often works well.
#'
#' The returned results from the json are formatted into a [tibble] with only
#' the following values:
#'
#' * item url
#' * date
#' * publisher
#' * location: county, state, and year
#' * text snippet
#'
#' Users who want different values will returned would need to adjust the
#' internal function `process_row` which does this work.
#'
#' @param query Either a character string or a vector of character strings used
#' to search pages. The format here should be identical to [create_basic_loc_request].
#'
#' @param year_start An integer giving the starting year for the search. If not
#' provided, defaults to earliest date of 1756.
#'
#' @param year_end An integer giving the ending year for the search. If not
#' provided, defaults to latest date of 1963.
#'
#' @param facets A set of facets to further restrict the search, as defined in [add_facets].
#'
#' @param ... Additional parameters that are passed on to [create_basic_loc_request].
#'
#' @returns a [tibble] of items, with one item per row.
#'
#' @examples
#'
#' loc_search_pages("banana", year_start = 1910, year_end = 1915,
#'                  facets = c(location_state = "florida", language = "english"),
#'                  items_page = 500)
#'
#' @export
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

  cat("\tApproximate total of ", results_total, "results on", pages_total, "page(s)\n")

  # loop through pages and add results - we don't trust initial totals
  # and pagination, so keep cycling until I don't get a next
  page <- 1
  search_results <- NULL
  repeat {
    # get response
    cat("\t\tretreiving page", page, "\n")
    response <- req |>
      httr2::req_url_query(sp = page) |>
      httr2::req_perform()
    # get content
    page_content <- response |>
      httr2::resp_body_json()
    search_results <- process_results(page_content$results) |>
      dplyr::bind_rows(search_results)
    # is there more?
    if(is.null(page_content$pagination$`next`)) {
      break
    }
    page <- page + 1
  }

  return(search_results)
}

#' Sample all items from a *Chronicling America* search
#'
#' @description
#' Sample all items from a *Chronicling America* search, stratified by year, and
#' place them into a [tibble] dataset.
#'
#' @details
#'
#' For very large searches, this function will sample the full results including
#' a text snippet. Results are sampled by year, so that a consistent number of
#' items are provided by year. Additionally, sampling is done by whole pages,
#' so it constitues a form of cluster sampling. Weights and cluster ids are
#' included in the final result to account for the sampling procedure.
#'
#' It is best to use year restrictions and/or facets to reduce the total volume
#' of the search. Searches with more than 100,000 total items will likely be
#' refused by the API.
#'
#' The returned results from the json are formatted into a [tibble] with only
#' the following values:
#'
#' * item url
#' * date
#' * publisher
#' * location: county, state, and year
#' * text snippet
#'
#' Users who want different values returned would need to adjust the
#' internal function `process_row` which does this work.
#'
#' @param query Either a character string or a vector of character strings used
#' to search pages. The format here should be identical to [create_basic_loc_request].
#'
#' @param n_sample_page The number of pages of items to sample each year. Combined
#' with `items_page` will give the sample size of items per year. If the number of
#' pages returned from the search in a given year is below this number, the full
#' number of items will be kept. Only full pages are sampled, so the last page
#' is ignored.
#'
#' @param year_start An integer giving the starting year for the search. If not
#' provided, defaults to earliest date of 1756.
#'
#' @param year_end An integer giving the ending year for the search. If not
#' provided, defaults to latest date of 1963.
#'
#' @param facets A set of facets to further restrict the search, as defined in [add_facets].
#'
#' @param ... Additional parameters that are passed on to [create_basic_loc_request].
#' Users should not pass the `items_page` parameter as this is defined by the
#' sampling procedure as 1.
#'
#' @returns a [tibble] of items, with one item per row.
#'
#' @examples
#'
#' loc_sample_pages("banana", year_start = 1910, year_end = 1915,
#'                  facets = c(language = "english"))
#'
#' @export
loc_sample_pages <- function(query, n_sample_page = 10,
                             year_start = 1756, year_end = 1963,
                             facets = NULL,
                             ...) {

  # force items_page to 1 for sampling
  req <- create_basic_loc_request(query, ...) |>
    add_facets(facets)

  # NULL object for later search results
  search_results <- NULL

  # loop through years and sample in each year
  for(year in year_start:year_end) {
    cat("\t", year, "\n")
    response <- req |>
      restrict_years(year, year) |>
      httr2::req_perform()

    content <- response |>
      httr2::resp_body_json()

    n_pages <- content$pagination$total
    n_items <- content$pagination$of

    # if the number of available pages is less than or equal to our sample, then
    # we just use everything, otherwise we sample
    # also we don't sample the last partial page because of issues
    pages_sampled <- 1:max(1, (n_pages - 1))
    if(n_sample_page < length(pages_sampled)) {
      pages_sampled <- sample(pages_sampled, n_sample_page, replace = FALSE)
    }

    # loop through pages and get the data
    year_results <- NULL
    for(page in pages_sampled) {
      cat("\t\tretrieving page", page, "\n")
      response <- req |>
        restrict_years(year, year) |>
        httr2::req_url_query(sp = page) |>
        httr2::req_perform()
      page_content <- response |>
        httr2::resp_body_json()
      year_results <- process_results(page_content$results) |>
        dplyr::mutate(cluster_id = paste(year, page, sep = ".")) |>
        dplyr::bind_rows(year_results)
    }

    # add weights (inverse of probability of being sampled)
    year_results$weight <- n_items/nrow(year_results)
    search_results <- year_results |>
      dplyr::bind_rows(search_results)
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
                                     throttle_rate = 20 / 60,
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
    httr2::req_options(http_version = 1) |>
    httr2::req_retry(
      max_tries = retries,
      is_transient = \(resp) httr2::resp_status(resp) %in% TRANSIENT_CODES,
      retry_on_failure = TRUE
    ) |>
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

# retrieve the snippet from the URL provided in the response
# Currently I am no longer using this because I don't want to make a separate
# request for each row within the existing request, but it could be re-used to
# pull snippets from a sample or the full data later.

#' Retrieve the relevant snippet from a search for a given URL
#'
#' @description
#'
#' Each item is returned with a url to the relevant snippet for the searched
#' text but not the snippet itself. This function will retrieve the snippet
#' text from the API
#'
#' @details
#'
#' The url uses the Text Services API to extract the relevant snippet. The
#' throttle speed for this service is 150 request/60 seconds.
#'
#' @param url A character string of the URL for the snippet
#'
#' @returns An snippet of text as a character string.
#'
#' @export
retrieve_snippet <- function(url) {

  httr2::request(url) |>
    httr2::req_retry(
      max_tries = 30,
      is_transient = \(resp) httr2::resp_status(resp) %in% TRANSIENT_CODES
    ) |>
    httr2::req_throttle(rate = 150 / 60) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck(1, "relevant_snippet", .default = NA_character_)

}
