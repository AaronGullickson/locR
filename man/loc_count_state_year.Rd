% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{loc_count_state_year}
\alias{loc_count_state_year}
\title{Count frequency of a word from a \emph{Chronicling America} search}
\usage{
loc_count_state_year(
  query,
  year_start = 1756,
  year_end = 1963,
  facets = NULL,
  ...
)
}
\arguments{
\item{query}{Either a character string or a vector of character strings used
to search pages. The format here should be identical to \link{create_basic_loc_request}.}

\item{year_start}{An integer giving the starting year for the search. If not
provided, defaults to earliest date of 1756.}

\item{year_end}{An integer giving the ending year for the search. If not
provided, defaults to latest date of 1963.}

\item{facets}{A set of facets to further restrict the search, as defined in \link{add_facets}.}

\item{...}{Additional parameters that are passed on to \link{create_basic_loc_request}.}
}
\value{
a \link{tibble} giving the state, year, and frequency.
}
\description{
Count frequency of a word from a \emph{Chronicling America} search across states
and years.
}
\details{
This function will simply count the frequency (number of results) for a given
search across states and years, returning a \link{tibble} of those frequencies.
If the user would prefer to get actual search items, they should use \link{loc_search_pages}
instead.
}
\examples{

loc_count_state_year("banana", year_start = 1910, year_end = 1915,
                 facets = c(language = "english"), items_page = 5)

}
