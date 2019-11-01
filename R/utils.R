#' Sets memoise cache function
#'
#' USes the memoise package to set a cache function for jsonlite::fromJSON
#' The choice of the cache folder defines where to save the local files. If not set, will use tempdir()
#'
#' @param cache_folder The local folder where to save files
#'
#' @return A memoised function
#' @export
#'
#' @examples
#' mem_fromJSON <- simfinR_set_fromJSON()
#'
simfinR_set_fromJSON <- function(cache_folder = tempdir()) {

  mem_fromJSON <- memoise::memoise(jsonlite::fromJSON,
                                   cache = memoise::cache_filesystem(cache_folder))

  return(mem_fromJSON)

}

#' Finds a company name by its simfin id
#'
#' @param id_in Id of company
#' @param api_key Your api key
#'
#' @return A string (name of company)
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- 'YOURAPIHERE'
#' my_name <- simfinR_id_to_name(59265, api_key)
#' }
simfinR_id_to_name <- function(id_in, api_key) {

  if (length(id_in) > 1) {
    stop('Input id_in should be lenght 1')
  }

  df_companies <- simfinR_get_available_companies(api_key = api_key)

  idx <- which(df_companies$simId == id_in)

  if (length(idx) == 0) {
    my_msg <- paste0('Cant find name for ', id_in)
    stop(my_msg)
  }

  name_out <- df_companies$name[idx]

  return(name_out)
}

#' Period to date
#'
#' Returns the last date for a given period and year.
#'
#' @param period_in The period ('Q1', 'Q2', 'Q3', 'Q4', 'FY')
#' @param year_in The year
#'
#' @return A single date object
#' @export
#'
#' @examples
#' first_quarter <- simfinR_period_to_date('Q1', 2019)
simfinR_period_to_date <- function(period_in, year_in) {

  all_year <-seq(as.Date(paste0(year_in, '-01-01')),
                 as.Date(paste0(year_in, '-12-31')),
                 by = '1 day')

  ref_date <- NULL

  df_quarters <- dplyr::tibble(ref_date = all_year,
                               quarters = lubridate::quarter(all_year)) %>%
    dplyr::group_by(quarters) %>%
    dplyr::summarise(last_date = dplyr::last(ref_date))

  out <- switch(as.character(period_in),
                'Q1' = df_quarters$last_date[1],
                'Q2' = df_quarters$last_date[2],
                'Q3' = df_quarters$last_date[3],
                'Q4' = df_quarters$last_date[4],
                'FY' = df_quarters$last_date[4])

  return(out)

}
