#' Get price data for a group of companies
#'
#' This function will use the simfin api to grab adjusted prices of stocks.
#'
#' @param id_companies A vector of ids of companies
#' @param api_key Your api key
#' @param cache_folder The cache folder to save files
#'
#' @return A dataframe with price data
#' @export
#'
#' @examples
#' \dontrun{
#' df_prices <- simfinR_get_price_data(59265, 'YOURAPIKEY')
#' }
simfinR_get_price_data <- function(id_companies,
                                   api_key,
                                   cache_folder = 'simfin_cache') {

  l_args <- list(id_sim = id_companies,
                 api_key = api_key,
                 cache_folder = cache_folder)
  l_out <- purrr::pmap(.l = l_args, .f = simfinR_get_single_price_data)

  df_prices <- dplyr::bind_rows(l_out)

  return(df_prices)

}

#' Get price data for single company
#'
#' Helper function of \code{\link{simfinR_get_price_data}}.
#' Will fetch data for a single company and save results in cache.
#'
#' @param id_sim The simfin id
#' @param api_key Your api key
#' @param cache_folder Cache folder to save files
#'
#' @return A dataframe with adjusted prices
#' @export
#'
#' @examples
#' \dontrun{
#' df_prices <- simfinR_get_single_price_data(59265, 'YOURAPIKEY')
#' }
simfinR_get_single_price_data <- function(id_sim,
                                          api_key,
                                          cache_folder = 'simfin_cache') {

  message(paste0('Fetching price data for ', id_sim))

  base_url <- sprintf(paste0('https://simfin.com/api/v1/companies/id/%s/shares/prices?',
                             'api-key=%s'),
                      id_sim,
                      api_key)

  # find name
  my_name <- simfinR_id_to_name(id_sim, api_key)

  # set memoise cache fct
  mem_fromJSON <- simfinR_set_fromJSON(cache_folder = cache_folder)

  l_out <- list()
  try({
    l_out <- mem_fromJSON(base_url)
  }, silent = TRUE)

  if (length(l_out) == 0) {
    message('\t- Cant find data. Returns empty tibble')
    return(dplyr::tibble())
  }

  # set names (avoids CHECK notes)
  closeAdj <- splitCoef <- ref_date <- close_adj <- split_coef <- NULL

  df_prices <- l_out$priceData %>%
    dplyr::rename(ref_date = date, close_adj = closeAdj,
           split_coef = splitCoef) %>%
    dplyr::mutate(share_class_id = l_out$shareClassId,
           share_class_name = l_out$shareClassName,
           share_class_type = l_out$shareClassType,
           currency = l_out$currency,
           ref_date = as.Date(ref_date),
           close_adj = as.numeric(close_adj),
           split_coef = split_coef,
           company_name = my_name )

  return(df_prices)

}
