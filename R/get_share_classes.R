#' Get share class data for a group of companies
#'
#' This function will use the simfin api to grab share classes of stocks.
#'
#' @param id_companies A vector of ids of companies
#' @param api_key Your api key
#' @param cache_folder The cache folder to save files
#'
#' @return A dataframe with share class data
#' @export
#'
#' @examples
#' \dontrun{
#' df <- simfinR_get_share_classes(59265, 'YOURAPIKEY')
#' }
simfinR_get_share_classes <- function(id_companies,
                                   api_key,
                                   cache_folder = 'simfin_cache') {

  l_args <- list(id_sim = id_companies,
                 api_key = api_key,
                 cache_folder = cache_folder)
  l_out <- purrr::pmap(.l = l_args, .f = simfinR_get_single_share_classes)

  df <- dplyr::bind_rows(l_out)

  return(df)

}

#' Get share class data for single company
#'
#' Helper function of \code{\link{simfinR_get_share_classes}}.
#' Will fetch data for a single company and save results in cache.
#'
#' @param id_sim The simfin id
#' @param api_key Your api key
#' @param cache_folder Cache folder to save files
#'
#' @return A dataframe with share classes
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_share_classes(59265, 'YOURAPIKEY')
#' }
simfinR_get_single_share_classes = function(id_sim, api_key, cache_folder = 'simfin_cache'){

  message(paste0('Fetching share classes for ', id_sim))

  base_url <- sprintf('https://simfin.com/api/v1/companies/id/%s/shares/classes/list?api-key=%s',
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

  df <- l_out %>%
    dplyr::rename(share_class_id = shareClassId, share_class_name = shareClassName,
           share_class_type = shareClassType) %>%
    dplyr::mutate(company_name = my_name)

  return(df)
  
}
