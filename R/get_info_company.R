#' Get description information for a company
#'
#' Uses the simfin api to get information for a single company.
#'
#' @param id_sim Id of company
#' @param api_key Your api key
#' @param cache_folder Folder to save cache files
#'
#' @return A list with information
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- 'YOURAPIHERE'
#' l_info <- simfinR_get_info_company(59265, api_key, tempdir())
#' }
simfinR_get_info_company <- function(id_sim, api_key, cache_folder) {

  if (length(id_sim) > 1) {
    stop('Input id_sim should be lenght 1')
  }

  base_url <- 'https://simfin.com/api/v1/companies/id/%s?api-key=%s'
  my_url <- sprintf(base_url, id_sim, api_key)

  # set memoise cache fct
  mem_fromJSON <- simfinR_set_fromJSON(cache_folder = tempdir())

  l_info_company <- mem_fromJSON(my_url)

  # clean up NULL values
  my_fct <- function(x) {
    if (is.null(x)) return(NA)
    return(x)
  }

  l_info_company <- lapply(l_info_company, my_fct)

  return(l_info_company)
}
