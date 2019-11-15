#' Get available companies from simfin
#'
#' Uses the api to get a table of available companies with respective simfin ids.
#' All data is saved temporarily in your R session so you wont spend your dialy quota of api calls.
#'
#' @param api_key Your api key
#'
#' @return A dataframe with company name and simfin id
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- 'YOURKEYHERE'
#' df_available <- simfinR_get_available_companies(api_key)
#' }
simfinR_get_available_companies <- function(api_key) {

  base_url <- 'https://simfin.com/api/v1/info/all-entities'
  my_url <- paste0(base_url,'?api-key=',api_key )

  # set memoise cache fct
  mem_fromJSON <- simfinR_set_fromJSON()

  df_info <- mem_fromJSON(my_url)

  return(df_info)
}
