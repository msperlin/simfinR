#' Get available financial statements from simfin
#'
#' Uses the api to get available statements for a single company.
#'
#' @param id_sim Id of company (see \code{\link{simfinR_get_available_companies}} for downloading a table with ids)
#' @param api_key Your api key
#' @param silent Be silent? (TRUE or FALSE)
#'
#' @return A dataframe with available statements
#' @export
#'
#' @examples
#' \dontrun{
#' id_companies <- 59265
#' api_key <- 'YOURAPIHERE'
#'
#' df_available <- simfinR_get_available_statements(id_companies, api_key = api_key)
#' }
simfinR_get_available_statements <- function(id_sim, api_key, silent = TRUE) {

  if (length(id_sim) > 1) {
    stop('This function only works for a single company')
  }

  if (!silent) {
    message('Checking available statements for ', id_sim)
  }

  base_url <- 'https://simfin.com/api/v1/companies/id/%s/statements/list?api-key=%s'
  my_url <- sprintf(base_url, id_sim, api_key)

  fix_name <- function(df_in, name_in) {

    df_in$statement <- name_in
    return(df_in)
  }

  # set memoise cache fct
  mem_fromJSON <- simfinR_set_fromJSON()

  l_out <- mem_fromJSON(my_url)

  if (length(l_out$pl) == 0) {
    df_available <- dplyr::tibble()
    return(df_available)
  }

  df_available <- dplyr::bind_rows(purrr::map2(l_out, names(l_out),
                                               .f = fix_name))

  df_available <- df_available %>%
    dplyr::mutate(id_sim = id_sim,
                  name_sim = simfinR_id_to_name(id_sim[1], api_key))

  return(df_available)

}
