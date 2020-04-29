#' Fetch financial data for a single company
#'
#' This is a helper function for \code{\link{simfinR_get_fin_statements}}.
#' It will make calls to the simfin api and save the results in cache.
#'
#' @param id_sim Simfin id of a company (single value)
#' @param type_statement Type of financial statements ('pl' - profit loss, 'bs' - balance sheet, 'cf' - cashflow)
#' @param period_in Time periods to grab the data: 'FY' - fiscal year (only for 'pl' and 'cf', use 'Q4' for end of year 'bs'),
#'                'Q1' - first quarter, 'Q2', 'Q3', 'Q4'
#' @param year The year
#' @param api_key Your api key
#' @param cache_folder The directory to cache results
#'
#' @return A single dataframe with the data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' id_companies <- 59265
#' api_key <- 'YOURAPIHERE'
#'
#' df_single_fin <- get_single_fin_statement(id_sim = id_companies, type_statement = 'pl',
#'                                           period_in = 'FY', year = 2018, api_key=api_key,
#'                                           cache_folder = 'simfim_cache')
#' }
simfinR_get_single_fin_statement <- function(id_sim,
                                             type_statement,
                                             period_in,
                                             year,
                                             api_key,
                                             cache_folder) {

  # check args
  ## this function is called by get_fin_statements so no need for arg checking

  # get info of of company
  l_info_company <- simfinR_get_info_company(id_sim, api_key, cache_folder)


  message(paste0('Fetching data for ', l_info_company$name,
                 ' (', id_sim, ') | type=', type_statement,
                 ' | period=', period_in, ' | year=', year))

  # check if data is available
  df_available <- simfinR_get_available_statements(id_sim = id_sim,
                                                   api_key = api_key)

  #browser()

  df_available <- df_available %>%
    dplyr::filter(period == period_in,
                  fyear == year,
                  statement == type_statement)

  if (nrow(df_available) < 1) {
    message('\tCant find data..')
    return(dplyr::tibble())
  } else {
    message('\tTable found. Fetching it..')
  }

  base_url <- sprintf(paste0('https://simfin.com/api/v1/companies/id/%s/statements/standardised?',
                             'api-key=%s&',
                             'stype=%s&',
                             'ptype=%s&',
                             'fyear=%s'),
                      id_sim,
                      api_key,
                      type_statement,
                      period_in,
                      year)

  # set memoise cache fct
  mem_fromJSON <- simfinR_set_fromJSON(cache_folder = cache_folder)

  l_out <- mem_fromJSON(base_url)

  # organize data
  raw_df_values <- l_out$values
  ref_date = simfinR_period_to_date(period_in, year)

  # set object for CRAN checks (undefined global..)
  acc_value <- checkPossible <- closeAdj <- close_adj <- NULL
  company_name <- company_sector <- displayLevel <- fyear <- parent_tid <- NULL
  period <- splitCoef <- split_coef <- standardisedName <- NULL
  statement <- tid <- uid <- valueChosen <- NULL

  df_values <- raw_df_values %>%
    dplyr::mutate(id_sim = id_sim,
                  type_statement = type_statement,
                  period = period_in,
                  year = year,
                  ref_date = ref_date,
                  company_name = l_info_company$name,
                  company_sector = l_info_company$sectorName,
                  company_ticker = l_info_company$ticker) %>%
    dplyr::select(company_name, company_sector,
                  type_statement, period, year, ref_date,
                  acc_name = standardisedName, acc_value = valueChosen,
                  tid, uid, parent_tid, display_level = displayLevel,
                  check_possible = checkPossible) %>%
    dplyr::mutate(acc_value = as.numeric(acc_value))

  return(df_values)

}
