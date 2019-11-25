#' Get financial statements from simfin
#'
#' Uses the simfim api <https://simfin.com/> for downloading corporate financial datasets available at the website.
#' In order to use the function, you'll need a valid api key from <https://simfin.com/data/access/api>.
#' All data from the site is locally cached with package memoise, meaning it will never grab repeated data from the api.
#'
#' @param id_companies Simfin Id of companies to get data (see \code{\link{simfinR_get_available_companies}} for details)
#' @param api_key Your api key (get one at <https://simfin.com/data/access/api>)
#' @param type_statements Types of financial statements ('pl' - profit loss, 'bs' - balance sheet, 'cf' - cashflow)
#' @param periods Time periods to grab the data ('FY' - final year, 'Q1' - first quarter, 'Q2', 'Q3', 'Q4')
#' @param years The years to grab data (vector or single value)
#' @param cache_folder Directory for cache files
#'
#' @return A dataframe with financial statements
#' @export
#'
#' @examples
#' \dontrun{
#' id_companies <- 59265
#' api_key <- 'YOURAPIHERE'
#'
#' df_fin <- get_fin_statements(id_companies, api_key = api_key)
#' }
simfinR_get_fin_statements <- function(id_companies,
                                       api_key,
                                       type_statements = c('pl', 'bs', 'cf'),
                                       periods = 'FY',
                                       years = 2018,
                                       cache_folder = 'simfin_cache') {

  # check input values
  possible_values <- c('pl', 'bs', 'cf')
  if (any(!(type_statements %in% possible_values))) {
    stop(paste0('Arg type_statements should be one of: ',
                paste0(possible_values,
                       collapse = ', ')))
  }

  possible_values <- c('FY', 'Q1', 'Q2', 'Q3', 'Q4')
  if (any(!(periods %in% possible_values))) {
    stop(paste0('Arg periods should be one of: ',
                paste0(possible_values,
                       collapse = ', ')))
  }

  # check available statements for all companies
  l_args <- list(id_sim = id_companies,
                 api_key = api_key,
                 silent = FALSE)

  df_available <- simfinR_get_available_companies(api_key)

  # make sure all companies are available
  message(paste0('Fetching data for companies: ', paste0(id_companies, collapse = ', ')))
  message(paste0('Fetching data for years: ', paste0(years, collapse = ', ')))
  message(paste0('Periods: ', paste0(periods, collapse = ', ')))
  message('\nMaking sure all company ids are available')

  flag <- all(id_companies %in% df_available$simId)

  if (!flag) {
    idx <- which(!(id_companies %in% df_available$simId))
    unmatched <- id_companies[idx]

    my_msg <- paste0('Cant find data for the following ids: \n\n',
                     paste0(unmatched, collapse = ', '))

    stop(my_msg)

  }

  message(' - all good')
  message('\nStart grabbing data\n')

  # set memoise cache fct
  mem_fromJSON <- simfinR_set_fromJSON(cache_folder = cache_folder)

  # set names
  year <- NULL

  # expand grid of inputs
  df_grid <- expand.grid(id_companies = id_companies,
                         type_statements = type_statements,
                         periods = periods,
                         year = years) %>%
    dplyr::arrange(id_companies, year, periods, type_statements)

  l_args <- list(id_sim = df_grid$id_companies,
                 type_statement = df_grid$type_statements,
                 period_in = df_grid$periods,
                 year = df_grid$year,
                 api_key = api_key,
                 cache_folder = cache_folder)

  l_out <- purrr::pmap(.l = l_args,
                       .f = simfinR_get_single_fin_statement)

  df_fin <- dplyr::bind_rows(l_out)

  return(df_fin)
}
