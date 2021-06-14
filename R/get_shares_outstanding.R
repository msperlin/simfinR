#' Get shares outstanding data for a group of companies
#'
#' This function will use the simfin api to grab shares outstanding of stocks.
#'
#' @param id_companies A vector of ids of companies
#' @param api_key Your api key
#' @param cache_folder The cache folder to save files
#'
#' @return A dataframe with shares outstanding data
#' @export
#'
#' @examples
#' \dontrun{
#' df <- simfinR_get_shares_outstanding(59265, 'YOURAPIKEY')
#' }
simfinR_get_shares_outstanding <- function(id_companies,
                                   api_key,
                                   cache_folder = 'simfin_cache') {

  l_args <- list(id_sim = id_companies,
                 api_key = api_key,
                 cache_folder = cache_folder)  
  l_out <- purrr::pmap(.l = l_args, .f = simfinR_get_single_shares_outstanding)

  df <- dplyr::bind_rows(l_out)

  return(df)

}

#' Get shares outstanding data for single company
#'
#' Helper function of \code{\link{simfinR_get_shares_outstanding}}.
#' Will fetch data for a single company and save results in cache.
#'
#' @param id_sim The simfin id
#' @param api_key Your api key
#' @param cache_folder Cache folder to save files
#'
#' @return A dataframe with shares outstanding
#' @export
#'
#' @examples
#' \dontrun{
#' df <- simfinR_get_single_shares_outstanding(59265, 'YOURAPIKEY')
#' }
simfinR_get_single_shares_outstanding = function(id_sim, api_key, cache_folder = 'simfin_cache'){

  share_classes = simfinR_get_single_share_classes(id_sim, api_key)

  # I don't expect this to happen but check to ensure no duplicate ids.
  if(any(duplicated(share_classes$share_class_id))) stop(
      'share_class_id is duplicated, this will cause duplication in joins.'
  )

  df = dplyr::bind_rows(lapply(
    split(share_classes, 1:nrow(share_classes)),
    function(share_class){
        
      idt = NULL
      
      response = simfinR_get_single_shares_outstanding_for_class(
          id_sim,
          share_class$share_class_id,
          api_key,
          cache_folder
      )
      
      for(type in names(response)) if(length(response[[type]]) > 0) idt %<>% dplyr::bind_rows(data.frame(
        share_class_id = share_class$share_class_id,
        type = type,
        ref_date = as.Date(response[[type]]$date),
        value = response[[type]]$value,
        stringsAsFactors = FALSE
      ))
      
      return(idt)
      
  }))
  
  if(nrow(df) == 0) return(list())

  df = dplyr::inner_join(
      share_classes,
      df,
      by = 'share_class_id'
  )

  return(df)
  
}

#' Get shares outstanding data for single company and share class
#'
#' Helper function of \code{\link{simfinR_get_single_shares_outstanding}}.
#' Will fetch data for a single company and save results in cache.
#'
#' @param id_sim The simfin id
#' @param id_share_class The simfin share class id from simfinR_get_share_classes
#' @param api_key Your api key
#' @param cache_folder Cache folder to save files
#'
#' @return A dataframe with shares outstanding
#' @export
#'
#' @examples
#' \dontrun{
#' df <- simfinR_get_single_shares_outstanding_for_class(59265, 'YOURAPIKEY')
#' }
simfinR_get_single_shares_outstanding_for_class = function(id_sim, id_share_class, api_key, cache_folder = 'simfin_cache'){    

  message(paste0('Fetching shares outstanding for sim ', id_sim, ' and class ', id_share_class))

  base_url <- sprintf('https://simfin.com/api/v1/companies/id/%s/shares/classes/%s/outstanding?api-key=%s',
                      id_sim,
                      id_share_class,
                      api_key)

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

  return(l_out)
}