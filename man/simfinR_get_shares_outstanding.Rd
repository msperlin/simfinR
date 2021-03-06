% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_shares_outstanding.R
\name{simfinR_get_shares_outstanding}
\alias{simfinR_get_shares_outstanding}
\title{Get shares outstanding data for a group of companies}
\usage{
simfinR_get_shares_outstanding(
  id_companies,
  api_key,
  cache_folder = "simfin_cache"
)
}
\arguments{
\item{id_companies}{A vector of ids of companies}

\item{api_key}{Your api key}

\item{cache_folder}{The cache folder to save files}
}
\value{
A dataframe with shares outstanding data
}
\description{
This function will use the simfin api to grab shares outstanding of stocks.
}
\examples{
\dontrun{
df <- simfinR_get_shares_outstanding(59265, 'YOURAPIKEY')
}
}
