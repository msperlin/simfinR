% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_price_data_share_classes.R
\name{simfinR_get_price_by_class}
\alias{simfinR_get_price_by_class}
\title{Get price data by share class for a group of companies}
\usage{
simfinR_get_price_by_class(
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
A dataframe with price data by share class
}
\description{
This function will use the simfin api to grab price data by share class.
}
\examples{
\dontrun{
df <- simfinR_get_price_by_class(59265, 'YOURAPIKEY')
}
}
