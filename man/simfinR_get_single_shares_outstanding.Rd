% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_shares_outstanding.R
\name{simfinR_get_single_shares_outstanding}
\alias{simfinR_get_single_shares_outstanding}
\title{Get shares outstanding data for single company}
\usage{
simfinR_get_single_shares_outstanding(
  id_sim,
  api_key,
  cache_folder = "simfin_cache"
)
}
\arguments{
\item{id_sim}{The simfin id}

\item{api_key}{Your api key}

\item{cache_folder}{Cache folder to save files}
}
\value{
A dataframe with shares outstanding
}
\description{
Helper function of \code{\link{simfinR_get_shares_outstanding}}.
Will fetch data for a single company and save results in cache.
}
\examples{
\dontrun{
df <- simfinR_get_single_shares_outstanding(59265, 'YOURAPIKEY')
}
}
