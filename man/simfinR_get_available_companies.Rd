% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_available_companies.R
\name{simfinR_get_available_companies}
\alias{simfinR_get_available_companies}
\title{Get available companies from simfin}
\usage{
simfinR_get_available_companies(api_key)
}
\arguments{
\item{api_key}{Your api key}
}
\value{
A dataframe with company name and simfin id
}
\description{
Uses the api to get a table of available companies with respective simfin ids.
All data is saved temporarily in your R session so you wont spend your dialy quota of api calls.
}
\examples{
\dontrun{
api_key <- 'YOURKEYHERE'
df_available <- simfinR_get_available_companies(api_key)
}
}
