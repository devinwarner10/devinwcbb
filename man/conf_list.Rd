% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conf_list.R
\name{conf_list}
\alias{conf_list}
\title{Creates a list of conferences}
\usage{
conf_list(conf_data)
}
\arguments{
\item{data}{A tibble of the Ken Pomeroy data filtered for conference games}
}
\value{
A tibble. Has one column of distinct conference names.
}
\description{
Takes the Ken Pomeroy basketball data and creates a list of distinct
conferences featured in the data set. Used to create the conference selection
drop down in the Shiny app.
}
