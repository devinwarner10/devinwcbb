% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_teams_records.R
\name{all_teams_records}
\alias{all_teams_records}
\title{Builds Team Win Records Table}
\usage{
all_teams_records(data)
}
\arguments{
\item{data}{A tibble of the Ken Pomeroy basketball data}
}
\value{
A tibble. The rows are individual teams and sorted by weighted rank.
The columns give games won, total games, and win percentage.
}
\description{
Takes the Ken Pomeroy basketball data and builds a tibble of each teams
win record. Give the full Ken Pomeroy data to get total ranking. Give the
Ken Pomeroy data filtered for conference games to get conference rankings.
}
\examples{

## Get total ranking for each team
all_teams_records(get_cbb_data(0))


## Get conference ranking for each team
all_teams_records(get_cbb_data(1))

}
