% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/team_win_record.R
\name{team_win_record}
\alias{team_win_record}
\title{Returns as a vector or string the win record of a given team.}
\usage{
team_win_record(data, team, text = FALSE)
}
\arguments{
\item{data}{A tibble, preferable the kenpom data}

\item{team}{A string, the team to filter for}

\item{text}{Boolean Output as text rather than a vector}
}
\value{
numeric vector or character string
}
\description{
Asked in the homework as part of Question 3e. Counts the total number of
games played by the team, calculates how many games the team won, and calculates
a win percentage. Returns a sentance (character string) or a numeric
vector based on the value of 'text'.
}
\examples{
team_win_record(get_cbb_data(), "Southern Utah")
team_win_record(get_cbb_data(), "Southern Utah", 1)

}
