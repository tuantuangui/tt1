% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MetCox.R
\name{MetCox}
\alias{MetCox}
\title{The cox analysis about the metabolites}
\usage{
MetCox(dat)
}
\arguments{
\item{dat}{the data that row is sample, and column is the metabolite, the column
must have time and status}
}
\value{
test
}
\description{
The cox analysis about the metabolites
}
\examples{
result <- MetCox(dat_surv)
}
