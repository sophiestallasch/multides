% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe_missings.R
\name{describe_missings}
\alias{describe_missings}
\title{Compute missing statistics}
\usage{
describe_missings(data)
}
\arguments{
\item{data}{A data frame.}
}
\value{
A tibble with two columns:
\itemize{
\item{\code{variable}} {The original names of the columns in \code{data}
the missing percentage was calculated for}
\item{\code{missings}} {Percentage of missings}
}
The first row of the returned tibble gives
the total percentage of missings across all columns
(i.e., "Total sparseness").
}
\description{
This function provides missing statistics by calculating
a) the total sparseness and b) the missing percentage per column.
}
\examples{
describe_missings(studach)
}
