\name{crabs}
\alias{crabs}
\docType{data}
\title{Weight of crabs}
\description{
The length and weight of 361 crabs. The crabs were measured at three different days and they were raised in three different vat types.
}
\usage{data(crabs)}
\format{
  A data frame with 361 observations on the following 5 variables.
  \describe{
    \item{\code{day}}{id for day of measurement (a numeric vector)}
    \item{\code{date}}{date of measurement (a numeric vector)}
    \item{\code{kar}}{id of the vat type (a numeric vector}
    \item{\code{lgth}}{length of the crab in cm}
    \item{\code{wgt}}{weight of the crab in grams}
  }
}
\details{
Only crabs from day 1 (190692) are used in the isdals book.
}
\source{
Experiment carried out at the Royal Veterinary and Agricultural University of Copenhagen.
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(crabs)
}
\keyword{datasets}
