\name{stearicacid}
\alias{stearicacid}
\docType{data}
\title{Digestibility percentage of fat for various levels of stearic acid}
\description{
  The average digestibility percent was measured for nine different levels of
  stearic acid proportion
}
\usage{data(stearicacid)}
\format{
  A data frame with 9 observations on the following 2 variables.
  \describe{
    \item{\code{stearic.acid}}{Percentage of stearic acid}
    \item{\code{digest}}{Average digestibility percentage}
  }
}
\source{Jorgensen, G. and Hansen, N.G. (1973). Fedtsyresammensaetningens
  indflydelse paa fedstoffers fordojelighed. Landokonomisk
  Forsogslaboratorium.
}
\examples{
data(stearicacid)
lm(digest ~ stearic.acid, data=stearicacid)
}
\keyword{datasets}
