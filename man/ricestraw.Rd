\name{ricestraw}
\alias{ricestraw}
\docType{data}
\title{Weight increase for cattle fed with rice straw}
\description{
Weight gain of cattle fed with rice straw to see if rice straw can
replace wheat straw as potential feed for slaughter cattle in Tanzania
}
\usage{data(ricestraw)}
\format{
  A data frame with 35 observations on the following 2 variables.
  \describe{
    \item{\code{time}}{number of days that the calf has been fed rice straw}
    \item{\code{weight}}{weight gain (in kg) since the calf was first
      fed rice straw}
  }
}
%\details{
%}
\source{
  Ph.D. project at the Faculty of LIFE Sciences, University of Copenhagen
}
%\references{
%}
\examples{
data(ricestraw)
plot(ricestraw$time, ricestraw$weight)
lm(weight ~ time, data=ricestraw)
}
\keyword{datasets}
