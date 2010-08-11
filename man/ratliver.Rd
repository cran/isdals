\name{ratliver}
\alias{ratliver}
\docType{data}
\title{Drugs in rat's livers}
\description{
An experiment was undertaken to
investigate the amount of drug present in the liver of a rat. Nineteen
rats were randomly selected, weighed, placed under a light anesthetic,
and given an oral dose of the drug. It was believed that large livers
would absorb more of a given dose than a small liver, so the actual dose
given was approximately determined as 40 mg of the drug per kilogram of
body weight. After a fixed length of time, each rat was sacrificed, the
liver weighed, and the percent dose in the liver was determined.
}
\usage{data(ratliver)}
\format{
  A data frame with 19 observations on the following 4 variables.
  \describe{
    \item{\code{BodyWt}}{body weight of each rat in grams}
    \item{\code{LiverWt}}{weight of liver in grams}
    \item{\code{Dose}}{relative dose of the drug given to each
  rat as a fraction of the largest dose}
    \item{\code{DoseInLiver}}{proportion of the dose in the liver}
  }
}
%\details{
%
%}
\source{
S. Weisberg (1985). Applied Linear Regression (2nd ed.). John Wiley and Sons
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(ratliver)
}
\keyword{datasets}
