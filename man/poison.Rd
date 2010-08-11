\name{poison}
\alias{poison}
\docType{data}
\title{Effects of insecticides on mortality}
\description{
The data concerns three insecticides (rotenone, deguelin, and a mixture of
those). A total of 818 insects were exposed to different doses of one of the
three insecticides. After exposure, it was recorded if the insect died or
not.
}
\usage{data(poison)}
\format{
  A data frame with 818 observations on the following 3 variables.
  \describe{
    \item{\code{status}}{status of insect: dead=1, alive=0 (numeric vector)}
    \item{\code{poison}}{type of insecticide. A factor with levels \code{D} (deguelin) \code{M} (mixture)) \code{R} (rotenone)}
    \item{\code{logdose}}{natural logarithm of dose of insecticide}
  }
}
%\details{
%
%}
\source{
D.J. Finney (1952). Probit analysis. Cambridge University Press, England.
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(poison)

}
\keyword{datasets}
