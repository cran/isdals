\name{dioxin}
\alias{dioxin}
\docType{data}
\title{dioxin in water}
\description{
Over a period of 14 years from 1990 to 2003, environmental agencies
monitored the average amount of dioxins found in the liver of crabs at
two different monitoring stations located some distance apart from a
closed paper pulp mill. The outcome is the average total equivalent
dose (TEQ), which is a summary measure of different forms of dioxins
with different toxicities found in the crabs
}
\usage{data(dioxin)}
\format{
  A data frame with 28 observations on the following 3 variables.
  \describe{
    \item{\code{site}}{a factor with levels \code{a} \code{b} corresponding to the two monitoring stations}
    \item{\code{year}}{the year}
    \item{\code{TEQ}}{a numeric vector for the total equivalent dose}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{C. J. Schwarz (2013). Sampling, Regression, Experimental Design and Analysis for Environmental Scientists, Biologists, and Resource Managers. Course Notes.}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(dioxin)
## maybe str(dioxin) ; plot(dioxin) ...
}
\keyword{datasets}
