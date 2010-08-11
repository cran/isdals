\name{pillbug}
\alias{pillbug}
\docType{data}
\title{Effect of stimuli on pillbugs}
\description{
An experiment on the effect of different stimuli was carried out with 60
pillbugs. The bugs were split into three
groups: 20 bugs were exposed to strong light, 20 bugs were exposed to
moisture, and 20 bugs were used as controls. For each bug it was registered how
many seconds it used to move six inches.  
}
\usage{data(pillbug)}
\format{
  A data frame with 60 observations on the following 2 variables.
  \describe{
    \item{\code{time}}{number of seconds it took the pillbug to move six inches}
    \item{\code{group}}{treatment. A factor with levels \code{Control} \code{Light} \code{Moisture}}
  }
}
%\details{
%
%}
\source{
Samuels and Witmer (2003). Statistics for the Life Sciences (3rd ed.).
Pearson Education, Inc., New Jersey.
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(pillbug)
}
\keyword{datasets}
