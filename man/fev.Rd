\name{fev}
\alias{fev}
\docType{data}
\title{Forced expiratory volume in children}
\description{
Dataset to examine if  respiratory function in children was influenced by
exposure to smoking at home. 
}
\usage{data(fev)}
\format{
  A data frame with 654 observations on the following 5 variables.
  \describe{
    \item{\code{Age}}{age in years}
    \item{\code{FEV}}{forced expiratory volume in liters}
    \item{\code{Ht}}{height measured in inches}
    \item{\code{Gender}}{gender (0=female, 1=male)}
    \item{\code{Smoke}}{exposure to smoking (0=no, 1=yes)}
  }
}
%\details{
%
%}
\source{
  I. Tager and S. Weiss and B. Rosner and F. Speizer (1979). Effect of
  Parental Cigarette Smoking on the Pulmonary Function of Children. 
  American Journal of Epidemiology. 110:15-26
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(fev)
}
\keyword{datasets}
