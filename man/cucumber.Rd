\name{cucumber}
\alias{cucumber}
\docType{data}
\title{Disease spread in cucumber}
\description{
  Spread of a disease in cucumbers depends on climate and amount of
  fertilizer. The amount of infection on standardized plants was
  recorded after a number of days, and two plants were examined for
  each combination of climate and dose.
}
\usage{data(cucumber)}
\format{
  A data frame with 361 observations on the following 3 variables.
  \describe{
    \item{\code{disease}}{a numeric vector}
    \item{\code{climate}}{a factor with levels \code{A} (change to day temperature 3
  hours before sunrise) and \code{B} (normal change to day temperature)}
    \item{\code{dose}}{a numeric vector with dose of applied fertilizer}
  }
}
%\details{
%
%}
\source{
de Neergaard, E. et al (1993). Studies of Didymella bryoniae: the
influence of nutrition and cultural practices on the occurrence of stem
lesions and internal and external fruit rot on different cultivars of
cucumber. Netherlands Journal of Plant Pathology. 99:335-343
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(cucumber)
}
\keyword{datasets}
