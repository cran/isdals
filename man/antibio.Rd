\name{antibio}
\alias{antibio}
\docType{data}
\title{Decomposition of organic material}
\description{
The amount of organic material in heifer dung was measured after eight weeks of decomposition.
The data come from 36 heifers from six treatment groups. The treatments are different types of antibiotics. Only 34 observations are available.
}
\usage{data(antibio)}
\format{
  A data frame with 34 observations on the following 2 variables.
  \describe{
    \item{\code{type}}{a factor with the antibiotic treatments. Level: \code{Alfacyp} \code{Control} \code{Enroflox} \code{Fenbenda} \code{Ivermect} \code{Spiramyc}}
    \item{\code{org}}{a numeric vector with the amount of organic matrial}
  }
}
%\details{
%
%}
\source{C. Sommer and B. M. Bibby (2002). The influence of veterinary medicines on the decomposition of dung organic matter in soil. European Journal of Soil Biology", 38, 115-159.
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(antibio)
}
\keyword{datasets}
