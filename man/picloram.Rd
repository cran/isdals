\name{picloram}
\alias{picloram}
\docType{data}
\title{
Picolram and herbacide efficacy
}
\description{
A small dataset for evaluating the effects of increasing pplication rates of picloram for control of tall larkspur. 
}
\usage{data(picloram)}
\format{
  A data frame with 313 observations on the following 3 variables.
  \describe{
    \item{\code{replicate}}{a factor with levels \code{1} \code{2} \code{3} corresponding to the three replicates (locations) used}
    \item{\code{dose}}{the dose of picloram used in kg ae/ha}
    \item{\code{status}}{a numeric vector. 0 means the plant survived, 1 that it died}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
David L. Turner, Michael H. Ralphs and John O. Evans (1992): Logistic Analysis for Monitoring and Assessing Herbicide Efficacy. Weed Technology
}
\examples{
data(picloram)
}
\keyword{datasets}
