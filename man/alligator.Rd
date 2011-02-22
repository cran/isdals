\name{alligator}
\alias{alligator}
\docType{data}
\title{Alligator food preference}
\description{Data on food preference for 59 alligators. It is of
  interest to examine if different sized alligators have different food
  preferences. 
}
\usage{data(alligator)}
\format{
  A data frame with 59 observations on the following 2 variables.
  \describe{
    \item{\code{length}}{length of the alligator (in meters)}
    \item{\code{food}}{a factor with levels \code{Fish}
      \code{Invertebrates} \code{Other} representing the food preference}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Agresti, A. (2007). An Introduction to Categorical Data Analysis. Wiley
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(alligator)
library(VGAM)
model <- vglm(food ~ length, family=multinomial, data=alligator)
summary(model)
}
\keyword{datasets}
