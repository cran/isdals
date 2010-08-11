\name{birthweight}
\alias{birthweight}
\docType{data}
\title{Birth weight of boys and girls}
\description{
  Data from a study that was undertaken to
  investigate how the sex of the baby and the age of the fetus
  influence  birth weight during the last weeks of the pregnancy.
}
\usage{data(birthweight)}
\format{
  A data frame with 361 observations on the following 5 variables.
  \describe{
    \item{\code{sex}}{a factor with levels \code{male} \code{female}}
    \item{\code{age}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
  }
}
%\details{
%
%}
\source{
 Anette Dobson (2001). An Introduction to Generalized Linear Models (2nd
 ed.) Chapman and Hall.
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(birthweight)
## maybe str(birthweight) ; plot(birthweight) ...
}
\keyword{datasets}
