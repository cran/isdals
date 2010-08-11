\name{bodyfat}
\alias{bodyfat}
\docType{data}
\title{Body fat in women}
\description{
It is expensive and cumbersome to
determine the body fat in humans as it involves immersion of the
person in water. This dataset provides
information on body fat, triceps skinfold thickness, thigh
circumference, and mid-arm circumference for twenty healthy females
aged 20 to 34.  It is desirable if a model  could provide reliable
predictions of the amount of body fat, since the measurements needed for
the predictor variables are easy to obtain. 
}
\usage{data(bodyfat)}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{Fat}}{body fat}
    \item{\code{Triceps}}{triceps skinfold measurement}
    \item{\code{Thigh}}{thigh circumference}
    \item{\code{Midarm}}{mid-arm circumference}
  }
}
%\details{
%}
\source{
J. Neter and M.H. Kutner and C.J. Nachtsheim and W. Wasserman
(1996). Applied Linear Statistical Models. McGraw-Hill
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(bodyfat)
}
\keyword{datasets}
