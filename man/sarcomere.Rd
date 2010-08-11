\name{sarcomere}
\alias{sarcomere}
\docType{data}
\title{Sarcomere length and meat tenderness
}
\description{
The average sarcomere length in the meat and the corresponding
tenderness as scored by a panel of sensory judges was examined. A high
score corresponds to tender meat.
}
\usage{data(sarcomere)}
\format{
  A data frame with 24 observations on the following 3 variables.
  \describe{
    \item{\code{pig}}{factor with levels 1--24. Pid id}
    \item{\code{sarc.length}}{numeric  Sarcomere length}
    \item{\code{tenderness}}{numeric  Meat tenderness score}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
  A. J. Moller and E. Kirkegaard and T. Vestergaard
  (1987). Tenderness of Pork Muscles as Influenced by Chilling Rate and
  Altered Carcass Suspension. Meat Science, 27, p. 275--286.
}
\examples{
data(sarcomere)
cor(sarcomere$sarc.length, sarcomere$tenderness)
}
\keyword{datasets}
