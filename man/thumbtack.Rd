\name{thumbtack}
\alias{thumbtack}
\docType{data}
\title{Throwing thumbtacks}
\description{
  A brass thumbtack was thrown 100 times and it was registered whether
  the pin was pointing up or down towards the table upon landing.
}
\usage{data(thumbtack)}
\format{
  The format is:
 int [1:100] 1 1 0 0 1 1 0 1 0 0 ...
}
\details{
  1 corresponds to "tip pointing down" and 0 corresponds to "tip pointing up"
}
\references{
Mats Rudemo (1979). Statistik og sandsynlighedslaere med biologiske anvendelser. Del 1: Grundbegreber.
}
\examples{
data(thumbtack)
mean(thumbtack)
}
\keyword{datasets}
