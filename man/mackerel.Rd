\name{mackerel}
\alias{mackerel}
\docType{data}
\title{Nematodes in mackerel}
\description{
 Data to examine if cooling right after catching prevents
  nematodes (roundworms) from moving from the belly of mackerel to the
  fillet. A total of 150 mackerels were investigated and their length,
  number of nematodes in the belly, and time before counting the
  nematodes in the fillet were registered. The response variable is
  binary: presence or absence of nematodes in the fillet.
}
\usage{data(mackerel)}
\format{
  A data frame with 150 observations on the following 7 variables.
  \describe{
    \item{\code{length}}{a numeric vector}
    \item{\code{visc}}{a numeric vector}
    \item{\code{left}}{a numeric vector}
    \item{\code{right}}{a numeric vector}
    \item{\code{filet}}{a numeric vector}
    \item{\code{portion}}{a numeric vector}
    \item{\code{time}}{a numeric vector}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
 A. Roepstorff and H. Karl and B. Bloemsma and H. H. Huss (1993).
  Catch handling and the possible migration of Anisakis larvae in
  herring, Clupea harengus. Journal of Food Protection. 56:783-787.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(mackerel)
## maybe str(mackerel) ; plot(mackerel) ...
}
\keyword{datasets}
