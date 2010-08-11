\name{massspec}
\alias{massspec}
\docType{data}
\title{Comparison of mass spectrometry methods
}
\description{
Two common methods are GC-MS (gas chromatography-mass
spectrometry) and HPLC (high performance liquid chromatography). The
biggest difference between the two methods is that one uses gas while
the other uses liquid. We wish to determine if the
two methods measure the same amount of muconic acid in human urine.
}
\usage{data(massspec)}
\format{
  A data frame with 16 observations on the following 3 variables.
  \describe{
    \item{\code{sample}}{a numeric vector}
    \item{\code{hplc}}{a numeric vector}
    \item{\code{gcms}}{a numeric vector}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(massspec)
}
\keyword{datasets}
