\name{OORdata}
\alias{OORdata}
\docType{data}
\title{pH and enzyme activity}
\description{A new enzyme, OOR, makes it possible for a certain bacteria species to develop on oxalate. In an experiment the enzyme activity (micromole per minute per mg) was measured and registered for 29 different pH-values.}
\usage{data(OORdata)}
\format{
  A data frame with 29 observations on the following 2 variables.
  \describe{
    \item{\code{ph}}{pH value (a numeric vector)}
    \item{\code{act}}{enzyme activity measured in micromole per minute per mg (a numeric vector)}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{Pierce, E., Becker, D. F., and Ragsdale, S. W. (2010). Identification and characterization of oxalate oxidoreductase, a novel thiamine pyrophosphate- dependent 2-oxoacid oxidoreductase that enables anaerobic growth on oxalate. Journal of Biological Chemistry, 285:40515-40524.}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(OORdata)
%## maybe str(OORdata) ; plot(OORdata) ...
}
\keyword{datasets}
