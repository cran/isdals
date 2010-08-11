\name{hazard}
\alias{hazard}
\docType{data}
\title{Sorption of hazardous organic solvents}
\description{
The sorption was
measured for a variety of hazardous organic solvents. The
solvents were classified into three types (esters, aromatics, and
chloroalkanes), and the purpose was to examine differences
between the three types. 
}
\usage{data(hazard)}
\format{
  A data frame with 32 observations on the following 2 variables.
  \describe{
    \item{\code{type}}{type of solvent. Factor with levels \code{aromatic} \code{chlor} \code{estere}}
    \item{\code{sorption}}{sorption measurements}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
J.D. Ortego, T.M Aminabhavi, S.F. Harlapur, R.H. Balundgi (1995).
A review of polymeric geosynthetics used in hazardous waste facilities.
Journal of Hazardous Materials, 42, 115-156. 
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(hazard)
}
\keyword{datasets}
