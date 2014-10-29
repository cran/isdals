\name{soap}
\alias{soap}
\docType{data}
\title{Quality of soap}
\description{The electric conductance was measured for 32 pieces of soap in 4 groups (8 pieces in each group). The content of fatty acid differs between the groups. Quality of soap is mainly determined by its content of fatty acid, which can be determined with a chemical analysis. It is much easier to measure the electric conductance, and it is therefore of interest if there is a simple relation between the two.}
\usage{data(soap)}
\format{
  A data frame with 32 observations on the following 3 variables.
  \describe{
    \item{\code{group}}{the groups of soap (notice: numeric vector, not factor)}
    \item{\code{fattyacid}}{content if fatty acid in percent (numeric vector)}
    \item{\code{conduct}}{electric conductance in milli Siemens (numeric vector)}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{Unknown}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(soap)
%## maybe str(soap) ; plot(soap) ...
}
\keyword{datasets}
