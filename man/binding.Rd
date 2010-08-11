\name{binding}
\alias{binding}
\docType{data}
\title{Binding of antibiotics}
\description{
When an antibiotic is injected into the bloodstream, a certain part of it will
bind to serum protein.  This binding reduces the medical effect. As part of a
larger study, the binding rate was measured for 12 cows which were given one of
three types of antibiotics: chloramphenicol, erythromycin, and tetracycline
}
\usage{data(binding)}
\format{
  A data frame with 12 observations on the following 2 variables.
  \describe{
    \item{\code{antibiotic}}{antibiotic type. Factor with levels \code{Chlor} \code{Eryth} \code{Tetra}}
    \item{\code{binding}}{binding rate}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
G. Ziv and F. G. Sulman (1972). Binding of antibiotics to bovine and ovine serum. Antimicrobial Agents and Chemotherapy, 2, 206-213.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(binding)
}
\keyword{datasets}
