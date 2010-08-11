\name{cuckoo}
\alias{cuckoo}
\docType{data}
\title{Hatching of cuckoo eggs}
\description{
Cuckoos place their eggs in other birds' nests for hatching and rearing.
Researchers investigated 154 cuckoo eggs and measured their size.  
The adoptive species is also registered (three types).
It is believed that cuckoos choose the ``adoptive parents''
such that the cuckoo eggs are similar in size to the 
eggs of the adoptive species. 
}
\usage{data(cuckoo)}
\format{
  A data frame with 154 observations on the following 2 variables.
  \describe{
    \item{\code{spec}}{adoptive species. Factor with levels \code{redstart} \code{whitethroat} \code{wren}}
    \item{\code{width}}{width of egg (unit: half millimeters)}
  }
}
%\details{
%}
\source{
O.H. Latter (1905). The egg of Cuculus Canorus: An attempt to ascertain from the dimensions of the cuckoo's egg if the species is tending to break up into sub-species, each exhibiting a preference for some one foster-parent. Biometrika, 4, 363-373.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(cuckoo)
}
\keyword{datasets}
