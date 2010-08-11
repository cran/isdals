\name{herring}
\alias{herring}
\docType{data}
\title{Nematodes in herring fillets}
\description{
  An experiment was carried out in order to investigate the migration of
  nematodes in Danish herrings. The fish were allocated to eight
  different treatment groups corresponding to different combinations of
  storage time and storage conditions until filleting.  After filleting,
  it was determined whether nematodes were present in the fillet or not.
}
\usage{data(herring)}
\format{
  A data frame with 884 observations on the following 4 variables.
  \describe{
    \item{\code{group}}{a numeric vector that is the combination of
      storage and time}
    \item{\code{time}}{a numeric vector that contains the duration of
      storage in hours before the fish is filleted}
    \item{\code{condi}}{a numeric vector representing the storage condition}
    \item{\code{fillet}}{a numeric vector to indicate the presence of
      nematodes (1) or absence of nematodes (0)}
  }
}
\details{
 The variable group is the combination of storage condition and
  storage time.  Notice that a storage time 0 is equivalent to storage
  condition 0 and that no fish were stored 132 hours under condition
  4. Hence, there are only 8 combinations; i.e., 8 levels of the group
  variable.
}
  
\source{    
  A. Roepstorff and H. Karl and B. Bloemsma and H. H. Huss (1993).
  Catch handling and the possible migration of Anisakis larvae in
  herring, Clupea harengus. Journal of Food Protection. 56:783-787.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(herring)
## maybe str(herring) ; plot(herring) ...
}
\keyword{datasets}
