\name{lucerne}
\alias{lucerne}
\docType{data}
\title{Fertility of lucerne}
\description{
Ten plants were used in an experiment of fertility of lucerne Two
clusters of flowers were selected from each plant and pollinated.  One
cluster was bent down, whereas the other was exposed to wind and sun.
At the end of the experiment, the average number of seeds per pod was
counted for each cluster and the weight of 1000 seeds was registered
for each cluster.
}
\usage{data(lucerne)}
\format{
  A data frame with 10 observations on the following 5 variables.
  \describe{
    \item{\code{plant}}{plant id}
    \item{\code{seeds.exp}}{average number of seeds per pod from cluster exposed to sun and wind}
    \item{\code{wgt.exp}}{weight of 1000 seeds from cluster exposed to sun and wind}
    \item{\code{seeds.bent}}{average number of seeds per pod from cluster that was bent down}
    \item{\code{wgt.bent}}{weight of 1000 seeds from cluster that was bent down}
  }
}
%\details{
%}
\source{
H.L. Petersen (1954). Pollination and seed setting in lucerne.
Kgl. Veterinaer og Landbohojskole, Aarsskrift 1954, 138-169.
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(lucerne)
}
\keyword{datasets}
