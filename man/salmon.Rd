\name{salmon}
\alias{salmon}
\docType{data}
\title{Parasite counts for salmons}
\description{
An experiment with two difference salmon stocks, from River Conon in Scotland
and from River Atran in Sweden, was carried out.
Thirteen fish from each stock were infected and after
four weeks the number of a certain type of parasites was counted for each of
the 26 fish.
}
\usage{data(salmon)}
\format{
  A data frame with 26 observations on the following 2 variables.
  \describe{
    \item{\code{stock}}{origin of the fish. A factor with levels \code{atran} \code{conon}}
    \item{\code{parasites}}{a numeric vector with the parasite counts}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Heinecke, R. D, Martinussen, T. and Buchmann, K. (2007).
Microhabitat selection of Gyrodactylus salaris Malmberg on different salmonids.
Journal of Fish Diseases, 30, 733-743.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(salmon)
}
\keyword{datasets}
