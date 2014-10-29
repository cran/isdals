\name{inhibitor}
\alias{inhibitor}
\docType{data}
\title{Enzyme experiment with inhibitors}
\description{The data comes from an enzyme experiment with inhibitors. The enzyme acts on a substrate that was tested in six concentrations between 10 micro M and 600 micro M. Three concentrations of the inhibitor were tested, namely 0 (controls), 50 micro M and 100 micro M. There were two replicates for each combination yielding a total of 36 observations of reaction rate.}
\usage{data(inhibitor)}
\format{
  A data frame with 36 observations on the following 3 variables.
  \describe{
    \item{\code{Iconc}}{Inhibitor concentration in micro Mole (numeric vector)}
    \item{\code{Sconc}}{Substrate concentration in micro Mole (numeric vector)}
    \item{\code{RR}}{Reaction rate (numeric vector)}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{The experiment was carried out by students at a biochemistry course at University of Copenhagen.}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(inhibitor)
}
\encoding{utf8}
\keyword{datasets}
