\name{geneexp}
\alias{geneexp}
\docType{data}
\title{Gene expression}
\description{
Two groups were compared in an experiment with six microarrays. Two
conditions (the test group and the reference group) were examined on
each array and the amount of protein synthesized by the gene (also
called the gene expression) was registered.
}
\usage{data(geneexp)}
\format{
  A data frame with 6 observations on the following 3 variables.
  \describe{
    \item{\code{array}}{array id}
    \item{\code{test}}{gene expression level for test group}
    \item{\code{reference}}{gene expression level for reference group}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Fictious data.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(geneexp)
}
\keyword{datasets}
