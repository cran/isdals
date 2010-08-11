\name{logit}
\alias{logit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate the logit transform}
\description{
  Calculate the logit transform
}
\usage{
logit(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{numeric vector}
}
\details{
  Calculates the logit transform of p, ie., log(p/(1-p))
}
\value{
  Calculates the logit transform of p
}
\author{Claus Ekstrom <ekstrom@life.ku.dk>}
%\note{
%}
% \seealso{\code{\link{rstandard}}, \code{\link{predict}}}
\examples{
p <- 0.3
logit(p)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
