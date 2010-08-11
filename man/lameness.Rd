\name{lameness}
\alias{lameness}
\docType{data}
\title{Lameness scores for horses}
\description{
A score measuring the symmetry of the gait for eight trotting horses. Each horse was tested twice, namely while it was clinically healthy and after mechanical induction of lameness in a fore limb.
}
\usage{data(lameness)}
\format{
  A data frame with 8 observations on the following 3 variables.
  \describe{
    \item{\code{horse}}{a numeric vector with an id of the horse}
    \item{\code{lame}}{the symmetry score when the horse is lame}
    \item{\code{healthy}}{the symmetry score when the horse is healthy}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{A.T. Jensen, H. Sorensen, M.H. Thomsen and P.H. Andersen (2010).
Quantification of symmetry for functional data with application to equine lameness classification. Submitted manuscript.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(lameness)
}
\keyword{datasets}
