\name{cancer2}
\alias{cancer2}
\docType{data}
\title{Tumor size and emission of radioactivity}
\description{
An experiment involved 21 cancer tumors. For each tumor the weight
was registered as well as the emitted radioactivity obtained with a special
medical technique (scintigraphic images). Three data points from large tumors were removed.
}
\usage{data(cancer2)}
\format{
  A data frame with 18 observations on the following 3 variables.
  \describe{
    \item{\code{id}}{tumor id (numeric)}
    \item{\code{tumorwgt}}{tumor weight}
    \item{\code{radioact}}{emitted radioactivity (numeric)}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Shin et al. (2005). Noninvasive imaging for monitoring of viable cencer cells using a dual-imaging reporter gene. The Journal of Nuclear Medicine, 45, 2109-2115. 
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(cancer2)
}
\keyword{datasets}
