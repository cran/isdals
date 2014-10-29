\name{listeria}
\alias{listeria}
\docType{data}
\title{Listeria growth in experiment with mice}
\description{
Ten wildtype mice and ten RIP2-deficient mice, i.e., mice without the RIP2 protein, were used in the experiment. Each mouse was infected with listeria, and after three days the bacteria growth was measured in the liver or spleen.
Errors were detected for two liver measurements, so the total number of observations is 18.}
\usage{data(listeria)}
\format{
  A data frame with 18 observations on the following 3 variables.
  \describe{
    \item{\code{organ}}{a factor with levels \code{liv} \code{spl} telling where the mesurement was taken}
    \item{\code{type}}{a factor with levels \code{rip2} \code{wild} corresponding to the mouse type}
    \item{\code{growth}}{bacteria growth}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{Anand, P. K., Tait, S. W. G., Lamkanfi, M., Amer, A. O., Nunez, G., Pagès, G., Pouysségur, J., McGargill, M. A., Green, D. R., and Kanneganti, T.-D. (2011). TLR2 and RIP2 pathways mediate autophagy of listeria monocytogenes via extracellular signal-regulated kinase (ERK) activation. Journal of Biological Chemistry, 286:42981-42991.}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(listeria)
}
\encoding{utf8}
\keyword{datasets}
