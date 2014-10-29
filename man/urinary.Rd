\name{urinary}
\alias{urinary}
\docType{data}
\title{Feline urinary tract disease}
\description{The impact of food intake and exercise as possible
  explanatory variables for the urinary tract disease in cats.
}
\usage{data(urinary)}
\format{
  A data frame with 74 observations on the following 3 variables.
  \describe{
    \item{\code{disease}}{a factor with levels \code{no} \code{yes}}
    \item{\code{food}}{a factor with levels \code{excessive} \code{normal}}
    \item{\code{exercise}}{a factor with levels \code{little} \code{much}}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Willeberg P (1976). Interaction effects of epidemiologic factors in the feline urological syndrome. Nordisk Veterinaer Medicin, 28, 193-200
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(urinary)
head(urinary)
}
\keyword{datasets}
