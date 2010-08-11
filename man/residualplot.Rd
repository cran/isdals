\name{residualplot}
\alias{residualplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots a standardaized residual}
\description{
  Plots a standardized residual plot from an lm object and provides
  additional graphics to help evaluate the variance homogeneity and mean.
}
\usage{
residualplot(object, bandwidth = 0.3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{lm object}
  \item{bandwidth}{The width of the window used to calculate the local
    smoothed version of the mean and the variance. Value should be
    between 0 and 1 and determines the percentage of the windowwidth
    used}
  \item{...}{Other arguments passed to the plot function}
}
\details{
  Plots a standardized residual plot from an lm object and provides
  additional graphics to help evaluate the variance homogeneity and
  mean.

  The brown area is a smoothed estimate of 1.96*SD of the standardized
  residuals in a window around the predicted value. The brown area
  should largely be rectangular if the standardized residuals have more
  or less the same variance.

  The dashed line shows the smoothed mean of the standardized residuals
  and should generally follow the horizontal line through (0,0).
}
\value{
  Produces a standardized residual plot
}
\author{Claus Ekstrom <ekstrom@life.ku.dk>}
%\note{
%}
\seealso{\code{\link{rstandard}}, \code{\link{predict}}}
\examples{
# Linear regression example
x <- rnorm(100)
y <- rnorm(100, mean=.5*x)
model <- lm(y ~ x)
residualplot(model)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
