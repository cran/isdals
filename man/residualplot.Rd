% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residualplot.R
\name{residualplot}
\alias{residualplot}
\title{Plots a standardaized residual}
\usage{
residualplot(object, bandwidth = 0.3, ...)
}
\arguments{
\item{object}{an lm object}

\item{bandwidth}{The width of the window used to calculate the local smoothed version of the mean and the variance. Value should be between 0 and 1 and determines the percentage of the windowwidth used}

\item{...}{Arguments passed to plot.}
}
\value{
Produces a standardized residual plot
}
\description{
Plots a standardized residual plot from an lm object and provides additional graphics to help evaluate the variance homogeneity and mean.
}
\details{
Plots a standardized residual plot from an lm object and provides
additional graphics to help evaluate the variance homogeneity and mean.

The brown area is a smoothed estimate of 1.96*SD of the standardized
residuals in a window around the predicted value. The brown area
should largely be rectangular if the standardized residuals have more
or less the same variance.

The dashed line shows the smoothed mean of the standardized residuals
and should generally follow the horizontal line through (0,0).
}
\examples{
# Linear regression example
x <- rnorm(100)
y <- rnorm(100, mean=.5*x)
model <- lm(y ~ x)
residualplot(model)

}
\author{
Claus Ekstrøm <ekstrom@sund.ku.dk>
}
\keyword{hplot}
