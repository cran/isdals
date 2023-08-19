#' Plots a standardaized residual
#'
#' Plots a standardized residual plot from an lm object and provides additional graphics to help evaluate the variance homogeneity and mean.
#'
#' Plots a standardized residual plot from an lm object and provides
#' additional graphics to help evaluate the variance homogeneity and mean.
#'
#' The brown area is a smoothed estimate of 1.96*SD of the standardized
#' residuals in a window around the predicted value. The brown area
#' should largely be rectangular if the standardized residuals have more
#' or less the same variance.
#'
#' The dashed line shows the smoothed mean of the standardized residuals
#' and should generally follow the horizontal line through (0,0).
#'
#' @param object an lm object
#' @param bandwidth The width of the window used to calculate the local smoothed version of the mean and the variance. Value should be between 0 and 1 and determines the percentage of the windowwidth used
#' @param ... Arguments passed to plot.
#'
#' @return Produces a standardized residual plot
#'
#' @author Claus Ekstrøm <ekstrom@@sund.ku.dk>
#'
#' @examples
#' # Linear regression example
#' x <- rnorm(100)
#' y <- rnorm(100, mean=.5*x)
#' model <- lm(y ~ x)
#' residualplot(model)
#'
#' @keywords hplot
#' @importFrom stats complete.cases predict rstandard sd smooth.spline
#' @importFrom graphics lines points polygon
#' @importFrom grDevices rgb
#'
#' @export
residualplot <- function(object, bandwidth=.3, ...) {
  # Check that input is correct
  if (!inherits(object, "lm")) 
    stop("use only with \"lm\" objects")

  x <- predict(object)
  y <- rstandard(object)

  cc <- complete.cases(x, y)
  x <- x[cc]
  y <- y[cc]
  
  plot(x, y, 
       xlab="Fitted values", ylab="Standardized residuals", ...)
  # Make "outliers" black
  outliers <- (abs(y)>1.96)
  points(x[outliers], y[outliers], pch=16)

  # Add mean smoothing spline
  # If there is at least 4 unique x values
  uniqx <- sort(unique(x))
  if (length(uniqx)>3) {
    lines(smooth.spline(x, y, df=3), lty=2, lwd=2, col = "black")
  }

  # Slow approach here. Should be in c
  window <- bandwidth*(max(x)-min(x))/2
  vary <- length(uniqx)
  i <- 1
  for (j in uniqx) {
    vary[i] <- 1.96*sd(y[abs(x-j)<=window])
    i <- i +1
  }
  vary[is.na(vary)] <- 0
  color <- rgb(237, 149, 100, 50, maxColorValue=255)
  polygon(c(uniqx, rev(uniqx)), c(vary, -(rev(vary))),
          col=color, border=NA)
#  lines(uniqx, vary, col="red")

}
