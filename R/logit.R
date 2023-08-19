#' Compute the logit
#'
#' Compute the logit of a probability
#'
#' @param p a probability between 0 and 1
#'
#' @return A number with list with class \code{htest} containing the following components:
#'
#' @author Claus Ekstrom \email{ekstrom@@sund.ku.dk}
#'
#' @keywords utils
#' @export logit
logit <- function(p) {
  if (!is.numeric(p))
    stop("input must be numeric")
  
  return(log(p) - log(1-p))
}
