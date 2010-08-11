\name{cooling}
\alias{cooling}
\docType{data}
\title{Tenderness of pork}
\description{
  Two different cooling methods for pork meat were compared in an
  experiment with 18 pigs from two different groups: low or high pH
  content. After slaughter, each pig was split in two and one side was
  exposed to rapid cooling while the other was put through a cooling
  tunnel. After the experiment, the tenderness of the meat was
  measured.
}
\usage{data(cooling)}
\format{
  A data frame with 18 observations on the following 4 variables.
  \describe{
    \item{\code{pig}}{a numeric vector with the id of the pig}
    \item{\code{ph}}{pH concentration level. A factor with levels \code{high} \code{low}}
    \item{\code{tunnel}}{Tenderness observed from tunnel cooling}
    \item{\code{rapid}}{Tenderness observed from rapid cooling}
  }
}
%\details{
%
%}
\references{
  A. J. Moller and E. Kirkegaard and T. Vestergaard
  (1987). Tenderness of Pork Muscles as Influenced by Chilling Rate and
  Altered Carcass Suspension. Meat Science, 27, p. 275--286.
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(cooling)
hist(cooling$tunnel[cooling$ph=="low"], main="", xlab="Tenderness (low pH)", col="lightgray", ylim=c(0,5), xlim=c(3,9))
hist(cooling$tunnel[cooling$ph=="high"], main="", xlab="Tenderness (high pH)", col="lightgray", ylim=c(0,5), xlim=c(3,9))

hist(cooling$tunnel[cooling$ph=="low"], freq=FALSE, main="", xlab="Tenderness (low pH)", col="lightgray", ylim=c(0,.5), xlim=c(3,9))
hist(cooling$tunnel[cooling$ph=="high"], freq=FALSE, main="", xlab="Tenderness (high pH)", col="lightgray", ylim=c(0,.5), xlim=c(3,9))

plot(cooling$tunnel, cooling$rapid,
     xlim=c(3,9), ylim=c(3,9),
     xlab="Tenderness (tunnel)", ylab="Tenderness (rapid)")

boxplot(cooling$tunnel, cooling$rapid, names=c("Tunnel", "Rapid"),
        ylab="Tenderness score")
}
\keyword{datasets}

