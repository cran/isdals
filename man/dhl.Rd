\name{dhl}
\alias{dhl}
\docType{data}
\title{Running times from relay race}
\description{
Running times from 5 times 5 km relay race in Copenhagen 2006, held
over four days.  The sex distribution in the team classifies the teams
into six groups. Total running time for a team (not each participant)
is registered.
}
\usage{data(dhl)}
\format{
  A data frame with 24 observations on the following 6 variables.
  \describe{
    \item{\code{day}}{race day. A factor with levels \code{Monday} \code{Thursday} \code{Tuesday} \code{Wednesday}}
    \item{\code{men}}{number of men on the team (numeric)}
    \item{\code{women}}{number of men on the team (numeric)}
    \item{\code{hours}}{hours of running (should be combined with minutes and seconds)}
    \item{\code{minutes}}{minutes of running (should be combined with hours and seconds)}
    \item{\code{seconds}}{seconds of running (should be combined with hours and minutes)}
  }
}
\details{
The total running time for the team (not for each
participant) is registered.  On average, there are 800 teams per
combination of race day and sex group.  The dataset contains median running times.
}
\source{
http://www.sparta.dk
}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(dhl)
attach(dhl)
totaltime <- 60*60*hours + 60*minutes + seconds ## Total time in seconds
}
\keyword{datasets}
