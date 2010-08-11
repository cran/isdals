\name{vitamina}
\alias{vitamina}
\docType{data}
\title{Food intake for Danish people 1985}
\description{
The daily food intake was studied for 2224 subjects, and the content of many different vitamins and substances were meaured,
}
\usage{data(vitamina)}
\format{
  A data frame with 2224 observations on the following 20 variables.
  \describe{
    \item{\code{person}}{subject id (a numeric vector)}
    \item{\code{wt}}{weight (kg)}
    \item{\code{ht}}{height (cm)}
    \item{\code{sex}}{sex: 1 for male, 2= for female}
    \item{\code{age}}{age}
    \item{\code{bmr}}{basal metabolic rate}
    \item{\code{E_bmr}}{energy divided by bmr}
    \item{\code{energi}}{energy content (kJ)}
    \item{\code{Avit}}{vitamin A (RE)}
    \item{\code{retinol}}{retinol (microgram)}
    \item{\code{betacar}}{beta-caroten (microgram)}
    \item{\code{Dvit}}{vitamin D (microgram}
    \item{\code{Evit}}{vitamin E (alphaTE)}
    \item{\code{B1vit}}{vitamin B1 (milligram)}
    \item{\code{B2vit}}{vitamin B2 (milligram)}
    \item{\code{niacin}}{niacin (NE)}
    \item{\code{B6vit}}{vitamin B6 (milligram)}
    \item{\code{folacin}}{folacin (microgram)}
    \item{\code{B12vit}}{vitamin B12 (microgram)}
    \item{\code{Cvit}}{vitamin C (milliggram)}
  }
}
\details{
Only variables Avit and bmr are used in the isdals book.
}
\source{
J. Haraldsdottir, J.H. Jensen, A. Moller (1985).
Danskernes kostvaner 1985, Hovedresultater.
Levnedsmiddelstyrelsen, publikation nr.\ 138.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(vitamina)
}
\keyword{datasets}
