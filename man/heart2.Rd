\encoding{UTF-8}
\name{heart2}
\alias{heart2}
\docType{data}
\title{More Stanford heart transplant data}

\description{
This contains the Stanford heart transplant data in a different format.
The main data set is in (\code{\link{heart}}).
Survival of patients on the waiting list for the Stanford heart transplant program.
}

\usage{data(heart2)}

\format{
  A data frame with 103 observations on the following 8 variables.
  \describe{
    \item{\code{times1}}{Time of transplant/censoring time.}
    \item{\code{delta}}{Transplant indicator.}
    \item{\code{times2}}{Time to death since the transplant/censoring time.}
    \item{\code{time}}{times1 + times2}
    \item{\code{status}}{Censoring indicator: dead=1, alive=0.}
    \item{\code{age}}{Age-48 years.}
    \item{\code{year}}{Year of acceptance; in years after 1 Nov 1967.}
    \item{\code{surgery}}{Prior bypass surgery; 1=yes.}
  }
}

\references{
Crowley J., Hu M. (1977). Covariance analysis of heart transplant survival data. 
\emph{Journal of the American Statistical Association}, \bold{72}(357), 27-36. 
\doi{10.2307/2286902}
}

%\seealso{
%}

%\examples{
%data(heart2)
%## maybe str(heart2) ; plot(heart2) ...
%}

\keyword{datasets}
