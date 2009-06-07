\name{heart2}
\alias{heart2}
\docType{data}
\title{ More Stanford Heart Transplant data}
\description{
This contains the Stanford Heart Transplant data in a different format. The main data set is in (\code{\link{heart}}). 
Survival of patients on the waiting list for the Stanford heart transplant program. 
%A data frame with 103 observations and 8 variables: times1 (time of transplant/censoring time), delta (transplant indicator), times2 (time to death since the transplant/censoring time), time (times1 + times2), status (censoring indicator: dead=1, alive=0), age (age-48 years), year (year of acceptance; in years after 1 Nov 1967) and surgery (prior bypass surgery; 1=yes). 
}
\usage{data(heart2)}
\format{
  A data frame with 103 observations on the following 8 variables.
  \describe{
    \item{\code{times1}}{time of transplant/censoring time}
    \item{\code{delta}}{transplant indicator}
    \item{\code{times2}}{time to death since the transplant/censoring time}
    \item{\code{time}}{times1 + times2}
    \item{\code{status}}{censoring indicator: dead=1, alive=0}
    \item{\code{age}}{age-48 years}
    \item{\code{year}}{year of acceptance; in years after 1 Nov 1967}
    \item{\code{surgery}}{prior bypass surgery; 1=yes}
  }
}

%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%J Crowley and M Hu (1977), Covariance analysis of heart transplant survival data. Journal of the American Statistical Association, 72, 27-36.
%}


\references{
J Crowley and M Hu (1977), Covariance analysis of heart transplant survival data. Journal of the American Statistical Association, 72, 27-36.
}

\seealso{ \code{\link{heart}} }
%\examples{
%data(heart2)
%## maybe str(heart2) ; plot(heart2) ...
%}
\keyword{datasets}
