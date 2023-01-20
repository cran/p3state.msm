\encoding{UTF-8}
\name{data.creation.reg}
\alias{data.creation.reg}
\title{Regression dataset}

\description{
Returns the input data in a different format.
Provides the adequate dataset for implementing regression models.
}

\usage{data.creation.reg(data)}

\arguments{
  \item{data}{A data.frame with at least 5 variables:
  times1 (time of the intermediate event/censoring time),
  delta (indicator of transition to the intermediate event),
  times2 (time to the final event/censoring time),
  time (times1 + times2)
  and status (censoring indicator: "dead"=1,"alive"=0).
  The remaining variables in the data.frame are left for the covariates.}
}

\value{A data.frame in a counting process format.}

\author{Luis Meira-Machado, Javier Roca-Pardinas and Artur Araújo}

\references{
Meira-Machado L., Roca-Pardinas J. (2011). 
p3state.msm: Analyzing Survival Data from an Illness-Death Model. 
\emph{Journal of Statistical Software}, \bold{38}(3), 1-18. \doi{10.18637/jss.v038.i03}
}

%\seealso{
%}

%\examples{
%}

\keyword{manip}
\keyword{survival}
