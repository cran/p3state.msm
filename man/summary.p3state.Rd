\encoding{UTF-8}
\name{summary.p3state}
\alias{summary.p3state}
\title{Summary Methods for an p3state Object}

\description{
Provides results for an object of class \sQuote{p3state}.
It gives the estimated transition probabilities,
bivariate distribution of the gap times
and marginal distribution of the second gap time
(the last two only available for the progressive three-state model).
Also provides the results for the fit of semi-parametric Cox regression models.
}

\usage{
\S3method{summary}{p3state}(object, model = NULL, covmat = NULL,
estimate = NULL, time1 = NULL, time2 = NULL, ...)
}

\arguments{
  \item{object}{An object of class \sQuote{p3state}.}
  \item{model}{A character string specifying which model(s) to fit.
  Possible values are "TDCM", "CMM" and "CSMM".
  If NULL none of the regression models will be implemented.}
  \item{covmat}{Return the variance-covariance matrices?
  By default covmat=FALSE.}
  \item{estimate}{If TRUE nonparametric estimates are given.
  These include: transition probabilities,
  bivariate distribution function and marginal distribution of the second time
  (the last two only for the progressive three-state model).}
  \item{time1}{The first time for obtaining estimates
  of the transition probabilities,
  bivariate distribution function. NULL is equivalent to 0.}
  \item{time2}{The second time for obtaining estimates
  of the bivariate distribution function.}
  \item{...}{Further arguments for summary.}
}

\value{No value is returned.}

\author{Luis Meira-Machado, Javier Roca-Pardinas and Artur Araújo}

\references{
Meira-Machado L., Roca-Pardinas J. (2011). 
p3state.msm: Analyzing Survival Data from an Illness-Death Model. 
\emph{Journal of Statistical Software}, \bold{38}(3), 1-18. \doi{10.18637/jss.v038.i03}
}

\seealso{
\code{\link{p3state}}
}

\examples{
data(heart2)
res.p3state<-p3state(heart2, formula=~age+year)
summary(res.p3state,model="CMM",time1=20,time2=100)
}

\keyword{methods}
\keyword{models}
\keyword{nonparametric}
\keyword{survival}
