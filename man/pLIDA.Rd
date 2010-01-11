\name{pLIDA}
\alias{pLIDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Transition probabilities}
\description{
  Computation of the transition probabilities.
}
\usage{
pLIDA(object, time1, time2,tp=NULL)
}

\arguments{
  \item{object}{Component datafr of an object of class p3state. }
  \item{time1}{ The first time for obtaining estimates for the transition probabilities, 			bivariate distribution function. NULL is equivalent to 0.}
  \item{time2}{ The second time for obtaining estimates for the bivariate distribution 			function. }
 \item{tp}{Optional argument: tp="all" (default value) to obtain all the transition probabilities p11, p12 and p22; tp="p11" to obtain only p11;  tp="p12" to obtain only p12;tp="p22" to obtain only p22. }
}

 
\seealso{ \code{\link{p3state}} }
\examples{
data(heart2)
res.p3state<-p3state(heart2)
pLIDA(res.p3state,time1=30,time2=300)
}