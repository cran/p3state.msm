\encoding{UTF-8}
\name{pLIDA}
\alias{pLIDA}
\title{Transition probabilities}

\description{Computation of the transition probabilities.}

\usage{pLIDA(object, time1, time2,tp=NULL)}

\arguments{
  \item{object}{Component datafr of an object of class p3state.}
  \item{time1}{The first time for obtaining estimates for the transition probabilities,
  bivariate distribution function. NULL is equivalent to 0.}
  \item{time2}{The second time for obtaining estimates for the bivariate distribution function.}
  \item{tp}{Optional argument:
  tp="all" (default value) to obtain all the transition probabilities p11, p12 and p22;
  tp="p11" to obtain only p11;
  tp="p12" to obtain only p12;
  tp="p22" to obtain only p22.}
}

\value{
Returns a single value if argument \code{tp} equals "p11", "p12", or "p22".
Returns a list if argument \code{tp} equals "all".
}

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
res.p3state<-p3state(heart2)
pLIDA(res.p3state,time1=30,time2=300)
}

\keyword{nonparametric}
\keyword{survival}
