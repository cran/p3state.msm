\name{Biv}
\alias{Biv}
\title{ Bivariate distribution function}
\description{ Computation of the bivariate distribution function}

\usage{Biv(object, time1, time2)}
\arguments{
  \item{object}{ Component datafr of an object of class p3state. }
  \item{time1}{ The first time for obtaining estimates for the transition probabilities, 	
    bivariate distribution function. NULL is equivalent to 0.}
  \item{time2}{ The second time for obtaining estimates for the bivariate distribution function.  }
}


\author{ Luis Meira-Machado and Javier Roca-Pardinas  }

\seealso{ \code{\link{p3state}} }
\examples{
data(heart2)
res.p3state<-p3state(heart2)
Biv(res.p3state,time1=30,time2=300)
}
