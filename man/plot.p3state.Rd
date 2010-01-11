\name{plot.p3state}
\alias{plot.p3state}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Method for an p3state object}
\description{
Plot method for an object of class 'p3state'. It draws the estimated transition probabilities, bivariate distribution of the gap times and marginal distribution of the second gap time (the last two only available for the progressive three-state model)
}
\usage{
plot.p3state(x, plot.trans = NULL, plot.marginal = NULL, 
plot.bivariate = NULL, time1, time2, xlab, ylab, zlab, col, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class p3state.  }
  \item{plot.trans}{ Graphical output for the transition probabilities. By default, 				plot.trans=FALSE. Possible values are: "all", "P11",  "P12",  "P22" and 			"P23". }
  \item{plot.marginal}{ Graphical output for the marginal distribution of the second time (only 			available for the progressive three-state model). By default, 				plot.marginal=FALSE. }
  \item{plot.bivariate}{ Graphical output for the bivariate distribution (only available for the 			progressive three-state model). By default, plot.bivariate=FALSE. }
  \item{time1}{The first time for obtaining estimates for the transition probabilities, 			bivariate distribution function. NULL is equivalent to 0. }
  \item{time2}{The second time for obtaining estimates for the bivariate distribution 			function.  }
  \item{xlab}{x-axix label. }
  \item{ylab}{y-axix label. }
  \item{zlab}{z-axix label (only for the bivariate distribution).}
  \item{col}{Colour for the bivariate plot.} 
 \item{\dots}{Further arguments for plot }
}




\author{ Luis Felipe Meira-Machado and Javier Roca-Pardinas }

\seealso{ \code{\link{p3state}}}
\examples{
data(heart2)
res.p3state<-p3state(heart2)
##Only transition probabilities
plot(res.p3state,plot.trans="all",time1=20,time2=100)

##Example of three-state model. All plots.
p<-which((heart2$delta==0 & heart2$status==0) | heart2$delta==1)
inputdata<-heart2[p,]
res2.p3state<-p3state(inputdata)
plot(res2.p3state,plot.trans="all",time1=20,
time2=200,plot.bivariate=TRUE,plot.marginal=TRUE)

}
