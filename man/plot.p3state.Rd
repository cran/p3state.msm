\encoding{UTF-8}
\name{plot.p3state}
\alias{plot.p3state}
\title{Plot Method for an p3state object}

\description{
Plot method for an object of class \sQuote{p3state}.
Draws the estimated transition probabilities,
bivariate distribution of the gap times
and marginal distribution of the second gap time
(the last two only available for the progressive three-state model)
}

\usage{
\S3method{plot}{p3state}(x, plot.trans = NULL, plot.marginal = NULL,
plot.bivariate = NULL, time1, time2, xlab, ylab, zlab, col, col.biv = NULL, ...)
}

\arguments{
  \item{x}{An object of class \sQuote{p3state}.}
  \item{plot.trans}{Graphical output for the transition probabilities.
  By default, plot.trans=FALSE.
  Possible values are: "all", "P11",  "P12",  "P22" and "P23".}
  \item{plot.marginal}{Graphical output for the marginal distribution
  of the second time (only available for the progressive three-state model).
  By default, plot.marginal=FALSE.}
  \item{plot.bivariate}{Graphical output for the bivariate distribution
  (only available for the progressive three-state model).
  By default, plot.bivariate=FALSE.}
  \item{time1}{The first time for obtaining estimates
  of the transition probabilities,
  bivariate distribution function. NULL is equivalent to 0.}
  \item{time2}{The second time for obtaining estimates
  of the bivariate distribution function.}
  \item{xlab}{x-axix label.}
  \item{ylab}{y-axix label.}
  \item{zlab}{z-axix label (only for the bivariate distribution).}
  \item{col}{Colour for the bivariate plot.}
  \item{col.biv}{A logical variable indicating whether
  you want color to be used in the filled.contour plot.
  By default col.biv = FALSE.}
 \item{\dots}{Further arguments for plot.}
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

\keyword{hplot}
\keyword{methods}
\keyword{models}
\keyword{nonparametric}
\keyword{survival}
