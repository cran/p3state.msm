\name{p11LIDA}
\alias{p11LIDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Transition probabilities}
\description{
  Computation of the transition probability p11
}
\usage{
p11LIDA(object, time1, time2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Component datafr of an object of class p3state. }
  \item{time1}{ The first time for obtaining estimates for the transition probabilities, 			bivariate distribution function. NULL is equivalent to 0.}
  \item{time2}{ The second time for obtaining estimates for the bivariate distribution 			function. }
}

%\details{
%  ~~ If necessary, more details than the description above ~~
%}



%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{component datafr of an object of class p3state. }
%  \item{comp2 }{The first time for obtaining estimates for the transition probabilities, 			%bivariate distribution function. NULL is equivalent to 0.}
%\item{comp3 } 		The second time for obtaining estimates for the bivariate distribution 			%function. 
%}
%\references{ ~put references to the literature/web site here ~ }

\author{ Luis Meira-Machado and Javier Roca Pardinas}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{p3state}} }
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as

data(heart2)
res.p3state<-p3state(heart2)
p11LIDA(res.p3state,time1=30,time2=300)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.