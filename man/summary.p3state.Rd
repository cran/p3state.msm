\name{summary.p3state}
\alias{summary.p3state}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Summary Methods for an p3state Object}
\description{
  Provides results for an object of class \code{p3state}. It gives the estimated transition probabilities, bivariate distribution of the gap times and marginal distribution of the second gap time (the last two only available for the progressive three-state model). Provides also the results for the fit of semi-parametric Cox regression models.
}
\usage{
summary.p3state(object, model = NULL, covmat = NULL, 
estimate = NULL, time1 = NULL, time2 = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{p3state}.  }
  \item{model}{ A character string specifying which model(s) to fit. Possible values are 			"TDCM", "CMM" and "CSMM". If NULL none of the regression 			models will be implemented. }
  \item{covmat}{ Return the variance-covariance matrices? By default covmat=FALSE. }
  \item{estimate}{ If TRUE nonparametric estimates are given. These include: 				transition probabilities, bivariate distribution function and marginal 			distribution of the second time (the last two only for the progressive 			three-state model).  }
  \item{time1}{ The first time for obtaining estimates for the transition probabilities, 			bivariate distribution function. NULL is equivalent to 0. }
  \item{time2}{ The second time for obtaining estimates for the bivariate distribution 			function. }
  \item{...}{ Further arguments for summary. }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
%\references{ ~put references to the literature/web site here ~ }
\author{Luis Meira-Machado and Javier Roca-Pardinas   }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{p3state}}}
\examples{
data(heart2)
res.p3state<-p3state(heart2, formula=~age+year)
summary(res.p3state,model="CMM",time1=20,time2=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

