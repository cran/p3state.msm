\encoding{UTF-8}
\name{p3state}
\alias{p3state}
\title{Inference in progressive multi-state models with three states}

\description{
This function provides nonparametric estimates in progressive multi-state models
with three states (illness-death model and three-state model).
Also fits semi-parametric Cox models in a multi-state framework (one for each transition).
}

\usage{p3state(data, coxdata=NULL, formula=NULL, regression=NULL)}

\arguments{
  \item{data}{A data.frame in which to interpret the variables named in the covariates.
  A data frame with at least 5 variables:
  times1 (time of the intermediate event/censoring time),
  delta (indicator of transition to the intermediate event),
  times2 (time to the final event/censoring time),
  time (times1 + times2)
  and status (censoring indicator: "dead"=1, "alive"=0).
  The remaining variables in the data.frame are left for the covariates.}
  \item{coxdata}{Data set in a counting process data-structure.
  This data set can be obtained using \code{\link{data.creation.reg}}.
  If NULL the main function p3state will automatically create this dataset every time it is called.}
  \item{formula}{A formula giving the vector of covariates.
  For example \code{formula=~age+sex}}.
  \item{regression}{A logical variable indicating whether you want the regression model.}
}

\details{
Multi-state models may be considered a generalization of survival analysis
where survival is the ultimate outcome of interest but where
intermediate (transient) states are identified.
The influence of the intermediate events on survival may be investigated
through the effect of the time-dependent covariate
(using the Cox regression model with time-dependent covariates; TDCM).
However, these covariates can also be re-expressed as a multi-state model
with states based on the values of the covariate (typically coded as 1=yes; 0=no).
If all subjects observe the intermediate event then the time-dependent
covariate makes it possible to use the progressive three-state model.
Otherwise makes it feasible to use an illness-death model.
In these models, issues of interest include the estimation of transition
probabilities and assessing the effects of individual risk factors.
}

\value{
  Returns a list of the following items:
  \item{descriptives}{Vector with observed transitions between states.}
  \item{datafr}{data.frame to be used for obtaining the nonparametric estimates and for plotting purposes.}
  \item{tdcm}{Object of class \sQuote{coxph} with the fit of the Cox model with time-dependent covariates.}
  \item{msm12}{Object of class \sQuote{coxph} with the fit of the Cox model for transition from state 1 to 	state 2.}
  \item{msm13}{Object of class \sQuote{coxph} with the fit of the Cox model for transition from state 1 to state 3
  (only for the progressive three-state model).}
  \item{cmm23}{Object of class \sQuote{coxph} with the fit of the Cox Markov model for transition from state 2 to state 3.}
  \item{tma}{Object of class \sQuote{coxph} with the fit of a Cox model for testing the Markov assumption.}
}

\references{
Meira-Machado L., De Una-Alvarez J., Cadarso-Suarez C. (2006). 
Nonparametric estimation of transition probabilities in a non-Markov illness-death model. 
\emph{Lifetime Data Analysis}, \bold{12}(3), 325-344. \doi{10.1007/s10985-006-9009-x}

de Una-Alvarez J., Meira-Machado L. (2008). 
A simple estimator of the bivariate distribution function for censored gap times. 
\emph{Statistics & Probability Letters}, \bold{78}(15), 2440-2445. \doi{10.1016/j.spl.2008.02.031}

Meira-Machado L., Roca-Pardinas J. (2011). 
p3state.msm: Analyzing Survival Data from an Illness-Death Model. 
\emph{Journal of Statistical Software}, \bold{38}(3), 1-18. \doi{10.18637/jss.v038.i03}
}

\author{Luis Meira-Machado, Javier Roca-Pardinas and Artur Araújo}

\examples{
data(heart2)
res.p3state <- p3state(heart2, formula=~age+year+surgery)
summary(res.p3state)

##Only regression
summary(res.p3state, model="TDCM")
summary(res.p3state, model="CMM")

##without regression
summary(res.p3state, time1=20, time2=200)

##Both
summary(res.p3state, estimate=TRUE, time1=20, time2=200, model="CMM")

##Just for illustration purposes we create a new subset by restricting 
##the original data set from those subjects experiencing the transplant
## (progressive three-state model)
p <- which((heart2$delta==0 & heart2$status==0) | heart2$delta==1)
exampledata <- heart2[p,]
res2.p3state <- p3state(exampledata)
summary(res2.p3state)
}

\keyword{models}
\keyword{nonparametric}
\keyword{survival}
