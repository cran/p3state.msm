\name{data.creation.reg}
\alias{data.creation.reg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Regression Dataset }
\description{
  Returns the input data in a different format. Provides the adequate dataset for implementing regression models. 
}
\usage{
data.creation.reg(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data.frame with at least 5 variables: times1 (time of the intermediate 			event/censoring time), delta (indicator of transition to the intermediate 			event), times2 (time to the final event/censoring time), time (times1 + 			times2) and status (censoring indicator: "dead"=1,"alive"=0). The 			remaining variables in the data.frame are left for the covariates.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
A data.frame in a counting process format.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Luis Felipe Meira-Machado and Javier Roca-Pardinas }
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
