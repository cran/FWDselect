\name{selection}
\alias{selection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selecting a subset of \code{q} variables
}
\description{
Main function for selecting the best subset of \eqn{q} variables. Note that the selection procedure can be used with lm, glm or gam functions. 
}
\usage{
selection(x, y, q, criterion = "deviance", method = "lm", family = "gaussian", 
seconds = FALSE, nmodels = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame containing all the covariates. 
}
  \item{y}{A vector with the response values.}
  \item{q}{subset size or number of variables to be selected.}
  \item{criterion}{The information criterion to be used. Default is the deviance. Other provided functions are the coefficient of determination ("\code{R2}") or the residual variance ("\code{variance}").
}
  \item{method}{A character string specifying which regression method is used: linear models ("\code{lm}"), generalized additive models ("\code{glm}") or generalized additive models ("\code{gam}").
}
  \item{family}{A description of the error distribution and link function to be used in the model: "\code{gaussian}", "\code{binomial}" or "\code{poisson}".}

  \item{seconds}{A logical value. By default, \code{FALSE}.  If \code{TRUE} the function not only returns the single best model but rather the few best models.
}
  \item{nmodels}{Number of second models to be returned.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{Best model}{The best model. If \code{seconds=TRUE}, it returns also the best alternative models.}
  \item{Variable name}{Names of the variable.}
  \item{Variable number}{Number of the variables.}
  \item{Information criterion}{Information criterion used and its value.}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Marta Sestelo, Nora M. Villanueva and Javier Roca-Pardinas.}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

library(FWDselect)
data(emissionS02)

obj1=selection(x=emissionS02[,-19],y=emissionS02[,19],q=1,method="lm",criterion="deviance")
obj1

obj11=selection(x=emissionS02[,-19],y=emissionS02[,19],q=1,method="lm",criterion="deviance",
seconds=TRUE,nmodels=2)
obj11
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{variable selection}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
