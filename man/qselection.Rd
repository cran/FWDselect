\name{qselection}
\alias{qselection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selecting variables for several subset sizes
}
\description{
Function that enables to obtain the best variables for more than one size of subset. Returns a table with the chosen covariates to be introduced into the models and their information criteria. 
}
\usage{
qselection(x, y, qvector, criterion = "deviance", method = "lm", family = "gaussian")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A data frame containing all the covariates.  
}
  \item{y}{
A vector with the response values.}
  \item{qvector}{ A vector with the subset size.
}
  \item{criterion}{ The information criterion to be used. Default is the deviance. Other provided functions are the coefficient of determination ("\code{R2}") or the residual variance ("\code{variance}").
}
  \item{method}{A character string specifying which regression method is used: linear models ("\code{lm}"), generalized additive models ("\code{glm}") or generalized additive models ("\code{gam}").
}
  \item{family}{A description of the error distribution and link function to be used in the model: "\code{gaussian}", "\code{binomial}" or "\code{poisson}".
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{q}{A vector of subset sizes.}
  \item{criterion }{A vector of Information criterion values.}
  \item{selection }{Selected variables for each size.}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Marta Sestelo, Nora M. Villanueva and Javier Roca-Pardinas.
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{selection}}

\code{\link{plot.qselection}}
}
\examples{
library(FWDselect)
data(emissionS02)

obj2=qselection(x=emissionS02[,-19],y=emissionS02[,19],qvector=c(1:4),method="lm",
criterion="R2")
obj2
}
\keyword{subset}
%\keyword{selection}% __ONLY ONE__ keyword per line
