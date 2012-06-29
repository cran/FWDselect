\name{print.selection}
\alias{print.selection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Short \code{selection} summary
}
\description{
\code{\link{selection}} summary
}
\usage{
\method{print}{selection}(x = model, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{selection} object.}
  \item{\dots}{
Other options.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function returns the best subset of size q and its information criterion value. In the case of \code{seconds=TRUE} this information is returned for each alternative model.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

\seealso{
\code{\link{selection}}
}
\examples{
library(FWDselect)
data(emissionS02)

obj1=selection(x=emissionS02[,-19],y=emissionS02[,19],q=1,method="lm",criterion="R2")
obj1

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
