\name{print.qselection}
\alias{print.qselection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Short \code{qselection} summary
}
\description{
\code{\link{qselection}} summary}
\usage{
\method{print}{qselection}(x=object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{qselection} object.
}
  \item{\dots}{
Other options.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function returns a summary table with the subsets of size \eqn{q}, their information criterion values and the chosen variables for each one. }
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Marta Sestelo, Nora M. Villanueva and Javier Roca-Pardinas.}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{qselection}}
}
\examples{
library(FWDselect)
data(emissionS02)

obj2=qselection(x=emissionS02[,-19],y=emissionS02[,19],qvector=c(1:6),method="lm",
criterion="deviance")
obj2}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
