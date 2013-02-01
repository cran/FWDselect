\name{print.qselection}
\alias{print.qselection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Short \code{qselection} summary
}
\description{
\code{\link{qselection}} summary
}
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
The function returns a summary table with the subsets of size \eqn{q}, their information criterion values and the chosen variables for each one. 
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
	\code{\link{qselection}}
}
\examples{
library(FWDselect)
data(pollution)
x=pollution[,-19]
y=pollution[,19]
obj2=qselection(x,y,qvector=c(1:4),method="lm",
criterion="R2")
obj2
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line