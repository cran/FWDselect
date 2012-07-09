\name{plot.qselection}
\alias{plot.qselection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualization of \code{qselection} object
}
\description{
This function plots the cross-validation information criterion for several subsets of size \eqn{q} chosen by the user.
}
\usage{
\method{plot}{qselection}(x=object,y=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{qselection} object.}
 \item{y}{ NULL
}
  \item{\dots}{
Other options.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Simply returns a plot.
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
data(emissionS02)

obj2=qselection(x=emissionS02[,-19],y=emissionS02[,19],qvector=c(1:7),method="lm",
criterion="R2")
plot(obj2)
}