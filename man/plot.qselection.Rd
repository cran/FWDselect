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
data(polution)
x=pollution[,-19]
y=pollution[,19]
obj2=qselection(x,y,qvector=c(1:4),method="lm",
criterion="R2")
plot(obj2)
}