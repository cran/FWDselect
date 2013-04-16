\name{test}
\alias{test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bootstrap based test for covariate selection
}
\description{
Function that applies a bootstrap based test for covariate selection. It helps to determine the number of variables to be included in the model.
}
\usage{
test(x, y, method = "lm", family = "gaussian", nboot = 50, speedup=TRUE,
 unique=FALSE, num.h0=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A data frame containing all the covariates.  
}
  \item{y}{
A vector with the response values.
}
 \item{method}{
 	A character string specifying which regression method is used, i.e., linear models ("\code{lm}"), generalized additive models ("\code{glm}") or generalized additive models ("\code{gam}").
}
  \item{family}{This is a family object specifying the distribution and link to use in fitting: "\code{gaussian}", "\code{binomial}" or "\code{poisson}".
}
  \item{nboot}{
  	Number of bootstrap repeats.
}
 \item{speedup}{
 	A logical value. If  \code{TRUE} (default), the testing procedure is  accelerated by a minor change in the statistic.}  
\item{unique}{
	A logical value. If  \code{TRUE}, the test is performed only for one null hypothesis, given by the argument  \code{num.h0}.
}
 \item{num.h0}{ 
 	If  \code{unique} is \code{TRUE}, \code{num.h0} is the integer number \eqn{q}{} of \eqn{H_0(q)} to be tested.
}
}
\details{
In a regression framework, let \eqn{X_1, X_2, \ldots, X_p},  a set of \eqn{p} initial variables and \eqn{Y} the response variable, we propose a procedure to test the null hypothesis of \eqn{q} significant variables  in the model versus the alternative in which  the model contains more than \eqn{q} variables. Based on the general model 

\deqn{Y=m(\textbf{X})+\varepsilon  \quad {\rm{where}} \quad m(\textbf{X})= m_{1}(X_{1})+m_{2}(X_{2})+\ldots+m_{p}(X_{p})}

the following strategy is considered: for a subset of size \eqn{q}, considerations will be given to a test for the null hypothesis

 


\deqn{H_{0} (q): \sum_{j=1}^p I_{\{m_j \ne 0\}} \le q}


vs. the general hypothesis 


\deqn{H_{1} :  \sum_{j=1}^p I_{\{m_j \ne 0\}} > q}
}

\value{
 \item{Hypothesis}{
 	Number of the null hypothesis tested
}
 \item{Statistic}{
 	Value of the T statistic
}
 \item{pvalue}{
 	pvalue obtained in the testing procedure
}
 \item{Decision}{
 	Result of the test for a significance level of 0.05
}
}

\references{
Sestelo, M., Villanueva, N. M. and Roca-Pardinas, J. (2013). FWDselect: an R package for selecting variables in regression models. \emph{Discussion Papers in Statistics and Operation Research}, 13/02.
}
\author{
Marta Sestelo, Nora M. Villanueva and Javier Roca-Pardinas.
}
\note{
The detailed expression of the formulas are described in HTML help \url{http://cran.r-project.org/web/packages/FWDselect/FWDselect.pdf}. For more details about the testing procedure see \cite{Sestelo et al. (2013)}
}
\seealso{
\code{\link{selection}}
}

\examples{
library(FWDselect)
data(pollution)
x=pollution[,-19]
y=pollution[,19]
test(x,y,method="lm",nboot=5)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bootstrap}
\keyword{test}% __ONLY ONE__ keyword per line