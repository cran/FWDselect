\name{test}
\alias{test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bootstrap based test for covariate selection}
\description{
Function that applies a bootstrap based test for covariate selection. It helps to determine the number of variables to be included into the model.
}
\usage{
test(x, y, method = "lm", family = "gaussian", nboot = 50, max.r=5, fix.r = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A data frame containing all the covariates.  
}
  \item{y}{
A vector with the response values.
}
 \item{method}{A character string specifying which regression method is used: linear models ("\code{lm}"), generalized additive models ("\code{glm}") or generalized additive models ("\code{gam}").
}
  \item{family}{A description of the error distribution and link function to be used in the model: "\code{gaussian}", "\code{binomial}" or "\code{poisson}".}
  \item{nboot}{Number of bootstrap repeats.
}
 \item{max.r}{Value until the search of the subset size \eqn{r} is performed.}  
\item{fix.r}{
A logical value. If  \code{TRUE} (default),  for each bootstrap repeat, the number of variables of the greatest model of the hypothesis (\eqn{r}) is fixed with the number of variables obtained from the sample.
}
 }
\details{

In a regression framework, let \eqn{X_1, X_2, \ldots, X_p},  a set of \eqn{p} initial variables and \eqn{Y} the response variable, a generalized additive regression model can be expressed as

 \deqn{Y=m(\textbf{X})+ \varepsilon}



where \eqn{m(\textbf{X})=m_1(X_1)+\ldots+m_p(X_p)} being \eqn{m_j  (j=1, \ldots, p)} smooth functions and \eqn{\varepsilon} the mean zero error. 


It is important to highlight that, in situations where we have a large number of variables, the correct estimation of the response will be obtained based on the selection of the appropriate  predictors. To this end, we  propose an automatic forward stepwise method however, before explaining it, it is necessary to establish some notation. Let \eqn{X_{j1}, \ldots, X_{jk}}, a subset of variables of size \eqn{k}, we define \eqn{\sigma^2_{j_{1} ,\ldots ,j_{k} }} as the error variance of the nonparametric model


\deqn{Y=m_{j_1}(X_{j_1})+m_{j_2}(X_{j_2})+\ldots+m_{j_k}(X_{j_k})+\varepsilon}


where \eqn{m_{jk}} are smooth and unknown functions  and \eqn{\varepsilon} the mean zero error.



 
In the variable selection procedure, an important step is to choose the number of covariates that have to be included in the model. For this reason, we propose the following strategy. For a subset of size \eqn{q}, considerations will be given to a test for the null hypothesis





\deqn{H_{0} \left(q\right): \sigma^2\left(q\right) = \mathop{\min }\limits_{r>q}  \sigma^2\left(r\right)}




vs. the general hypothesis 


\deqn{H_{1} : \sigma^2\left(q\right)>\mathop{\min }\limits_{r>q}  \sigma^2\left(r\right)}


where \eqn{\sigma^2(k)} is the error variance obtained with the best subset of \eqn{k} variables, namely




 \deqn{\sigma^2\left(k\right)=\mathop{\min }\limits_{1\le j_{1} <j_{2} <\ldots <j_{k} \le p}  \sigma^2_{j_{1} ,\ldots ,j_{k} } }








Note that if the null hypothesis is rejected, this means that the error variance of a model with one more variable will be lower. Therefore we need to include \eqn{q+1} variables in to the model. On the other hand, if the null hypothesis is not rejected, the residuals in both models will be the same so the inclusion of one new variable will not improve the prediction capacity of the model.


To test \eqn{H_0} we propose the use of the test statistic 


 \deqn{\widehat T=\frac{\hat{ \sigma}^2 \left(q\right)}{\mathop{\min }\limits_{r> q} \hat{ \sigma}^2 \left(r\right)}}

with 

 \deqn{\hat{ \sigma}^2 \left(k\right) =\mathop{\min } \limits_{1\le j_{1} <j_{2} <\ldots <j_{k} \le p} \hat{ \sigma}^2_{j_{1} ,\ldots ,j_{k} } }



being \eqn{\hat{ \sigma}^2_{j_{1} ,\ldots ,j_{k} }} the sample variance obtained by cross-validation according to the following expression


 \deqn{\hat{ \sigma}^2_{j_{1} ,\ldots ,j_{k} }=n^{-1} \sum_{i=1}^{n} \left(Y_i-\hat{Y}_i^{(-i)}\right)^{2}}


where  \eqn{\hat{Y}_i^{(-i)} } indicates the estimate of \eqn{Y_i} leaving out the \eqn{i}-th element of the sample obtained by fitting the previous model based on a sample \eqn{\{\textbf{X}_{i},Y_{i}\} _{i=1}^{n}}.







It is important to emphasize that, if the null hypothesis is verified, \eqn{\widehat{T}} should be close to one. In order to detect whether the true \eqn{T} is significantly greater than
one, we built an interval \eqn{[a,\infty)} where the hypothetical \eqn{T} was located with a specific 
probability. Hence, the test for checking \eqn{H_0} was that the null hypothesis was rejected if \eqn{a >1}.



Applying this test to \eqn{q=1,\ldots,p-1} may be an important step in a covariate selection procedure. If \eqn{H_0} is not rejected, only the subset of the covariables \eqn{X_{j_1},\ldots,X_{j_q}} will be retained, and the remainder variables will be eliminated from the model. In all other cases, the test is repeated with \eqn{q+1} variables until the null hypothesis is not rejected.





}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}

%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Marta Sestelo, Nora M. Villanueva and Javier Roca-Pardinas.
}
\note{
The detailed expression of the formulas are described in HTML help.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{selection}}
}
\examples{
library(FWDselect)
data(emissionS02)

test(x=emissionS02[,-19],y=emissionS02[,19],method="lm",max.r=3,nboot=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{bootstrap}% __ONLY ONE__ keyword per line
