\name{FWDselect-package}
\alias{FWDselect-package}
\alias{FWDselect}
\docType{package}
\title{
\code{FWDselect}: Selecting variables in regression models
}
\description{
\code{FWDselect} package introduces a simple method to select the best model using different types of data (binary, gaussian or poisson) and applying it in different contexts (parametric or nonparametric). The proposed method is a new forward stepwise-based selection procedure that  selects a model containing a subset of variables according to an optimal criterion (obtained by cross-validation) and also takes into account the computational cost. Additionally,  bootstrap resampling techniques are used to implement tests capable of detecting whether significant effects of the unselected variables are present in the model. 
}
\details{
\tabular{ll}{
Package: \tab FWDselect\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-06-29\cr
License: \tab GPL\cr}



\code{FWDselect} is just a shortcut for ``Forward selection'' and this is a very good summary of a major functionality of the package: to provide a forward stepwise-based selection procedure. This software helps to select relevant variables and to evaluate the number of them to be included in a regression model. In addition,  it is possible to display both  numerical and graphical outputs.  

In the package are included several functions that enable users to select the variables to include  either in linear, generalized linear or generalized additive regression models. Users can obtain the best combinations of \code{q} variables by means of the main function \code{\link{selection}}. Additionally, if one wants to obtain the results for more than one size of subset, it is possible to apply the \code{\link{qselection}} function, which returns a summary table with the different subsets, selected variables and information criterion values. The object obtained when using this last function is the argument required for \code{\link{plot.qselection}} which provides a graphical output. Finally, to determine the number of variables that have to be introduced into the model, only applying \code{\link{test}} function can be obtained it.
}


\author{
Marta Sestelo, Nora M. Villanueva and Javier Roca-Pardinas.

Maintainer: Marta Sestelo <sestelo@uvigo.es>
}
\references{
Burnham, K., Anderson, D. (2002). Model selection and multimodel inference: a practical information-theoretic approach. 2nd Edition Springer.

Efron, B. (1979). Bootstrap methods: another look at the jackknife. Annals of Statistics, 7:1-26. 

Efron, B. and Tibshirani, R. J. (1993). An introduction to the Bootstrap. Chapman and Hall, London.


Miller, A. (2002). Subset selection in regression. Champman and Hall.
}