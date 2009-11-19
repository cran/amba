\name{linear}
\alias{linear}
\alias{fitted.linear}
\alias{is.linear}
\alias{is.categorical}
\alias{is.polynomial}
\title{linear terms}
\description{Create linear terms. These are relatively complex, and in general it is better to use a categorical or polynomial term instead. For more information on linear terms, refer to linear.design, linear.ESTIMATOR, linear.estimate and linear.matrix. Note these differ substantially from convention. A linear term usually contains a single variable, however may contain multiple parameter estimates. The current implementation does not store the linear matrix, and uses what are described as key functions to map real-world variables to model-variables (i.e. the linear matrix) used for estimation. The linear subclasses take care of key function specification.
}
\usage{
linear (x, y=NULL, key=c ("1", "x"), valid.intercept=ifst (y), s)
\method{fitted}{linear}(t, \dots)
is.linear (t)
is.categorical (t)
is.polynomial (t)
}
\arguments{
\item{x}{}
\item{y}{}
\item{t}{}
\item{key}{}
\item{valid.intercept}{}
\item{s}{}
\item{\dots}{}
}




