\name{evaluate.term}
\alias{evaluate.linear}
\alias{evaluate.smooth}
\title{evaluate terms}
\description{Term estimates are based on the principle that they can be evaluated as functions of an explanatory variable, regardless of the type of term. To avoid confusion with the original explanatory values, denoted x, the new explanatory values are denoted u. Linear evaluation is trivial, smooth evaluation happens via interpolation.}
\usage{
\method{evaluate}{linear}(t, u, \dots)
\method{evaluate}{smooth}(t, u, \dots)
}
\arguments{
\item{t}{A term object.}
\item{u}{A vector (usually) of new explanatory values. For multivariate terms, namely linear interaction terms, u is matrix or data.frame.}
\item{\dots}{.}
}


