\name{clean}
\alias{clean.explanatory}
\alias{clean.response}
\alias{clean.fitted}
\alias{clean.residuals}
\title{clean with respect to x}
\description{These functions do the same as their corresponding functions, except that the vectors they return are clean with respect to x. This means that any values are removed, where the explanatory realisations are invalid. In principle (although these functions do not enforce this), the values that are returned should also be clean.}
\usage{
clean.explanatory (t)
clean.response (t, y)
clean.fitted (t)
clean.residuals (t, y)
}
\arguments{
\item{t}{.}
\item{y}{.}
}


