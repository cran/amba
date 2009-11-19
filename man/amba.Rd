\name{amba}
\alias{amba}
\alias{fit.amba}
\alias{print.amba}
\alias{summary.amba}
\alias{print.summary.amba}
\alias{fitted.amba}
\alias{residuals.amba}
\alias{ncon}
\alias{ndiv}
\alias{ambacon}
\alias{ambadiv}
\title{additive models for business applications}
\description{Create and fit amba models, and supporting functions. This is a complex issue, there is, or will be, extensive documentation on this topic.}
\usage{
amba (y, ts, fit=TRUE, \dots)
\method{fit}{amba}(m, nfits=8, \dots)
\method{print}{amba}(m, \dots)
\method{summary}{amba}(m, which=NA, \dots)
\method{print}{summary.amba}(s, \dots)
\method{fitted}{amba}(m, \dots)
\method{residuals}{amba}(m, partial=NULL, \dots)
ncon (m)
ndiv (m)
ambacon (m)
ambadiv (m)
}
\arguments{
\item{y}{Univariate realisations of a response variable.}
\item{ts}{Either a term object or a termlist object.}
\item{fit}{Whether or not to fit the amba model.}
\item{nfits}{}
\item{m}{An amba model}
\item{which}{If NA, produce overall summary output plus summary output for all terms. If a value is specified, then only that term is given.}
\item{s}{A summary.amba object.}
\item{partial}{If specified, the residuals are incremented by their corresponding fitted values.}
\item{\dots}{}
}

