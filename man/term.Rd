\name{term}
\alias{term}
\alias{is.term}
\alias{print.term}
\alias{fitted.term}
\alias{residuals.term}
\title{term objects}
\description{Creates term objects. The term class that defines term objects is intended as abstract. Until the package becomes more stable (e.g. version 1.0.0), and the process of defining term classes is documented, it would be unwise for users to try and use this directly.}
\usage{
term (d, e, s, x, E=NULL, I=NULL, validate=TRUE, clean=TRUE,
	nr=length (x), nc=1, nv, valid=NULL)
is.term (t)
\method{fitted}{term}(t, \dots)
\method{residuals}{term}(t, y, \dots)
}
\arguments{
\item{d}{A term design.}
\item{E}{An estimator.}
\item{I}{An inferencer.}
\item{e}{A term estimate.}
\item{s}{A symbol (e.g. a character) for x.}
\item{x}{Realisations of an explanatory variable.}
\item{validate}{Validates the term, refer to term.validate.}
\item{clean}{Whether or not the term is clean, that is, all x values are finite.}
\item{nr}{The number of realisations, usually the length of x. If x is a matrix or data.frame, then this is the number of rows.}
\item{nc}{The number of component (or nested) variables, usually 1. If x is a matrix or data.frame, then this is the number of columns.}
\item{nv}{The number of valid realisations. If clean, then nr = nv.}
\item{valid}{If unclean, then a logical vector, corresponding to valid realisations.}
\item{t}{A term object.}
\item{y}{Realisations of a response variable, or partial residuals.}
\item{\dots}{}
}



