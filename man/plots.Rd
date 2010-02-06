\name{plots}
\alias{plot.amba}
\alias{plot.term}
\alias{plot.categorical}
\alias{plot.interaction}
\alias{lines.term}
\alias{lines.categorical}
\title{plot functions for the amba package}
\description{Mostly self-explanatory. The which argument in the plot method for amba, if specified, produces a plot for a single term only, and is sometimes simpler than calling the plot method for the term directly, which may require y values to be specified. Note that plot.interaction doesn't do much (yet), and that currently lines.categorical will cause an error.}
\usage{
\method{plot}{amba}(m, which=NA, \dots, index=FALSE)
\method{plot}{term}(t, y=NULL, s, newplot=TRUE, main=term.label (t),
	lwd=2, col=rgb (0, 0.6, 0.1), \dots, index=FALSE)
\method{plot}{categorical}(t, y=NULL, s, main=term.label (t),
	lwd=2, col=rgb (0, 0.6, 0.1), \dots, index=FALSE)
\method{plot}{interaction}(t, \dots)
\method{lines}{term}(t, \dots)
\method{lines}{categorical}(t, \dots)
}
\arguments{
\item{m}{An amba model.}
\item{which}{Which term to plot. If NA, the default, plot all terms.}
\item{t}{A term object.}
\item{y}{Realisations of the response, or partial residuals.}
\item{s}{A symbol for y. Usually it is not necessary to specify this.}
\item{newplot}{Users should not use this, it is used by the lines methods.}
\item{index}{Plot the index of each data point, instead of a circle.}
\item{main}{.}
\item{lwd}{.}
\item{col}{.}
\item{\dots}{.}
}




