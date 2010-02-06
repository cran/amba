\name{fit.term}
\alias{fit.term}
\title{fit a term}
\description{This function (which is the same for all terms) delegates the process of fitting to a term estimator object (which is not necessarily the same). As term objects do not store a y value, the calling function must provide this, which is cleaned with respect to x, before being provided to the estimator. It can either be realisations of the response, or partial residuals. If the pump argument is true, the default, then the term estimates are updated, with what is returned by the estimator, referred to as the raw estimate. If pump is false, then the term estimates are left unchanged and the raw estimate is returned.}
\usage{
\method{fit}{term}(t, y, pump=TRUE, \dots)
}
\arguments{
\item{t}{.}
\item{y}{.}
\item{pump}{.}
\item{\dots}{.}
}


