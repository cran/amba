\name{smooth}
\alias{smooth}
\title{smooth terms}
\description{Create a smooth term object. For the purposes of this package, statistical smoothing is defined as approximating an unknown continuous function with a series of estimates, usually (although not always) by local estimation. The smooth objects here, contain two series, one is the x values where the estimates are computed, denoted sx, and is part of the smooth design object, two is the actual estimates themselves, denoted, sy, and is part of the smooth estimate object. Refer to smooth.design, smooth.ESTIMATOR, or smooth.estimate, for more information.}
\usage{
smooth (x, y=NULL, ns=20, deg=2, nfits=2, wf=stdwf, smoothness=NULL, s)
}
\arguments{
\item{x}{.}
\item{y}{.}
\item{ns}{The number of smooth estimates. If this is large (which usually adds little value), it may slow estimation considerably.}
\item{deg}{Assuming a local polynomial estimator, the degree of the local polynomials.}
\item{nfits}{If greater than 1, then the smoother refits itself, replacing x and y, with sx and sy.}
\item{wf}{A weight function. Currently only one available.}
\item{smoothness}{The smoothness parameter, more often referred to as bandwidth.}
\item{s}{.}
}



