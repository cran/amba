\name{interceptadj}
\alias{interceptadj}
\alias{interceptadj.amba}
\alias{interceptadj.linear}
\alias{interceptadj.smooth}
\alias{interceptadj.interaction}
\alias{interceptadj.default}
\title{intercept adjustments}
\description{These functions are primarily intended to support producing clean summary output. Unlike contemporary additive models, each term usually contains it's own intercept (and it is left in there). The term methods here, extract that value, and the amba method collapses them. Note that they only extract the value, they don't (despite their name) actually adjust it. Summary output creates an illusion that there is only one intercept.}
\usage{
interceptadj (\dots)
\method{interceptadj}{amba}(m, \dots)
\method{interceptadj}{linear}(t, \dots)
\method{interceptadj}{smooth}(t, \dots)
\method{interceptadj}{interaction}(t, \dots)
\method{interceptadj}{default}(\dots)
}
\arguments{
\item{m}{}
\item{t}{}
\item{\dots}{}
}





