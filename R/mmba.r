mmba = function (y, ts, fit=TRUE, ...)
{	ts = as.termlist (ts)
	for (t in ts) estimation.validate (t)
	m = extend (compenv (nt=length (ts), nr=length (y), ts, y), c ("mmba", "termenv") )
	if (!all (is.finite (y) ) ) stop ("all y must be valid")
	if (fit) fit (m, ...) else m
}

fit.mmba = function (m, ...)
{	for (t in m$ts) fit (t, m$y)
	m
}

plot.mmba = function (m, ..., index=FALSE)
{	p0 = par (mfrow=.amba.dims (m$nt), oma=c (0, 0, 0, 0), mar=c (2.5, 2.5, 3.0, 1.0) )
	for (i in itobj (m$ts) ) plot (m$ts [[i]], m$y, index=index)
	par (p0)
}

pairs.termlist = function (x, ...)
{	k = character ()
	m = NULL
	for (t in x)
		if (t$nc == 1)
		{	k = c (k, t$s)
			y = t$x
			if (is.character (y) ) y = factor (y)
			if (is.factor (y) )
			{	nlevs = length (levels (y) )
				y = as.numeric (y)
				u = 0.5 * diff (range (y, na.rm=TRUE) ) / (nlevs - 1)
				y = y + runif (length (y), -u, u)
			}
			m = cbind (m, y)
		}
	pairs (m, k, diag.panel=NULL, lower.panel=NULL)
}



