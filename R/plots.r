plot.mmba = function (m, which=NA, index=FALSE, ...)
{	if (is.na (which) )
	{	p0 = .cmba.par (m$nf)
		for (f in m$fs) plot (f, m$y, index, ...)
		par (p0)
	}
	else plot (m$fs [[which]], m$y, index, ...)
}

plot.amba = function (m, which=NA, index=FALSE, ...)
{	if (is.na (which) )
	{	p0 = .cmba.par (m$nf)
		for (i in 1:m$nf) plot (m$fs [[i]], residuals (m, i), index, ...)
		par (p0)
	}
	else plot (m$fs [[which]], residuals (m, which), index, ...)
}

pairs.cmba = function (m, ...) pairs (m$fs, ...)

pairs.contributionset = function (fs, ...)
{	k = character ()
	m = NULL
	for (t in fs)
	{	k = c (k, t$name)
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

plot.contribution = function (f, y, index=FALSE, main, xlab, ylab, ...)
{	if (missing (main) ) main = format (f)
	if (missing (xlab) ) xlab = f$name
	if (missing (ylab) )
		ylab = if (missing (y) ) "" else deparse (substitute (y) )
	u = seq (f); v = f (u)
	xrng = range (f$x, na.rm=TRUE)
	yrng = range (v)
	if (!missing (y) ) yrng = range (c (yrng, y), na.rm=TRUE)
	plot.new ()
	plot.window (xlim=xrng, ylim=yrng)
	box (); axis (1); axis (2)
	title (main=main, xlab=xlab, ylab=ylab)
	if (!missing (y) )
	{	if (index) text (f$x, y, 1:f$nr, ...)
		else points (f$x, y, ...)
	}
	lines (u, v, ...)
}

plot.categorical = function (f, y, index=FALSE, main, xlab, ylab, ...)
{	if (missing (main) ) main = format (f)
	if (missing (xlab) ) xlab = f$name
	if (missing (ylab) )
		ylab = if (missing (y) ) "" else deparse (substitute (y) )
	u = 1:f$np; v = f (f$labels [u])
	xrng = c (0.5, f$np + 0.5)
	yrng = range (v)
	if (!missing (y) ) yrng = range (c (yrng, y), na.rm=TRUE)
	plot.new ()
	plot.window (xlim=xrng, ylim=yrng)
	box (); axis (1, u, f$labels); axis (2)
	title (main=main, xlab=xlab, ylab=ylab)
	if (!missing (y) )
	{	z = as.integer (f$x) + runif (f$nr, -0.275, 0.275)
		if (index) text (z, y, 1:f$nr, ...)
		else points (z, y, ...)
	}
	lines (u, v, ...)
	points (u, v, pch=16, cex=1.5)
}

lines.contribution = function (f, ...)
{	u = seq (f)
	lines (u, f (u), ...)
}

.cmba.par = function (n)
{	dims = matrix (c (1,1,1,  2,1,2,  3,1,3,  4,2,2,  6,2,3,  8,2,4, 9,3,3,  12,3,4,
		15,3,5,  18,3,6,  20,4,5,  24,4,6,  30,5,6, 35,5,7, 40,5,8), ncol=3, byrow=TRUE)
	dims = rbind (dims [dims [,1] >= n,])
	if (nrow (dims) > 0) dims = dims [1, 2:3]
	else stop ("plot limited to 40 terms")
	par (mfrow=dims, oma=c (0, 0, 0, 0), mar=c (2.5, 2.5, 3.0, 1.0) )
}





