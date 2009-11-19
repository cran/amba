plot.amba = function (m, which=NA, ..., index=FALSE)
{	if (is.na (which) )
	{	p0 = par (mfrow=.amba.dims (m$nt), oma=c (0, 0, 0, 0), mar=c (2.5, 2.5, 3.0, 1.0) )
		for (i in itobj (m$ts) ) plot (m$ts [[i]], residuals (m, partial=i), index=index)
		par (p0)
	}
	else
	{	t = m$ts [[which]]
		plot (t, residuals (m, partial=which), "fh", index=index)
	}
}

plot.term = function (t, y=NULL, s, newplot=TRUE,
	main=term.label (t), lwd=2, col=rgb (0, 0.6, 0.1), ..., index=FALSE)
{	if (missing (s) )
	{	s = if (is.null (y) ) paste ("fh(", t$s, ")", sep="")
		else deparse (substitute (y) )
	}
	if (t$nc > 1) stop ("plot.term not applicable where nc > 1")
	fx = seq (t)
	fy = evaluate (t, fx)
	k = validex (t)
	x = t$x [k]
	y = y [k]
	if (newplot)
	{	rx = range (c (fx, x), na.rm=TRUE)
		ry = range (c (fy, y), na.rm=TRUE)
		plot.new ()
		plot.window (xlim=rx, ylim=ry)
		box ();
		axis (1);
		axis (2);
		title (main=main, xlab=t$s, ylab=s)
	}
	if (!is.null (y) )
	{	if (index)
		{	text (x, y, (1:t$nr) [k])
		}
		else points (x, y)
	}
	lines (fx, fy, lwd=lwd, col=col)
}

lines.term = function (t, ...) plot (t, ..., newplot=FALSE)

plot.categorical = function (t, y=NULL, s,
	main=term.label (t), lwd=2, col=rgb (0, 0.6, 0.1), ..., index=FALSE)
{	if (missing (s) )
	{	s = if (is.null (y) ) paste ("fh(", t$s, ")", sep="")
		else deparse (substitute (y) )
	}
	u = 1:t$d$np
	fy = t$e$th
	plot.new ()
	plot.window (xlim=c (0.5, t$d$np + 0.5), ylim=range (c (fy, y), na.rm=TRUE))
	box ();
	axis (1, 1:t$d$np, t$e$labs);
	axis (2);
	title (main=main, xlab=t$s, ylab=s)
	if (!is.null (y) )
	{	xst = as.integer (t$x) + runif (t$nr, -0.275, 0.275)
		if (index)
		{	k = validex (t)
			text (xst [k], y [k], (1:t$nr) [k])
		}
		else points (xst, y)
	}
	lines (u, fy, lwd=lwd, col=col)
	points (u, fy, pch=16, cex=1.5, col="white")
	points (u, fy, cex=1.5, lwd=lwd, col=col)
	#points (u, fy, pch=4, cex=1.5, col=col)
}

lines.categorical = function (t, ...)
	stop ("lines not applicable for categorical term")

plot.interaction = function (t, ...)
{	plot.new ()
	plot.window (xlim=0:1, ylim=0:1)
	box (lty=3, col="grey60")
	title (main=term.label (t) )
	text (0.5, 0.5, "todo", col="grey60")
}

.amba.dims = function (n)
{	dims = matrix (c (1,1,1,  2,1,2,  3,1,3,  4,2,2,  6,2,3,  8,2,4, 9,3,3,  12,3,4,
		15,3,5,  18,3,6,  20,4,5,  24,4,6,  30,5,6, 35,5,7, 40,5,8), ncol=3, byrow=TRUE)
	dims = rbind (dims [dims [,1] >= n,])
	if (nrow (dims) > 0) dims = dims [1, 2:3]
	else stop ("plot limited to 40 terms")
	dims
}


