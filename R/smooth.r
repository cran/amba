smooth = function (x, y, ns=10, degree=2, smoothness, name)
{	if (missing (name) ) name = deparse (substitute (x) )
	f = extendf (contribution (x, name), "smooth", .smooth.evaluate)
	xr = range (x, na.rm=TRUE)
	t = seq (xr [1], xr [2], length=ns)
	if (missing (smoothness) ) smoothness = 0.67 * diff (xr)
	implant (f, ns, t, degree, wf=.stdwf, smoothness, e=rep (0, ns) )
	f$xt = .cleanv (f, x)
	f$xi = order (f$xt)
	f$xt = f$xt [f$xi]
	.smooth.neighbours (f)
	.smooth.matrix (f)
	.tryfitc (f, y)
}

is.smooth = function (f) inherits (f, "smooth")

fitraw.smooth = function (f, y, ...)
{	e = numeric (f$ns)
	e [f$pop] = .smooth.series (f$t [f$pop], f$degree, f$xt, f$z, y [f$xi],
		f$npop, f$ilwr, f$iupr, f$wf, f$smoothness)
	e [!f$pop] = mean (y)
	e
}

.smooth.evaluate = function (u)
{	k = is.finite (u)
	u = u [k]
	v = spline (t, e, xout=u)$y
	m = rep (NA, length (k) )
	m [k] = v
	m
}

summaryraw.smooth = function (f, ...) data.frame (sx=f$t, sy=f$e)

.smooth.neighbours = function (f)
{	pop = rep (TRUE, f$ns)
	ilwr = iupr = numeric (f$ns)
	hs = f$smoothness / 2
	x = f$xt
	for (i in 1:f$ns)
	{	u = f$t [i]
		k = which (x >= u - hs & x <= u + hs)
		ilwr [i] = min (k)
		iupr [i] = max (k)
		if (iupr [i] - ilwr [i] < f$degree)
			pop [i] = FALSE
	}
	f$npop = sum (pop)
	f$pop = pop
	f$ilwr = ilwr [pop]
	f$iupr = iupr [pop]
}

.smooth.matrix = function (f)
{	u = f$xt
	z = matrix (nr=f$nv, nc=f$degree + 1)
	for (i in 0:f$degree) z [,i + 1] = u^i
	f$z = z
}

.smooth.series = function (t, d, x, z, y, ns, ilwr, iupr, wf, smoothness)
{	e = numeric (ns)
	pow = 0:d
	for (i in 1:ns)
	{	u = t [i]
		k = ilwr [i]:iupr [i]
		xst = x [k] - u
		yst = y [k]
		zst = z [k,]
		w = wf (smoothness, xst)
		ps = lm.wfit (zst, yst, w)$coefficients
		e [i] = sum (ps * u^pow)
	}
	e
}

.stdwf = function (m, x)
{	y = 2 * x / m
	1 - y * y
}





