#note, no allowance for sparse data
#possible error?
smooth = function (x, y, ns=10, degree=2, smoothness=0.67, bw, name)
{	if (missing (name) ) name = deparse (substitute (x) )
	f = .extendf (contribution (x, name), "smooth", FUNCTION (.smooth.evaluate) )
	xr = range (x, na.rm=TRUE)
	t = seq (xr [1], xr [2], length=ns)
	if (missing (bw) ) bw = smoothness * diff (xr)
	else smoothness = NA
	f = implant (f, ns, t, degree, wf=.stdwf, smoothness, bw, e=rep (0, ns) )
	f$xv = .cleanv (f, x)
	f$z = .smooth.matrix (f)
	.tryfitc (f, y)
}

is.smooth = function (f) inherits (f, "smooth")

fitraw.smooth = function (f, y, ...)
{	e = numeric (f$ns)
	e = .smooth.series (f$ns, f$t, f$degree, f$xv, f$z, y, f$wf, f$bw)
	e
}

#todo: rewrite (or replace with ofp function)
.smooth.evaluate = function (u)
{	k = is.finite (u)
	u = u [k]
	v = spline (.$t, .$e, xout=u)$y
	m = rep (NA, length (k) )
	m [k] = v
	m
}

summaryraw.smooth = function (f, ...) data.frame (sx=f$t, sy=f$e)

.smooth.matrix = function (f)
{	z = matrix (nr=f$nv, nc=f$degree + 1)
	for (i in 0:f$degree) z [,i + 1] = f$xv^i
	z
}

#note, no subsetting
.smooth.series = function (ns, t, d, x, z, y, wf, bw)
{	e = numeric (ns)
	pow = 0:d
	for (i in 1:ns)
	{	w = wf (bw, x - t [i])
		w [w < 0] = 0
		k = lm.wfit (z, y, w)$coefficients
		e [i] = sum (k * t [i]^pow)
	}
	e
}

#todo: replace
.stdwf = function (m, x)
{	y = 2 * x / m
	1 - y * y
}


