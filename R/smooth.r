smooth = function (x, y=NULL, ns=20, deg=2, nfits=2, wf=stdwf, smoothness=NULL, s)
{	if (missing (s) ) s = deparse (substitute (x) )
	if (deg == 0) stop ("degree must be at least one")
	d = smooth.design (ns, nfits, deg, wf, smoothness, x)
	e = smooth.estimate (ns)
	t = term (d, e, s, x)
	term.pump (t, y, "smooth")
}

standardestimation.smooth = function (t, ...)
{	lps = LocalPolynomialSmoothing (t)
	setestimation (t, lps)
}

#heuristic
#will change...
eqnp.smooth = function (t, ...) 2 * t$d$deg

interceptadj.smooth = function (t, ...) t$e$th

smooth.design = function (ns, nfits, deg, wf, smoothness, x)
{	d = extend (term.design (), "smooth.design")
	xr = range (x, na.rm=TRUE)
	d$ns = ns
	d$sx = seq (xr [1], xr [2], length=ns)
	d$nfits=deg
	d$deg = deg
	d$wf = wf
	d$smoothness = if (is.null (smoothness) ) 0.85 * diff (xr) else smoothness
	d
}

smooth.estimate = function (ns, th=0, sy=rep (0, ns) )
{	e = extend (term.estimate (), "smooth.estimate")
	e$th = th
	e$sy = sy
	e
}

reset.smooth = function (t, ...) t$e$sy [] = 0
mutate.smooth = function (t, re, ...) t$e$sy = re
rawestimate.smooth = function (t, ...) t$e$sy

evaluate.smooth = function (t, u, ...)
{	k = is.finite (u)
	u = u [k]
	v = spline (t$d$sx, t$e$sy, xout=u)$y
	m = rep (NA, length (k) )
	m [k] = v
	m
}

summary.smooth = function (t, ...)
{	s = extend (summary.term (t, ...), "summary.smooth")
	s$ext = extend (list (ns=t$d$ns, deg=t$d$deg, sx=t$d$sx, sy=t$e$sy),
		"summaryext.smooth")
	s
}

print.summaryext.smooth = function (s, ...)
{	print (data.frame (ns=s$ns, deg=s$deg) )
	cat ("sx:\n")
	print (s$sx)
	cat ("sy:\n")
	print (s$sy)
}

print.smooth.estimate = function (e, ...) print (t$e$sy)


