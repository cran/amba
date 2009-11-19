amba = function (y, ts, fit=TRUE, ...)
{	ts = as.termlist (ts)
	for (t in ts) estimation.validate (t)
	m = extend (compenv (th=0, ts, nt=length (ts), y, nr=length (y),
		converge=NA, nfits=NA, nsecs=NA), c ("amba", "termenv") )
	if (!all (is.finite (y) ) ) stop ("all y must be valid")
	v = validex (ts [[1]])
	for (i in iter (m$nt, 2) ) v = (v & validex (ts [[i]]) )
	m$complete=v
	if (fit) fit (m, ...) else m
}

#fit.amba = function (m, maxfits=30, ...)
fit.amba = function (m, nfits=8, ...)
{	time0 = Sys.time ()
	for (t in m$ts) reset (t)
	m$converge = FALSE
	m$nfits = 0
	mara = marb = mar0 = NA
	#diverge = FALSE
	m$th = m$th + mean (residuals (m) )
	#while (!m$converge && !diverge && m$nfits < maxfits)
	maxfits = nfits
	while (m$nfits < maxfits)
	{	kc = ambacon (m)
		
		#main effects
		es = list ()		
		for (i in kc)
			if (m$ts [[i]]$nc == 1)
				es [[i]] = fit (m$ts [[i]], residuals (m, partial=i), FALSE)
		for (i in kc) if (m$ts [[i]]$nc == 1) mutate (m$ts [[i]], es [[i]])

		#interactions (2-way) - experimental
		es = list ()
		for (i in kc)
			if (m$ts [[i]]$nc == 2)
				es [[i]] = fit (m$ts [[i]], residuals (m, partial=i), FALSE)
		for (i in kc) if (m$ts [[i]]$nc == 2) mutate (m$ts [[i]], es [[i]])

		r = residuals (m)
		m$th = m$th + mean (r)

		#convergence system doesn't work properly
		mara = marb
		marb = mean (abs (r) )
		m$nfits = m$nfits + 1

		if (m$nfits == 1) mar0 = marb
		if (m$nfits > 2)
		{	change.wrt.0 = marb / mar0
			change.wrt.a = marb / mara
			if (change.wrt.0 <= 0.0001) m$converge = TRUE
			#else if (change.wrt.a > 1) diverge = TRUE
			else if (change.wrt.a > 1) NULL
			else if (change.wrt.a > 0.9999) m$converge = TRUE
		}
	}
	#if (diverge) warning ("amba diverged")
	#else if (!m$converge) warning ("amba didn't converge within maximum iterations")

	#might remove
	if (ndiv (m) > 0)
	{	kd = ambadiv (m)
		r = residuals (m)
		pcd = rep (0, m$nt)
		for (i in kd)
		{	fit (m$ts [[i]], r)
			pcd [i] = term.pcd (m$ts [[i]], r)
		}
		total.pcd = sum (pcd)
		for (i in kd)
		{	m$ts [[i]]$w = pcd [i] / total.pcd
			fit (m$ts [[i]], m$ts [[i]]$w * r)
		}
	}
	
	m$nsecs= as.numeric (Sys.time () - time0)
	m
}

evaluate.amba = function (m, us, ...)
{	eh = m$th
	for (i in 1:m$nt) eh = eh + evaluate (m$ts [[i]], us [[i]])
	eh
}

eqnp.amba = function (m, convergent.only=TRUE, ...) 
{	np = 1
	for (t in m$ts) if (!convergent.only || t$convergent) np = np + eqnp (t)
	np
}

interceptadj.amba = function (m, ...)
{	th = 0
	for (t in m$ts) th = th + interceptadj (t)
	th
}

print.amba = function (m, ...) print (m$ts)

summary.amba = function (m, which=NA, ...)
{	np = eqnp (m)
	if (is.na (which) )
	{	s = structure (list (), class="summary.amba")
		s$th = m$th + interceptadj (m)
		s$ts = list ()
		r = residuals (m)
		for (i in itobj (m$ts) )
			s$ts [[i]] = summary (m$ts [[i]], residuals (m, partial=i), ..., np=np)
		s$gf = data.frame (gf (m, ...) )
		s$converge = m$converge
		s$nfits = m$nfits
		s$nsecs = m$nsecs
		s$nr = m$nr
		s$ncomplete = sum (m$complete)
		s$nt = m$nt
		s$ncon = ncon (m)
		s$eqnp = eqnp (m)
		s
	}
	else summary (m$ts [[which]], residuals (m, partial=which), ..., np=np)
}

print.summary.amba = function (s, ...)
{	cat ("intercept:", s$th, "\n\n")
	for (t in s$ts)
	{	print (t)
		cat ("\n")
	}
	names (s$gf) = paste ("overall", names (s$gf), sep=".")
	nfits = paste (s$nfits, "(", round (s$nsecs, 2), ")", sep="")
	nt = paste (s$nt, "(", s$ncon, ")", sep="")
	nr = paste (s$nr, "(", s$ncomplete, ")", sep="") 
	s$gf = data.frame (converged=s$converge, nfits, nt, nr, eqnp=s$eqnp, s$gf)
	names (s$gf) [2:4] = c ("nfits(nsecs)", "nt(ncon)", "nr(ncomplete)")
	print (s$gf)	
}

fitted.amba = function (m, ...) evaluate (m, ambax (m) )

residuals.amba = function (m, partial=NULL, ...)
{	r = m$y - m$th
	tst = if (ifst (partial) ) m$ts [[partial]] else NULL
	for (i in ambacon (m) )
	{	t = m$ts [[i]]
		if (is.null (tst) || !term.resists (tst, t) )
		{	if (t$clean) r = r - fitted (t)
			else r [t$valid] = r [t$valid] - fitted (t) [t$valid]
		}
	}
	if (ifst (tst) )
	{	if (tst$convergent) r = r + fitted (tst)
		else r = tst$w * r
	}
	r
}

gf.amba = function (m, method="mar", ...) simple.gf (NA, m$y, residuals (m), method)

ambax = function (m)
{	x = list ()
	for (i in 1:m$nt) x [[i]] = m$ts [[i]]$x
	x
}

ncon = function (m) length (ambacon (m) )
ambacon = function (m)
{	i = numeric ()
	for (j in 1:m$nt) if (m$ts [[j]]$convergent) i = c (i, j)
	i
}

ndiv = function (m) length (ambadiv (m) )
ambadiv = function (m)
{	i = numeric ()
	for (j in 1:m$nt) if (!m$ts [[j]]$convergent) i = c (i, j)
	i
}

