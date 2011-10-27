fit.amba = function (m, ..., nfits=12, preferential=TRUE)
{	start.time = Sys.time ()
	if (preferential) .amba.preferential (m, ...)
	else .amba.backfit (m, ...)
	m$ct= as.numeric (Sys.time () - start.time)
	invisible (m)
}

#first iteration, inefficient, as w=0
.amba.preferential = function (m, nfits=20, ...)
{	for (w in seq (0, 1, length=nfits) )
		for (j in 1:m$nf)
			m$fs [[j]] = fit (m$fs [[j]], .pref.res (m, w, j), FALSE)
}

.amba.backfit = function (m, nfits=8, ...)
{	e = list ()
	for (rf in 1:nfits)
	{	for (j in 1:m$nf)
			e [[j]] = fit (m$fs [[j]], residuals (m, j), TRUE)
		for (j in 1:m$nf)
			m$fs [[j]]$e = e [[j]]
	}
}

residuals.amba = function (m, which=NA, ...)
{	r = m$y
	for (j in 1:m$nf)
	{	if (is.na (which) || j != which)
		{	f = m$fs [[j]]
			yh = fitted (f)
			if (f$clean) r = r - yh
			else r [f$valid] = r [f$valid] - yh [f$valid]
		}
	}
	r
}

.pref.res = function (m, w, which)
{	r = m$y
	for (j in 1:m$nf)
	{	if (j != which)
		{	f = m$fs [[j]]
			yh = fitted (f)
			if (j > which) yh = w * yh
			if (f$clean) r = r - yh
			else r [f$valid] = r [f$valid] - yh [f$valid]
		}
	}
	r
}



