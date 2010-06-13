fit.amba = function (m, nfits=12, ...)
{	start.time = Sys.time ()
	e = list ()
	for (rf in 1:nfits)
	{	for (j in 1:m$nf)
			e [[j]] = fit (m$fs [[j]], residuals (m, j), TRUE)
		for (j in 1:m$nf)
			m$fs [[j]]$e = e [[j]]
	}
	m$ct= as.numeric (Sys.time () - start.time)
	invisible (m)
}

residuals.amba = function (m, which=NA, ...)
{	r = m$y
	for (i in 1:m$nf)
	{	if (is.na (which) || i != which)
		{	f = m$fs [[i]]
			yh = fitted (f)
			if (f$clean) r = r - yh
			else r [f$valid] = r [f$valid] - yh [f$valid]
		}
	}
	r
}

.exclude = function (v, which)
{	if (is.na (which) ) v
	else v [-which]
}

