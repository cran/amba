cmba = function (y, fs)
{	fs = contributionset (fs)
	if (!all (is.finite (y) ) ) stop ("all y must be valid")
	extend (ENVIRONMENT (fs, y, nf=length (fs), nr=length (y) ), "cmba")
}

mmba = function (y, fs, fit=TRUE, ...)
{	m = extend (cmba (y, fs), "mmba")
	if (fit) fit (m, ...) else m
}

amba = function (y, fs, fit=TRUE, ...)
{	m = extend (cmba (y, fs), "amba", ct=NA)
	if (fit) fit (m, ...) else m
}

contributionset = function (f)
{	if (is.contributionset (f) ) f
	else if (is.contribution (f) ) extend (list (f), "contributionset")
	else stop ("contributionset constructor not applicable")
}

is.cmba = function (f) inherits (f, "cmba")
is.mmba = function (f) inherits (f, "mmba")
is.amba = function (f) inherits (f, "amba")
is.contributionset = function (f) inherits (f, "contributionset")

fit.mmba = function (m, ...)
{	for (f in m$fs) fit (f, m$y)
	invisible (m)
}

amba.evaluate = function (m, us, ...)
{	v = 0
	for (i in 1:m$nf) v = v + m$fs [[i]] (us [[i]])
	v
}

print.cmba = function (m, ...) print (m$fs)
print.contributionset = function (m, ...) for (f in m) print (f)

summary.mmba = function (m, which=NA, ...)
{	if (is.na (which) )
	{	s = extend (list (), "summary.mmba")
		for (i in 1:m$nf) s [[i]] = summary (m, i)
		s
	}
	else summary (m$fs [[which]], m$y)
}

summary.amba = function (m, which=NA, ...)
{	if (is.na (which) )
	{	s = extend (list (), "summary.amba", "summary.cmba")
		for (i in 1:m$nf) s [[i]] = summary (m, i)
		s
	}
	else summary (m$fs [[which]], residuals (m, which) )
}

print.summary.cmba = function (s, ...) for (obj in s) print (obj)

fitted.amba = function (m, ...)
{	us = list ()
	for (i in 1:m$nf) us [[i]] = m$fs [[i]]$x
	amba.evaluate (m, us)
}

"+.contribution" = function (a, b)
{	a = contributionset (a)
	b = contributionset (b)
	n1 = length (a)
	for (i in 1:length (b) ) a [[n1 + i]] = b [[i]]
	a
}


