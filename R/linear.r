linear = function (x, y, delegates=c ("1", "x"), name)
{	if (missing (name) ) name = deparse (substitute (x) )
	f = .extendf (contribution (x, name), "linear", FUNCTION (.linear.evaluate) )
	f$np = length (delegates)
	f$labels = delegates
	f$delegates = .linear.delegates (delegates)
	f$z = .linear.matrix (f)
	if (!all (is.finite (f$z) ) ) stop ("invalid delegates")
	f$e = rep (0, f$np)
	.tryfitc (f, y)
}

#need z?
categorical = function (x, y, name)
{	if (missing (name) ) name = deparse (substitute (x) )
	if (!is.factor ("factor") ) x = factor (x)
	k = levels (x)
	delegates = paste ("as.integer (x=='", k, "')", sep="")
	f = extend (linear (x,, delegates, name), "categorical")
	f$labels = k
	.tryfitc (f, y)
}

polynomial = function (x, y, degree=1, name)
{	if (missing (name) ) name = deparse (substitute (x) )
	delegates = c ("1", "x")
	if (degree > 1) delegates = c (delegates, paste ("x^", 2:degree, sep="") )
	if (degree == 0) delegates = "rep (1, length (x) )"
	f = extend (linear (x,, delegates, name), "polynomial")
	if (degree == 0) f$labels = "1"
	.tryfitc (f, y)
}

is.linear = function (f) inherits (f, "linear")
is.categorical = function (f) inherits (f, "categorical")
is.polynomial = function (f) inherits (f, "polynomial")

seq.categorical = function (f, ...) 1:f$np

fitraw.linear = function (f, y, ...)
	as.vector ( (lm.fit (f$z, y)$coefficients) )

fitraw.categorical = function (f, y, ...)
{	p = numeric (f$np)
	for (i in 1:f$np) p [i] = mean (y [f$z [,i] == 1])
	p
}

.linear.evaluate = function (u)
{	v = 0
	for (i in 1:.$np) v = v + .$e [i] * .$delegates [[i]] (u)
	v
}

summaryraw.linear = function (f, ...)
	e = data.frame (parameter=f$labels, estimate=f$e)

fitted.linear = function (f, ...)
	if (f$clean) f$z %*% f$e else f (f$x)

fitted.categorical = function (f, ...) f (f$x)

.linear.delegates = function (delegates)
{	fs = list ()
	for (i in 1:length (delegates) )
	{	fs [[i]] = function (x) NULL
		body (fs [[i]]) = parse (text=delegates [i])
	}
	fs
}

.linear.matrix = function (f)
{	u = .cleanv (f, f$x)
	z = matrix (nr=f$nv, nc=0)
	for (d in f$delegates) z = cbind (z, d (u) )
	z
}

.extendf = function (f, sc, g)
{	f = extend (f, sc)
	attributes (g) = attributes (f)
	g
}


