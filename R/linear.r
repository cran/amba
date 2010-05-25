linear = function (x, y=NULL, key=c ("1", "x"), valid.intercept=ifst (y), s)
{	if (missing (s) ) s = deparse (substitute (x) )
	d = linear.design (key, valid.intercept)
	e = linear.estimate (key)
	t = term (d, e, s, x)
	zptr = objref (linear.matrix (t) )
	t$d$zptr = zptr
	term.pump (t, y, "linear")
}

standardestimation.linear = function (t, ...)
{	ols = OrdinaryLeastSquares (t)
	setestimation (t, ols)
}

linear.matrix = function (t)
{	z = NULL
	for (k in t$d$ks) z = cbind (z, linear.expand (t$nr, k (t$x) ) )
	if (!t$clean) z = cbind (z [t$valid,])
	if (!all (is.finite (z) ) )
		stop ("key functions must produce valid z given valid x")
	else z
}

linear.expand = function (n, x)
{	if (length (x) == 1 && n > 1) rep (x, n)
	else (x)
}

transpose = function (x) base::t (x)

eqnp.linear = function (t, ...) t$d$eqnp
interceptadj.linear = function (t, ...) t$e$th [1]
rawestimate.linear = function (t, ...) t$e$th

categorical = function (x, y=NULL, valid.intercept=ifst (y), s)
{	if (missing (s) ) s = deparse (substitute (x) )
	if (!inherits (x, "factor") ) x = factor (x)
	kstr = NULL
	levs = levels (x)
	for (lev in levs)
		kstr = c (kstr, paste ("as.integer (x=='", lev, "')", sep="") )
	t = linear (x, NULL, kstr, valid.intercept, s)
	t$e$labs = levs
	term.pump (t, y, "categorical")
}

polynomial = function (x, y=NULL, deg=1, valid.intercept=ifst (y), s)
{	if (missing (s) ) s = deparse (substitute (x) )
	kstr = "rep(1,length(x))"
	for (i in 1:deg) kstr [i + 1] = paste ("x^", i, sep = "")
	t = linear (x, NULL, kstr, valid.intercept, s)
	t$e$labs [1] = "1"
	term.pump (t, y, "polynomial")
}

is.linear = function (t) inherits (t, "linear")
is.categorical = function (t) inherits (t, "categorical")
is.polynomial = function (t) inherits (t, "polynomial")

linear.design = function (key, valid.intercept, zptr=NA)
{	d = extend (term.design (), "linear.design")
	d$np = d$eqnp = length (key)
	#this is only correct for main effects
	if (!valid.intercept) d$eqnp = d$np - 1
	d$valid.intercept = valid.intercept
	d$key = key
	d$ks = list ()
	for (i in 1:d$np) d$ks [[i]] = mutate (function (x) NULL, key [i])
	d$zptr = zptr
	d
}

linear.estimate = function (labs, th=rep (0, length (labs) ) )
{	e = extend (term.estimate (), "linear.estimate")
	e$labs = labs
	e$th = th
	e
}

reset.linear = function (t, ...) t$e$th [] = 0
mutate.linear = function (t, re, ...) t$e$th = re

evaluate.linear = function (t, u, ...)
{	v = 0
	for (i in 1:t$d$np) v = v + t$e$th [i] * t$d$ks [[i]] (u)
	v
}

summary.linear = function (t, ...) extend (summary.term (t, ...), "summary.linear")

fitted.linear = function (t, ...)
{	if (t$clean) t$d$zptr () %*% t$e$th
	else evaluate (t, t$x)
}

print.linear.estimate = function (e, ...) print (data.frame (labs=e$labs, th=e$th) )


