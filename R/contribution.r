fit = function (...) UseMethod ("fit")
fitraw = function (f, y, ...) UseMethod ("fitraw")
summaryraw = function (f, ...) UseMethod ("summaryraw")
#masks
pairs = function (...) graphics::pairs (...)
fitted = function (...) stats::fitted (...)
residuals = function (...) stats::residuals (...)

contribution = function (x, name="x")
{	f = extend (FUNCTION (function (u) NULL), "contribution", x, name)
	valid = .is.valid (x)
	f$clean = all (valid)
	f$nr = length (x)
	f$nv = sum (valid)
	f$valid = if (f$clean) TRUE else valid
	f
}

is.contribution = function (f) inherits (f, "contribution")

seq.contribution = function (f, n=80, ...)
{	k = range (f$x, na.rm=TRUE)
	seq (k [1], k [2], length=n)
}

print.contribution = function (f, ...) cat (format (f), "\n")
format.contribution = function (f, ...) paste (class (f) [1], " (", f$name, ")", sep="")

summary.contribution = function (f, y, ...)
{	s = structure (list (), class="summary.contribution")
	s$label = format (f)
	s$estimate = summaryraw (f)
	s$pcd = .pcd (f, y)
	s
}

print.summary.contribution = function (s, ...)
{	cat (s$label, "\n")
	print (s$estimate)
	cat ("pcd:", s$pcd, "\n")
}

fit.contribution = function (f, y, raw=FALSE, ...)
{	if (!f$clean) y = y [f$valid]
	e = fitraw (f, y)
	if (raw) e
	else
	{	f$e = e
		invisible (f)
	}
}

fitted.contribution = function (f, ...) f (f$x)
residuals.contribution = function (f, y, ...) y - fitted (f)

.is.valid = function (x)
{	if (is.character (x) || is.factor (x) ) !is.na (x)
	else is.finite (x)
}

.cleanf = function (f, g, ...) .cleanv (f, g (f, ...) )
.cleanv = function (f, x, ...) if (f$clean) x else x [f$valid]

.tryfitc = function (f, y) if (missing (y) ) f else fit (f, y)

.pcd = function (f, y)
{	yh = fitted (f)
	sum ( (yh - mean (yh) ) ^ 2) / sum ( (y - mean (y) )^2)
}


