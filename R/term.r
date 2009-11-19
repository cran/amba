term = function (d, e, s, x, E=NULL, I=NULL, validate=TRUE,
	clean=TRUE, nr=length (x), nc=1, nv=nr, valid=NULL)
{	t = extend (compenv (d, e, s, x, E, I, clean, nr, nc, nv, convergent=TRUE, w=1),
		c ("term", "termenv") )
	if (validate) term.validate (t)
	t
}

is.term = function (t) inherits (t, "term")

term.label = function (t)
{	str = paste (class (t) [1], t$s, sep = ":")
	if (!t$convergent) str = paste ("(-", str, ")", sep="")
	str 
}

term.validate = function (t)
{	x = t$x
	valid = NULL
	if (inherits (x, "matrix") || inherits (x, "data.frame") )
	{	t$nr = nrow (x)
		t$nc = ncol (x)
		valid = logical (t$nr)
		for (i in 1:t$nr)
		{	k = is.finite (as.numeric (x [i,]) )
			valid [i] = all (k)
		}
	}
	else if (is.vector (x) || is.factor (x) ) valid = is.finite (x)
	else stop ("term.validate can not be applied")
	clean = all (valid)
	t$nv = sum (valid)
	if (! clean)
	{	t$clean = FALSE
		t$valid = valid
	}
	if (t$nv < 2) stop ("terms must have at least 2 valid realisations")
}

validex = function (t) if (t$clean) rep (TRUE, t$nr) else t$valid

term.pump = function (t, y, sc=NULL)
{	t = extend (t, sc)
	if (!is.null (y) )
	{	estimation.validate (t)
		fit (t, y, TRUE)
	}
	t
}

estimation.validate = function (t)	if (is.null (t$E) || is.null (t$I) ) standardestimation (t)

setestimation = function (ts, ec, ...)
{	if (ifst (ec) )
	{	ts = as.termlist (ts, FALSE)
		for (t in ts)
		{	obj = if (is.estimation (ec) ) ec else ec (t, ...) 
			t$E = hypermethod (obj, "estimator")
			t$I = hypermethod (obj, "inferencer")
		}
	}
}

eqnp = function (...) UseMethod ("eqnp")
eqnp.default = function (...) Inf

interceptadj = function (...) UseMethod ("interceptadj")
interceptadj.default = function (...) 0

standardestimation = function (...) UseMethod ("standardestimation")
standardestimation.default = function (...) stop ("no estimation object specified") 

rawestimate = function (...) UseMethod ("rawestimate")
rawestimate.default = function (...) stop ("rawestimate not implemented")

termenv = function () extend (compenv (), "termenv")
term.design = function () extend (list (), "term.design")
term.estimate = function () extend (list (), "term.estimate")

fit.term = function (t, y, pump=TRUE, ...)
{	estimation.validate (t)
	raw.estimate = t$E (clean.response (t, y) )
	if (pump)
	{	mutate (t, raw.estimate)
		invisible (t)
	}
	else raw.estimate
}

print.term = function (t, ...) cat (term.label (t), "\n")

summary.term = function (t, y, np=eqnp (t), ...)
{	s = structure (list (), class="summary.term")
	s$label = term.label (t)
	s$inference = t$I (rawestimate (t), clean.response (t, y), np) 
	s$con = t$convergent
	s$pcd = term.pcd (t, y)
	s$nr = t$nr
	s$nv = t$nv
	s$eqnp = eqnp (t)
	s
}

print.summary.term = function (s, ...)
{	cat (s$label, "\n")
	if (ifst (s$ext) ) print (s$ext)
	if (ifst (s$inference) ) print (s$inference)
	nr = paste (s$nr, "(", s$nv, ")", sep="")
	std = data.frame (conditioning=s$con, x=nr, eqnp=s$eqnp, pcd=s$pcd)
	names (std) [2] = "nr(nv)"
	print (std)
}

fitted.term = function (t, ...) evaluate (t, t$x)
residuals.term = function (t, y, ...) y - fitted (t)

#all these functions with respect to x
clean.explanatory = function (t)
	if (t$clean) t$x else if (t$nc > 1) t$x [t$valid,] else t$x [t$valid]
clean.response = function (t, y) if (t$clean) y else y [t$valid]

clean.fitted = function (t)
{	yh = fitted (t)
	if (!t$clean) yh = yh [t$valid]
	yh
}

clean.residuals = function (t, y)
{	r = residuals (t, y)
	if (!t$clean) r = r [t$valid]
	r
}

seq.term = function (t, n=200, ...)
{	if (t$nc > 1) stop ("seq.term reqiures nc = 1")
	if (inherits (t$x, "factor") ) 1:length (levels (t$x) )
	else
	{	x = range (t$x, na.rm=TRUE)
		seq (x [1], x [2], length=n)
	}
}

term.pcd = function (t, y) gf.term (t, y)$rsq

gf.term = function (t, y, method="rsq", ...)
	simple.gf (clean.fitted (t), clean.response (t, y), method=method)

simple.gf = function (yh, y, r=y - yh, method=c ("rsq", "mar") )
{	gf = list ()
	if (any (method == "mar") ) gf$mar = mean (abs (r) )
	if (any (method == "msr") ) gf$msr = mean (r^2)
	if (any (method == "ssr") ) gf$ssr = sum (r^2)
	if (any (method == "rsq") )
		gf$rsq = sum ( (yh - mean (yh) ) ^ 2) / sum ( (y - mean (y) )^2)
	gf
}

pstars = function (ps)
{	v = character ()
	for (p in ps)
	{	if (!is.finite (p) ) v = c (v, "!!!!")
		else if (p > 0.05 && p <= 0.1) v = c (v, "   .")
		else
		{	n = 0
			if (p <= 0.000001) n = 4
			else if (p <= 0.001) n = 3
			else if (p <= 0.01) n = 2
			else if (p <= 0.05) n = 1
			str1 = paste (rep (" ", 4 - n), collapse = "")
			str2 = paste (rep ("*", n), collapse = "")
			v = c (v, paste (str1, str2, sep = "") )
		}
	}
	v
}









