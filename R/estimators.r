Estimation = function ()
{	estimator = function (y) stop ("estimator undefined")
	inferencer = function (re, y, np) NULL
	mclass (abstract=TRUE)
}

is.estimation = function (obj) inherits (obj, "Estimation")

#inefficient, in general only one explanatory involved
#todo: replace general case estimator with special case estimators
OrdinaryLeastSquares = function (...)
{	
	OrdinaryLeastSquares = function (t)
	{	.$labs = t$e$labs
		.$zptr = t$d$zptr
		.$nr = t$nv
		#treat all linear terms the same?
		.$cat = is.categorical (t)
		.$valid.intercept = t$d$valid.intercept
	}

	estimator = function (y)
		as.vector ( (lm.fit (.$zptr (), y)$coefficients) )
	
	inferencer = function (re, y, np)
	{	df = .$nr - np
		r = y - .$zptr () %*% re
		if (df < 1) stop ("term has df < 1")
		resvar = sum (r^2) / df
		sol = solve (transpose (.$zptr () ) %*% .$zptr () )
		e = list ()
		e$labs = .$labs
		e$th = re
		e$se = sqrt (diag (resvar * sol) )
		e$tv = re / e$se
		e$pv = 2 * (1 - pt (abs (e$tv), df) )
		e$stars = pstars (e$pv)
		e = data.frame (e, stringsAsFactors=FALSE)
		if (!.$valid.intercept)
			if (.$cat) e$th = e$th - e$th [1]
			else
			{	e = e [-1,]
				row.names (e) = 1:nrow (e)
			}
		e
	}

	mclass (Estimation)
}

#this might not work if we have missing values
#plus incorrect inference
WeightedLeastSquares = function (...)
{
	OrdinaryLeastSquares = function (t, wptr)
	{	super (t)
		.$wptr = wptr
	}

	estimator = function (y)
		as.vector ( (lm.wfit (.$zptr (), y, .$wptr () )$coefficients) )

	mclass (OrdinaryLeastSquares)
}

#poor weighting, very inefficient, no inference
LocalPolynomialSmoothing = function (...)
{
	LocalPolynomialSmoothing = function (t)
	{	.$ns = t$d$ns
		.$sx = t$d$sx
		.$deg = t$d$deg
		.$nfits = t$d$nfits
		.$wf = t$d$wf
		.$smoothness = t$d$smoothness
		.$z = clean.explanatory (t)
	}

	estimator = function (y)
	{	sy = smooth.series (.$z, y, .$ns, .$sx, .$deg, .$wf, .$smoothness)
		for (i in iter (.$nfits, 2) )
			sy = smooth.series (.$sx, sy, .$ns, .$sx, .$deg, .$wf, .$smoothness)
		#r = y - splinefun (.$sx, sy) (.$z)
		#sy - mean (r)
		sy
	}

	mclass (Estimation)
}

smooth.series = function (x, y, ns, sx, deg, wf, smoothness)
{	z = NULL
	for (i in 0:deg) z = cbind (z, x ^ i)
	sy = numeric (ns)
	hs = smoothness / 2
	yb = mean (y)
	for (i in 1:ns)
	{	u = sx [i]
		v = x >= u - hs & x <= u + hs
		sy [i] = if (sum (v) > deg)
		{	xsub = x [v]
			ysub = y [v]
			zsub = z [v,]
			w = wf (u, smoothness, xsub)
			sum (lm.wfit (zsub, ysub, w)$coefficients * u ^ (0:deg) )
		}
		else yb
	}
	sy
}

stdwf = function (m, w, x)
{	if (m != 0) x = x - m
	z = 2 * x / w
	1 - z * z
}


