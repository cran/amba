#incomplete
interaction = function (t1, t2)
{	x = data.frame (t1$x, t2$x)
	names (x) = c (t1$s, t2$s)
	pd = interaction.predesign (x)
	t = linear (x, NULL, pd$kstr, TRUE, pd$s)
	term.resist (t1, t)
	term.resist (t2, t)
	t$ncat = pd$ncat
	t$ncont = pd$ncont
	t$d$eqnp = pd$eqnp
	t$e$labs = pd$labs
	term.pump (t, NULL, "interaction")
}

term.resist = function (t, tx) t$resistors = c (t$resistors, tx$s)
term.resists = function (t, tx) (ifst (t$resistors) && any (tx$s == t$resistors) )

is.interaction = function (t) inherits (t, "interaction") 

interceptadj.interaction = function (t, ...) 0

interaction.predesign = function (x)
{	d = list ()
	if (!inherits (x, "data.frame") ) stop ("interaction x must be data.frame")
	d$s = paste (names (x), collapse = "&")
	d$ncat = d$ncont = 0
	vlevs = NULL
	catk = catf = catl = contk = contl = list ()
	for (i in 1:length (x) )
	{	v = x [[i]]
		if (inherits (v, "factor") )
		{	levs = levels (v) [table (v) > 0]
			nlevs = length (levs)
			if (nlevs == 0) stop ("interaction x contains corrupt variable")
			d$ncat = d$ncat + 1
			k1 = k2 = NULL
			for (lev in levs)
			{	k1 = c (k1, paste ("as.integer (x[,", i, "] =='", lev, "')", sep="") )
				k2 = c (k2, paste ("(x[,", i, "] =='", lev, "')", sep="") )
			}
			catk [[d$ncat]] = k1
			catf [[d$ncat]] = k2
			catl [[d$ncat]] = levs
			vlevs = c (vlevs, nlevs)	
		}
		else if (mode (v) == "numeric")
		{	d$ncont = d$ncont + 1
			contk [[d$ncont]] = paste ("x[,", i, "]", sep="")
			contl [[d$ncont]] = names (x) [i]
		}
		else stop ("unsupported variable class in interaction x")
	}
	d$kstr = interaction.combine (c (list (interaction.combine (catk, "*") ), contk), "*")
	d$labs = interaction.combine (c (list (interaction.combine (catl, ":") ), contl), ":")
	if (d$ncat == 0) d$eqnp = 1
	else
	{	#this block is not tested properly
		#it might be useful for summary output to indicate (rather than ignore) empty combinations
		filter = interaction.combine (catf, "&")
		nf = length (filter)
		gok = rep (FALSE, nf)
		f = function (x) NULL
		for (i in 1:nf)
		{	g = mutate (f, filter [i])
			n = sum (g (x), na.rm=TRUE)
			if (n > 0) gok [i] = TRUE
		}
		d$kstr = d$kstr [gok]
		d$labs = d$labs [gok]
		#not sure if this is right...
		#d$eqnp = prod (vlevs - 1) - sum (!gok)
		d$eqnp = prod (vlevs - 1)
	}
	d
}

#x is a list of vectors
interaction.combine = function (x, op)
{	n = length (x)
	if (n > 1)
	{	y = x [[1]]
		for (i in 2:n)
		{	y = outer (y, x [[i]], paste, sep=op)
			y = as.vector (t (y) )
		}
		y
	}
	else x [[1]]
}

