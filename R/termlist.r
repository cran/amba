termlist = function () extend (list (), "termlist")
is.termlist = function (ts) inherits (ts, "termlist")

as.termlist = function (obj, validate=TRUE)
{	if (inherits (obj, "termlist") ) obj
	else if (is.term (obj) )
	{	if (validate) +obj
		else
		{	ts = termlist ()
			ts [[1]] = obj
			ts
		}
	}
	else stop ("as.termlist not applicable")
}

print.termlist = function (ts, ...) for (t in ts) print (t)

"+.term" = function (t1, t2)
{	if (is.termlist (t1) ) .addcon (t1, t2)
	else
	{	ts = termlist ()
		if (missing (t2) ) .addcon (ts, t1)
		else
		{	.amba.checkops (t1, t2)
			.addcon (.addcon (ts, t1), t2)
		}
	}
}

"-.term" = function (t1, t2)
{	if (is.termlist (t1) ) .adddiv (t1, t2)
	else
	{	ts = termlist ()
		if (missing (t2) ) .adddiv (ts, t1)
		else
		{	.amba.checkops (t1, t2)
			.adddiv (.addcon (ts, t1), t2)
		}
	}
}

.addcon = function (ts, t)
{	t$convergent = TRUE
	ts [[length (ts) + 1]] = t
	ts
}

.adddiv = function (ts, t)
{	t$convergent = FALSE
	ts [[length (ts) + 1]] = t
	ts
}

.amba.checkops = function (t1, t2)
	if (!is.term (t1) || !is.term (t2) ) stop ("term addition not applicable") 




