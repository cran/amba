.First.lib = function (...)
{	assign (".oosp.cid", character (0), envir=.GlobalEnv)
	assign (".oosp.image", list (), envir=.GlobalEnv)
	assign (".oosp.constructor", NA, envir=.GlobalEnv)
	assign (".oosp.method", NA, envir=.GlobalEnv)
	.mclass.generate ("MObject")
}

