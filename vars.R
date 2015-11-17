a = 5
bb <- function()
{
	a = 3

    update.Variables(environment(), parent.frame())
}

copyVariablesToScope <- function(from, target)
{
    vars = ls(all.names = T, envir = from)
	
    lapply(
        vars,
        function(var)
        {
		    assign(var, get(var, pos = from), pos = )
        })
}
