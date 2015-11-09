# ************************************************************************************************** 
#   AUXILIARY FUNCTIONS
# ************************************************************************************************** 

varName.ToString <- function(var)
{
    deparse(substitute(var, parent.frame()))
}

assign.VariablesInGlobalScope <- function(var, value)
{
    assign(varName.ToString(var), value, pos = globalenv())
}

copyVariablesToScope <- function(from, target)
{
    vars = ls(all.names = T, envir = from)
    
    lapply(
        vars,
        function(var)
        {
            assign(var, get(var, pos = from), pos = target)
        })
}

bindStrings <- function(...)
{
    paste(c(...), collapse = '')
}

get.RdsFileDescriptor <- function(file.name)
{
    file.path(RDS_PATH, paste(c(file.name, RDS_FILE_EXTENSION), collapse = EXTENSION_SEPERATOR))
}

