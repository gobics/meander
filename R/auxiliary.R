# ************************************************************************************************** 
#   AUXILIARY FUNCTIONS
# ************************************************************************************************** 

bindStrings <- function(...)
{
    paste(c(...), collapse = '')
}

get.RdsFileDescriptor <- function(file.name)
{
    file.path(RDS_PATH, paste(c(file.name, RDS_FILE_EXTENSION), collapse = EXTENSION_SEPERATOR))
}

set.RunningMode <- function(mode)
{
    assign('RUNNING_MODE', mode, pos = globalenv())
}
