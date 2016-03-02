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

# ************************************************************************************************** 
#   BIT FLAG OPERATIONS
# ************************************************************************************************** 

set.Flag = function (byte, ...)
{
    flag = Reduce(bitwOr, ...)

    bitwOr(byte, flag)
}

unset.Flag = function (byte, ...)
{
    flag = Reduce(bitwOr, ...)

    bitwAnd(byte, bitwNot(flag))
}

toggle.Flag = function (byte, ...)
{
    flag = Reduce(bitwOr, ...)
    
    bitwXor(byte, flag)
}

has.Flag = function(byte, ...)
{
    flag = Reduce(bitwOr, ...)

    bitwAnd(byte, flag) == flag
}

# ************************************************************************************************** 
#   CONFIG FILE OPERATIONS
# ************************************************************************************************** 

check.ConfigFile = function ()
{
#    if (file.exists(CONFIG_FILE))
#
#   else
        create.ConfigFile()
}

create.ConfigFile = function ()
{


}

write.ConfigFile.Key = function(key, value)
{
    
}

load.ConfigFile.AllKeys = function()
{
    lines = readLines(CONFIG_FILE)
    
    # REMOVE COMMENTARY LINES
    lines = grep(bindStrings('^[^', CONFIG_FILE_COMMENT, ']'), lines, value = TRUE)   

    varNames = gsub(CONFIG_FILE_ENTRY_PATTERN, '\\1', lines)
    varValues = gsub(CONFIG_FILE_ENTRY_PATTERN, '\\2', lines)
}
