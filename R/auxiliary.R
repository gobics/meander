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

create.ConfigFile = function ()
{
    if (file.exists(CONFIG_FILE))
    {
        if (gsub(CONFIG_FILE_COMMENT, '', readLines(CONFIG_FILE)[1]) != CONFIG_FILE_HASH)
            FATAL_ERROR$new(description = 'Corrupted configuration file', message = sprintf('Configuration file already exists and seems to be corrupted. Please remove %s if it is not needed by you or another program.', CONFIG_FILE))$throw()
    }
    else
    {
        if (!file.exists(CONFIG_DIRECTORY))
            dir.create(CONFIG_DIRECTORY)

        file.copy(CONFIG_TEMP_FILE, CONFIG_FILE)
    }
}

appendTo.ConfigFile = function(keys, values)
{
    writeLines(sprintf(CONFIG_FILE_ENTRY_FORMATTER, keys, values), CONFIG_FILE)
}

load.ConfigFile.AllKeys = function()
{
    lines = readLines(CONFIG_FILE)
    
    # REMOVE COMMENTARY LINES
    lines = grep(bindStrings('^[^', CONFIG_FILE_COMMENT, ']'), lines, value = TRUE)

    varNames = gsub(CONFIG_FILE_ENTRY_PATTERN, '\\1', lines)
    varValues = gsub(CONFIG_FILE_ENTRY_PATTERN, '\\2', lines)

    for (i in 1:length(varNames))
    {
        assign(varNames[i], varValues[i], envir = .GlobalEnv)
    }
}
