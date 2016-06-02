# ************************************************************************************************** 
#   AUXILIARY FUNCTIONS
# ************************************************************************************************** 

bindStrings <- function(...)
{
    paste(c(...), collapse = '')
}

arrange.Text =
function( string, maxCol = 80, lead = '' )
{
    target = list()
    targetIdx = 1

    stack = strsplit( string, '[[:space:]]+' )[[ 1 ]]

    while ( length( stack ) > 0 )
    {
        stackIdx = ( cumsum( nchar( stack ) + 1 ) + nchar( lead ) + 1 ) <= ( maxCol + 1 )

        if ( ! any( stackIdx ) )
            stackIdx[ 1 ] = TRUE

        target[[ targetIdx ]] = paste( c( lead, stack[ stackIdx ] ), collapse = ' ' )

        targetIdx = targetIdx + 1

        stack = stack[ !stackIdx ] 
    }

    unlist( target )
}

get.RdsFileDescriptor <- function(file.name)
{
    file.path(RDS_PATH, paste(c(file.name, RDS_FILE_EXTENSION), collapse = EXTENSION_SEPERATOR))
}

override.TclVarBlock =
function( window, variable, value, exitOnClose = FALSE )
{
    tkwm.protocol( 
        window, 
        'WM_DELETE_WINDOW',
        function()
        {
            tcl( 'set', variable, value )
            if ( exitOnClose )
                tkdestroy( window )
        }
        )
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

load.ConfigFile = 
function()
{
    if ( ! check.ConfigFile() )
        tryCatch(
            create.ConfigFile(),
            error= function(e) 
                {
                FILE_WRITE_ERROR$new( file = CONFIG_FILE, isCritical = TRUE )$throw()               
                }
            )
    
    load.ConfigFile.Keys( read.ConfigFile.Keys() )
}

check.ConfigFile =
function()
{
    if ( ! file.exists( CONFIG_DIRECTORY ) )
        return( FALSE )

    if ( ! file.exists( CONFIG_FILE ) )
        return( FALSE )

    if ( ! isValid.ConfigFile() )
        FATAL_ERROR$new(
            description = 'Corrupted configuration file', 
            message = sprintf('Configuration file already exists and seems to be corrupted. Please remove %s and try to load MeandeR again.', CONFIG_FILE)
            )$throw()
    
    TRUE
}

isValid.ConfigFile =
function()
{
    lines = readLines( CONFIG_FILE )

    (
    ( length(lines) > 2 )
    &&
    ( gsub( CONFIG_FILE_COMMENT, '', lines[ 1 ] ) == CONFIG_FILE_HASH ) 
    &&
    ( gsub( CONFIG_FILE_COMMENT, '', lines[ 2 ] ) == MEANDER_VERSION )
    )
}

create.ConfigFile =
function()
{
    dir.create(
        CONFIG_DIRECTORY,
        showWarnings = FALSE,
        )

    file.copy(
        CONFIG_TEMP_FILE,
        CONFIG_FILE,
        overwrite = TRUE
        )
}

edit.ConfigFile.Keys = 
function( ... )
{
    keys = names( c( ... ) )

    pair = read.ConfigFile.Keys()

    keepIdx = which( ! pair[ , 1 ] %in% keys )

    keys = c( pair[ keepIdx, 1], keys )
    values = c( pair[ keepIdx, 2], ... )

    headerLines = c(
        bindStrings( CONFIG_FILE_COMMENT, CONFIG_FILE_HASH ),
        bindStrings( CONFIG_FILE_COMMENT, MEANDER_VERSION )
    )
    contentLines = sprintf( CONFIG_FILE_ENTRY_FORMATTER, keys, values )

    configFile = file( CONFIG_FILE, open = 'wt' )
    tryCatch(
        writeLines( c(headerLines, contentLines), configFile ),
        error = function(e)
        {
            FILE_WRITE_ERROR$new( file = CONFIG_FILE )$throw()
        },
        finally = close.connection( configFile )
        )

    load.ConfigFile.Keys( cbind( keys, values ) )

    invisible()
}

read.ConfigFile.Keys = 
function()
{
    lines = readLines(CONFIG_FILE)
    
    # REMOVE COMMENTARY LINES
    lines = grep(bindStrings('^[^', CONFIG_FILE_COMMENT, ']'), lines, value = TRUE)

    varKeys = gsub(CONFIG_FILE_ENTRY_PATTERN, '\\1', lines)
    varValues = gsub(CONFIG_FILE_ENTRY_PATTERN, '\\2', lines)

    cbind( varKeys, varValues )
}

load.ConfigFile.Keys = 
function( Pairs )
{
    apply(
        Pairs,
        1,
        function( pair )
        {
            assign( 
                pair[ 1 ], 

                if ( grepl( '^[[:digit:]]+\\.{0,1}[[:digit:]]*$', pair[ 2 ] ) )
                    as.numeric( pair[ 2 ] )
                else
                    if ( pair[ 2 ] == 'NULL' )
                        NULL
                    else
                        pair[ 2 ],

                envir = ._CONFIG 
                )
        })
}

