# THIS FILE CONTAINS STATIC VARIABLES MEANING THEY ARE AVAILABLE THROUGH THE
# HOLE APPLICATION. THEY ARE ALSO INITIALIZED HERE AND FURTHERMORE THIS IS
# THE PLACE WHERE THE _.onLoad()_ FUNCTION RESIDES.

._ERROR     = new.env()
._CONFIG    = new.env()


.onLoad =
function( libName, pkgName )
{
    # init error environment
    rm( list = ls( ._ERROR ), envir = ._ERROR )
    ._ERROR$stack           = list()
    ._ERROR$stack.Pointer   = 0

    # init config environment
    rm( list = ls( ._CONFIG ), envir = ._CONFIG )
    attemptExecution( create.ConfigFile() )
    load.ConfigFile.AllKeys()

}
